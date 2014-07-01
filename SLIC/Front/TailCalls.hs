-- | Tail-call optimization for the LAR back-end.
--
--   Tail-calls can use the same LAR as the calling function, by
--   using a /LAR mutator/ that uses the current LAR for the
--   function call. Tail calls are conservative: they only happen
--   when the current LAR has at least as much space as the LAR
--   needed for the call. This check makes this analysis dependent
--   on the actual LAR representation used by the back-end.
-- 
--   On a tail call, two things happen:
--   (1) The strict parameters of the new LAR are evaluated
--       using temporary space and the current LAR.
--   (2) The LAR mutator runs and transforms the current LAR.
--   (3) The values of (1) update the required slots of the LAR.
-- 
--   A tail call may need 1+N temporary space, where 1 is the
--   extra parameter space needed for swaps during permutations,
--   and N is the number of strict arguments in the call. This
--   space can be either temporary C variables/registers, or
--   space from the end of the LAR space (if it holds that
--   old_LAR_size - new_LAR_size >= N+1). We can omit the extra
--   parameter space, if we implement swapping with XOR.
--   

module SLIC.Front.TailCalls (bladesToSeqCopies, spotTCalls, test) where

import Data.Graph (SCC(..), buildG, components, stronglyConnCompR)
import Data.List (findIndex, nub)
import qualified Data.Map as M (lookup)
import Data.Set as S (fromList, member)
import Data.Tree
import SLIC.AuxFun (ierr, insCommIfMore)
import SLIC.State
import SLIC.SyntaxAux
import SLIC.SyntaxFL
import SLIC.Types

-- | Analysis that adds tail-call information to the syntax tree.
--   It finds all tail call positions, which are function calls,
--   where all parameters are one of these:
--   (a) formals of the enclosing function,
--   (b) values that do not need access to the LAR, such as a
--       closed value (integer or nullary constructor), a CAF,
--       or a function/constructor applied to closed values.
--   (c) strict parameters of the called function whose
--       evaluation returns a closed value.
-- 
spotTCalls :: Options -> ModF -> ModF
spotTCalls opts m =
  let p = modProg m
      cafs = reachableCAFs m
      stricts = reachableStricts m
      env = modTAnnot m
      arities = reachableArities m
      pmdepths = reachablePmDepths m
      tcoDefs = map tcoD (progDefs p)
      tcoD (DefF f fs e) = DefF f fs (tcoE (f, frmsToNames fs) e)
      tcoE (f0, fs) e@(FF vf@(V f) el NoCI) = 
        let indexedArgs = zip [0..] el
        in  if all (tcoCompatible fs f) indexedArgs then
              if larsCompatible f0 f then
                let ci = tcoCI cafs stricts fs f indexedArgs
                in  -- trace2 ("Found tail call: '"++(pprint e "")++"', ci="++
                    --         (pprint ci "")) $ 
                    FF vf el ci
              else
                -- trace2 ("Discarding tail call: '"++(pprint e "")++
                --         "', enclosing LAR is too small")$
                e
          else e
      tcoE _ e@(FF (BV{}) _ NoCI) = e
      tcoE _   (FF _ _ (Mut _ _)) = error "Found already set LAR mutation."
      tcoE ff  (CaseF cloc e0 qn pats) = CaseF cloc e0 qn (map (tcoPat ff) pats)
      tcoE ff  (ConF (CN CIf) [cond, e1, e2]) =
          ConF (CN CIf) [cond, tcoE ff e1, tcoE ff e2]
      tcoE _ e@(XF {}) = e
      tcoE _ e@(ConF{}) = e
      tcoE _ e@(ConstrF{}) = e
      tcoE _   (LetF{}) = ierr "The tail-call inspector cannot handle let."
      tcoE _   (LamF{}) = ierr "The tail-call inspector cannot handle lambdas."
      tcoPat ff (PatB pat e) = PatB pat (tcoE ff e)
      tcoCompatible _  f (slotIdx,  _) | isStrictGround f slotIdx = True
      tcoCompatible fs _ (_, XF (V v)) = (v `elem` fs) || (v `elem` cafs)
      tcoCompatible _  _ (_, e       ) = closedExpr cafs e
      isStrictGround f slotIdx =
        case M.lookup f stricts of
          Nothing   -> False
          Just strs ->
            let isGround =
                  case M.lookup f env of
                    Nothing -> ierr $ "isGround: cannot find type for "++(qName f)
                    Just (fT, _) ->
                      case (types fT) !! slotIdx of
                        Tg gt -> isNullaryGT gt
                        _     -> False
            in  (S.member slotIdx strs) && isGround
      -- If f is called inside f0, check that the LAR of f is smaller.
      larsCompatible f0 f =
        let (Just a0, n0) = (M.lookup f0 arities, findPMDepth f0 pmdepths)
            (Just a , n ) = (M.lookup f  arities, findPMDepth f  pmdepths)
        in  case (optGC opts, optCompact opts) of 
              (SemiGC, True) -> a0+n0 >= a+n
              (LibGC , _   ) ->
                let suspSize = if optTag opts then 3 else 2
                in  (a0*suspSize)+n0 >= (a*suspSize)+n
              _              -> False
  in  m{modProg=(p{progDefs=tcoDefs})}

-- | Generate the call information for a tail call.
tcoCI :: [QName] -> Stricts -> [QName] -> QName -> [(Int, ExprF)] -> CI
tcoCI cafs stricts fs f el =
  let closed = S.fromList $ map fst $
               filter (\(_, e)->closedExpr cafs e) el
      strs = fromJustSet (M.lookup f stricts)
      isFrm (_, XF (V v)) = v `elem` fs
      isFrm _ = False
      frmsUsed = map (\(i, XF (V v))->(i, v)) $ filter isFrm el
      genFrmMapping [] = []
      genFrmMapping ((tIdx, v):ifs) =
        case findIndex (\fv->fv==v) fs of
          Just oIdx -> (tIdx, oIdx):(genFrmMapping ifs)
          Nothing   -> ierr "genFrmMapping: no index found for formal"
      (axles, copies) = compileM $ genFrmMapping frmsUsed
  in  Mut ((axles, copies), closed, strs) Nothing

-- | Takes a list of visible CAFs and an expression. Returns 'True' if
--   the expression is closed; i.e. it is a constant expression, 
--   with the only variables permitted being CAFs.
closedExpr :: [QName] -> ExprF -> Bool
closedExpr _    (ConF _ [])    = True
closedExpr cafs (ConF _ el)    = all (closedExpr cafs) el
closedExpr _    (FF _ [] _)    = True
closedExpr cafs (FF _ el _)    = all (closedExpr cafs) el
closedExpr _    (ConstrF _ []) = True
closedExpr cafs (ConstrF _ el) = all (closedExpr cafs) el
closedExpr cafs (XF (V v))     = v `elem` cafs
closedExpr _    (XF (BV{}))    = False
closedExpr _    (CaseF{})      = False
closedExpr _    (LetF{})       = False
closedExpr _    (LamF{})       = False

-- | A LAR mutation is a parallel assignment from original slot indices, to
--   target slot indices, e.g. {2 -> 1, 1 -> 2, 1 -> 3}. We use the term
--   /copy/ to refer to a /move/ in parallel assignment.
type MutationL = [Copy]

-- | A copy from a slot index to another slot index.
type Copy = (SlotIdx, SlotIdx)

-- | Pretty printer for copies.
pprCopy :: Copy -> ShowS
pprCopy (src, dest) = shows src.("->"++).shows dest

-- | The graphs used internally contain no information, just unit.
type SCC' = SCC ((), SlotIdx, [SlotIdx])

-- | Compiles a list of slot-to-slot moves to a windmill (list of axles
--   and list of blades). This is required to handle the re-use
--   of formals from the current LAR. The intuition is that some moves
--   may depend on each other, while others not. For example, assume
--   the mutation [(3->1),(1->2),(5->3),(2->5),(8->7),(9->8),(7->9),(3->4)].
--   This can be done with two permutations [(3->1),(1->2),(5->3),(2->5)]
--   and [(8->7),(9->8),(7->9)], and one copy (3->4).
compileM :: MutationL -> Windmill
compileM mL =
  let listToGraph l = map (\(a, b)->((), b, [a])) l
      windmillG  = stronglyConnCompR $ listToGraph mL
      compileSCCs :: [SCC'] -> [Axle] -> [Copy] -> ([Axle], [Blade])
      compileSCCs [] axles copies = (axles, copiesToBlades copies)
      compileSCCs (scc:sccs) axles copies =
        case scc of
          -- nodes not in cycles are kept as extra 'copies'
          AcyclicSCC ((), tId, [oId]) ->
            let copy = (oId, tId)
            in  compileSCCs sccs axles (copy:copies)
          -- single-node cycles are ignored
          CyclicSCC [_] -> compileSCCs sccs axles copies
          -- cycle (axle) found
          CyclicSCC vvs ->
            let nodeID ((), nID, _) = nID
                axle = listToAxle $ map nodeID vvs
            in  compileSCCs sccs (axle:axles) copies
          _ -> error "compileSCCs: found strange SCC"
      copiesToBlades :: [Copy] -> [Blade]
      copiesToBlades [] = []
      copiesToBlades copies =
        let bounds = (0, (\(as, bs)->maximum (as++bs)+1) (unzip copies))
            bladesG = buildG bounds (copies)
            notSingleNode (Node _ cs) = cs/=[]
        in  filter notSingleNode $ components bladesG
  in  compileSCCs windmillG [] []

-- | Compiles a list of blades to an ordered list of copies. Does a post-order
--   traversal to order the copies. If the root is part of an axle, it is
--   replaced by the node which is assigned to it by the axle permutation.
bladesToSeqCopies :: Windmill -> [Copy]
bladesToSeqCopies (axles, blades) =
  let bladeToSeqCopies parent (Node i ns) =
         (concatMap (bladeToSeqCopies i) ns) ++ [(parent, i)]
      bladeRootToSeqCopies (Node i ns) =
         -- if the root is in a cycle, get its index after the permutation
         let root = findRootCopy axles i
         in  concatMap (bladeToSeqCopies root) ns
      axleToCopies axle =
        let axs@(a:as) = axleToList axle
            axsShift1  = as++[a]
        in  zip axs axsShift1
      findRootCopy []     i = i
      findRootCopy (a:as) i =
         case lookup i (axleToCopies a) of
           Just j  -> j
           Nothing -> findRootCopy as i
  in  concatMap bladeRootToSeqCopies blades

-- | Test routine.
test :: IO ()
test = do putStrLn "Mutation tests:"
          testMut [(2, 1), (1, 2), (1, 3), (1, 4)]
          testMut [(2, 1), (1, 5), (1, 6), (2, 2), (4, 3)]
          testMut [(1, 0), (2, 1), (0, 2)]
          testMut [(3, 1), (1, 2), (5, 3), (2, 5), (8, 7), (9, 8), (7, 9), (3, 4)]
          testMut [(3, 1), (1, 2), (2, 3), (2, 4), (4, 5), (4, 6), (3, 7), (7, 8)]

-- | Test a single LAR mutation involving only parallel assignment.
testMut :: MutationL -> IO ()
testMut ml =
  let checkMutation ms = length ms == length (nub (map snd ms))
      pprCopyL copies = insCommIfMore (map pprCopy copies)
  in  do putStrLn $ "[*] Original: "++(show ml)
         if checkMutation ml then
           do let windmill = compileM ml
              putStrLn $ "Result:"
              putStrLn $ pprWindmill windmill ""
              putStrLn $ "Compiled blades:"
              putStrLn $ pprCopyL (bladesToSeqCopies windmill) ""
         else
             putStrLn "The mutation is malformed."
