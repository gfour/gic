-- | Tail-call optimization for the LAR back-end.
--
--   Tail-calls can use the same LAR as the calling function, by
--   using a /LAR mutator/ that uses the current LAR for the
--   function call.
-- 
--   Another analysis then finds all pairs (f, g), where g is a
--   tail-call in f and uses them to find tail-call chains.
--   E.g. [(f, g), (g, h)] is a chain of possible calls, where f
--   calls g in tail position and g calls h in tail position.
--   This means that f, g, and h can use the same space for their
--   LARs, if maxSpace=max(LAR_f_size, LAR_g_size, LAR_h_size).
--   For this tail-call chain, the call to f will allocate space
--   of size maxSpace; then g and h will re-use it through a
--   LAR mutator. A mutator runs on a tail-call and initializes
--   the LAR of the next function.
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
--   old_LAR_size - new_LAR_size >= N+1).
--   

module SLIC.Front.TailCalls (spotTCalls, test) where

import Data.List (findIndex)
import qualified Data.Map as M (fromList, lookup)
import Data.Set as S (fromList, member)
import Data.Graph (SCC(..), stronglyConnCompR)
import Data.List (nub, sort)
import SLIC.AuxFun (ierr, trace2)
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
spotTCalls :: ModF -> ModF
spotTCalls m =
  let p = modProg m
      cafs = reachableCAFs m
      stricts = reachableStricts m
      env = modTAnnot m
      tcoDefs = map tcoD (progDefs p)
      tcoD (DefF f fs e) = DefF f fs (tcoE (frmsToNames fs) e)
      tcoE fs e@(FF vf@(V f) el NoCI) = 
          let indexedArgs = zip [0..] el
          in  if all (tcoCompatible fs f) indexedArgs then
                  let ci = tcoCI cafs stricts fs f indexedArgs
                  in  trace2 ("Found tail call: '"++(pprint e "")++"', ci="++
                              (pprint ci "")) $ 
                      FF vf el ci
          else e
      tcoE _ e@(FF (BV{}) _ NoCI) = e
      tcoE _   (FF _ _ (Mut _)) = error "Found already set LAR mutation."
      tcoE fs  (CaseF cloc e0 qn pats) = CaseF cloc e0 qn (map (tcoPat fs) pats)
      tcoE fs  (ConF (CN CIf) [cond, e1, e2]) =
          ConF (CN CIf) [cond, tcoE fs e1, tcoE fs e2]
      tcoE _ e@(XF {}) = e
      tcoE _ e@(ConF{}) = e
      tcoE _ e@(ConstrF{}) = e
      tcoE _   (LetF{}) = ierr "The tail-call inspector cannot handle let."
      tcoE _   (LamF{}) = ierr "The tail-call inspector cannot handle lambdas."
      tcoPat fs (PatB pat e) = PatB pat (tcoE fs e)
      tcoCompatible _  f (slotIdx, _) | isStrictGround f slotIdx = True
      tcoCompatible fs _ (_, XF (V v)) =
          (v `elem` fs) || (v `elem` cafs)
      tcoCompatible _  _ (_, e) = closedExpr cafs e
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
      (perms, copies) = compileM $ genFrmMapping frmsUsed
  in  Mut (perms, copies, closed, strs)

-- | Takes a list of visible CAFs and an expression. Returns True if
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

-- | The graphs used internally contain no information, just unit.
type SCC' = SCC ((), SlotIdx, [SlotIdx])

-- | Compiles a list of slot-to-slot moves to a list of permutations
--   and a list of slot copies. This is required to handle the re-use
--   of formals from the current LAR. The intuition is that some moves
--   may depend on each other, while others not. For example, assume
--   the mutation [(1<-3),(2<-1),(3<-5),(5<-2),(7<-8),(8<-9),(9<-7),(4<-3)].
--   This can be done with two permutations [(1<-3),(2<-1),(3<-5),(5<-2)]
--   and [(7<-8),(8<-9),(9<-7)], and one copy (4<-3).
compileM :: MutationL -> ([Permutation], [Copy])
compileM mL =
  let toGraph ml = stronglyConnCompR $ map (\(a, b)->((), a, [b])) ml
      compileSCCs :: [SCC'] -> [Permutation] -> [Copy] -> ([Permutation], [Copy])
      compileSCCs [] perms copies = (perms, sort copies)
      compileSCCs (scc:sccs) perms copies =
        case scc of
          AcyclicSCC ((), tId, [oId]) ->
            let copy = Copy oId tId
            in  compileSCCs sccs perms (copy:copies)
          CyclicSCC vvs@(v:vs) ->
            let vvsShift1 = vs++[v]
                nodeID ((), nID, _) = nID
                permL = map (\(a, b)->(nodeID a, nodeID b)) $
                        zip vvs vvsShift1
                perm = Perm $ M.fromList permL
            in  compileSCCs sccs (perm:perms) copies
          _ -> error "compileSCCs: found strange SCC"
  in  compileSCCs (toGraph mL) [] []

-- | Test routine.
test :: IO ()
test = do putStrLn "Mutation tests:"
          testMut [(1, 2), (2, 1), (3, 1)]
          testMut [(1, 2), (5, 1), (6, 1), (2, 2), (3, 4)]
          testMut [(1, 2), (2, 3), (3, 1)]
          testMut [(1, 3), (2, 1), (3, 5), (5, 2), (7, 8), (8, 9), (9, 7), (4, 3)]

testMut :: MutationL -> IO ()
testMut ml =
  let checkMutation ms = length ms == length (nub (map fst ms))
  in  do putStrLn $ "[*] Original: "++(show ml)
         if checkMutation ml then
           do let (perms, copies) = compileM ml
              putStrLn $ "Result: "
              putStrLn $ "Permutations="++(show perms)
              putStrLn $ "Copies="++(show copies)
         else
             putStrLn "The mutation is malformed."
