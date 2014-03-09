-- | The renamer that creates unique names for functions and variables.
-- 
--   Notes:
-- 
-- * The binder names for pattern matching expressions are assumed to be unique.
-- 

module SLIC.Front.Renamer (renInvNames, uniqueNames) where

import qualified Data.Map as M (Map, empty, fromList, lookup, map, mapKeys,
                                mapWithKey, toList, unions)
import SLIC.AuxFun (ierr, mergeM)
import SLIC.Constants (bMod)
import SLIC.Front.Typeclass
import SLIC.SyntaxAux
import SLIC.SyntaxFL
import SLIC.Types

-- * Renaming information

-- | A map of renaming pairs for unique variable name generation.
type Renamings = M.Map QName QName

-- | Renaming information is two mappings, those of module-scope renamings
--   (e.g. rename an imported function with its fully qualified name), or
--   local renamings (alpha-rename a local formal).
data RenInfo = RenModLoc Renamings Renamings

-- | The module-scope renaming projection.
moduleRen :: RenInfo -> Renamings
moduleRen (RenModLoc m _) = m

-- | The local renaming projection.
localRen :: RenInfo -> Renamings
localRen (RenModLoc _ l) = l

-- | Gather all functions that can be used in a module (locally defined and
--   imported) and create a mapping to their renamed form. For example, inside 
--   Main that defines a function f and also imports some function g from 
--   module D, the mapping is [f -> Main_f, g -> D_g].
gatherFuncMap :: ModF -> Renamings
gatherFuncMap modF = 
  let imports = modImports modF
      defs = progDefs $ modProg modF
      (m, _) = modNameF modF
      localFuncs :: [QName]
      localFuncs = map defVarName defs
      localFuncsMap :: [(QName, QName)]
      localFuncsMap = zip localFuncs (map (funcRenamer m) localFuncs)
      funcRenamer m' (QN Nothing v) = QN (Just m') v
      funcRenamer m' (QN (Just m'') v) =
        if m'==m then QN (Just m') v else ierr $ "Renaming from module "++m''++" to "++m'
  in  mergeM (M.fromList localFuncsMap) (mergeImportFunsRens imports)

mergeImportFunsRens :: [IDecl] -> Renamings
mergeImportFunsRens imports =
  M.fromList $ map (\(v, _) -> (v, v)) $ M.toList $ 
  M.unions (map ideclINames imports)
  
-- * Renaming functions

-- | The renaming function for function formals.
--   Takes the enclosing function name and the formal name.
renV :: MName -> QName -> QName -> QName
renV m (QN _ f) (QN _ v) = QN (Just m) (f++"_"++v)
  -- if M.null is then m++"_"++f++"_"++v
  -- else ierr "renV: what to do with the imports?"

-- | The renaming function for binding names.
renB :: QName -> Loc -> QName -> QName
renB (QN m f) (Just (c, _)) (QN m' v) = 
  if m==m' then
     QN m (f++"_Let"++(show c)++"_"++v)
  else
    ierr $ "Module consistency check failed in renB for : "++f++" and "++v
renB _ Nothing _ = ierr "renB: error renaming with depth=Nothing"

-- * The let/lambda enumerator

-- | Enumerates the let-bindings and the lambda abstractions in a program.
enumLetLam :: [DefF] -> [DefF]
enumLetLam defs = snd (procWithState enumLetLamD (0, 0) defs)

-- | Enumerates the let-bindings in a function definition.
enumLetLamD :: ((Int, Int), DefF) -> ((Int, Int), DefF)
enumLetLamD (st, DefF f fs e) =      -- enumLetLamD (i, DefF f fs e) =
  let (st', e') = enumLetLamE (st, e) -- restart counting for let-bindings
  in  (st', DefF f fs e')

-- | Enumerates the let-bindings in an expression. Threads a state
--   which contains two counters, one for the let bindings and one for
--   the lambda abstractions.
enumLetLamE :: ((Int, Int), ExprF) -> ((Int, Int), ExprF)
enumLetLamE (st, v@(XF _)) = (st, v)
enumLetLamE (st, FF f el) =
  let (st2, el2) = procWithState enumLetLamE st el
  in  (st2, FF f el2)
enumLetLamE (st, ConF c el) =
  let (st2, el2) = procWithState enumLetLamE st el
  in  (st2, ConF c el2)
enumLetLamE (st, ConstrF c el) =
  let (st2, el2) = procWithState enumLetLamE st el
  in  (st2, ConstrF c el2)
enumLetLamE (st, LetF loc defs e) =
  let c' = (fst st) + 1
      st1 = (c', snd st)
      (stE, e') = enumLetLamE (st1, e)
      (st', defs') = procWithState enumLetLamD stE defs            
  in  case loc of
        Nothing -> (st', LetF (Just (c', c')) defs' e')
        _ -> ierr "the renamer found already enumerated let"
enumLetLamE (st, CaseF d e b pats) =
  let enumLetLamP :: ((Int, Int), PatF) -> ((Int, Int), PatF)
      enumLetLamP (st1, PatF sPat e1) =
        let (st2, e2) = enumLetLamE (st1, e1)
        in  (st2, PatF sPat e2)
      (stE, e') = enumLetLamE (st, e)
      (stPats, pats') = procWithState enumLetLamP stE pats
  in  (stPats, CaseF d e' b pats')
-- lambda abstractions are considered separate and are numbered from 0
enumLetLamE (st, LamF d v e) =
  let (st', e') = enumLetLamE (st, e)
      d' = snd st' + 1
  in  if d==Nothing then        
        ((fst st', d'), LamF (Just d') v e')
      else
        ierr "the renamer found already enumerated lambda"
      
-- * The unique names generator

-- | Creates unique names in a program. Takes as input an FL module and
--   generates the FL module that uses the new names.
uniqueNames :: ModF -> ModF
uniqueNames modf@(Mod mInfo@(m, _) es is (Prog cs defs) an tcs) = 
  let defs' = enumLetLam defs
      -- as initial renamings, use the function renamings based on the module name
      -- or the imports
      fRens = RenModLoc (gatherFuncMap modf) M.empty
      es' = M.mapWithKey (renameFormals m) es
      renIDecl idecl =
        case ideclInfo idecl of
          Just (sigs, cids) ->
            let mn = ideclMName idecl
                info' = Just (M.mapWithKey (renameFormals mn) sigs, cids)
            in  idecl{ideclInfo=info'}
          Nothing -> idecl
      is' = map renIDecl is
  in  Mod mInfo es' is' (Prog cs (map (uniqueNamesD m fRens) defs')) an tcs

-- | In a module, rename the formals of a function given as a list, returning a new list.
renameFormals :: MName -> QName -> [QName] -> [QName]
renameFormals m f origFrmNames = map (renV m f) origFrmNames

-- | Creates unique names in a definition.
uniqueNamesD :: MName -> RenInfo -> DefF -> DefF
uniqueNamesD m ren (DefF f fs expr) = 
    let renameFrm (Frm v s) = Frm (renV m f v) s
        origFrmNames = frmsToNames fs
        renamedFrms = renameFormals m f origFrmNames
        -- generate renamings for the formal variables
        newLocalRens = M.fromList (zip origFrmNames renamedFrms)
        localRens' = mergeM (localRen ren) newLocalRens
        -- TODO: delete: add the new renamings, checking for possible clashes
        -- renamings    = mergeM ren newRenamings
        -- the new definition name is prefixed by the module name
        f' = QN (Just m) (lName f)
    in  DefF f' (map renameFrm fs) (uniqueNamesE m f (RenModLoc (moduleRen ren) localRens') expr)

-- | Creates unique name in an expression.
--   Takes an enclosing function name, a map of renamings (local and module-scope)
--   and an expression. Local renamings take precedence over module-scope renamings.
uniqueNamesE :: MName -> QName -> RenInfo -> ExprF -> ExprF
uniqueNamesE _ _ ren nvar@(XF (V v)) =
  -- first check for a name from the local renamings, then for one 
  -- from the module-scope renamings
  case M.lookup v (localRen ren) of
    Just v' -> XF (V v')
    Nothing -> case M.lookup v (moduleRen ren) of
      Just v' -> XF (V v')
      Nothing -> nvar
uniqueNamesE _ _ _ (XF (BV _ _)) =
  ierr "the renamer does not support bound variables"
uniqueNamesE m f ren (ConF c el) =
   ConF c (map (uniqueNamesE m f ren) el)
uniqueNamesE m f ren (FF func el) =
  let el' = map (uniqueNamesE m f ren) el
  in  case func of
        V func' -> 
          -- first check for a name from the local renamings, then for one
          -- from the module-scope renamings
          case M.lookup func' (localRen ren) of  
            Just f' -> FF (V f') el'
            Nothing ->
              case M.lookup func' (moduleRen ren) of  
                Just f' -> FF (V f') el'
                Nothing -> FF func el'
        -- do not process bound variable applications
        BV _ _ -> FF func el'
uniqueNamesE m f ren (CaseF d e b pats) =
  let ppat (PatF (SPat c0 bs0) e0) =
        PatF (SPat (renameConstr m ren c0) bs0) (uniqueNamesE m f ren e0)
  in  CaseF d (uniqueNamesE m f ren e) b (map ppat pats)
uniqueNamesE m f ren (ConstrF c el) =
  let c' = renameConstr m ren c
  in  ConstrF c' (map (uniqueNamesE m f ren) el)
uniqueNamesE m f ren (LetF d defs e) =
  let -- rename binding names
      defs0 = map (\(DefF f0 fs e0)->DefF (renB f d f0) fs e0) defs
      -- add the binding renamings to the local ones
      localRens' = mergeM (localRen ren) 
                   (M.fromList (map (\(DefF f0 _ _)->(f0, QN (Just m) $ lName $ renB f d f0)) defs))
      ren'  = RenModLoc (moduleRen ren) localRens'
      defs' = map (uniqueNamesD m ren') defs0
  in  LetF d defs' (uniqueNamesE m f ren' e)
uniqueNamesE m f ren (LamF d v e) =
  let v' = renV m f v
      localRens' = mergeM (localRen ren) (M.fromList [(v, v')])
  in  LamF d v' (uniqueNamesE m f (RenModLoc (moduleRen ren) localRens') e)

-- | Helper function for constructor renaming.
renameConstr :: MName -> RenInfo -> CstrName -> CstrName
renameConstr _ _ c@(QN m _) | m == bMod = c
renameConstr m ren c =
  -- if the constructor is built-in, leave it unchanged
  case M.lookup c builtinTEnv of
    Just _ -> c
    Nothing ->
      -- if the constructor is local, prefix it with module name, if it
      -- is imported, use fully qualified name from imports
      case M.lookup c (moduleRen ren) of
        Nothing -> QN (Just m) (lName c)
        Just cc -> cc

-- | Processes a list of elements keeping an integer state. Takes a function
--   that processes each element.
procWithState :: ((st, a)->(st, a)) -> st -> [a] -> (st, [a])
procWithState _ i0 [] = (i0, [])                                     
procWithState func i0 (e:es) =
  let (iE,  e')  = func (i0, e)
      (iEL, el') = procWithState func iE es
  in  (iEL, e' : el')

prepQN :: QName -> QName
prepQN (QN (Just mn) n) = QN (Just (prepStr mn)) (prepStr n)
prepQN (QN Nothing   n) = QN Nothing (prepStr n)

prepStr :: String -> String
prepStr s = map (\c->if c=='\'' then '_' else c) s

type FLNames a = ([IDecl], [Data], [DefFL a], [TcInstFH])

renInvNames :: RenameInvPat a => FLNames a -> FLNames a
renInvNames (idecls, dts, defs, tcInsts) =
  (renInvImps idecls, renInvData dts, renInvDefs defs, renInvTcInsts tcInsts)

renInvImps :: [IDecl] -> [IDecl]
renInvImps idecls =
  let renIns ins = M.mapKeys prepQN ins
      renInfo Nothing = Nothing
      renInfo (Just (fsigs, cids)) =
        Just (M.mapKeys prepQN (M.map (map prepQN) fsigs),
              M.mapKeys prepQN cids)
  in  map (\(IDecl mn ins ii)->IDecl (prepStr mn) (renIns ins) (renInfo ii)) idecls

-- | Renames invalid characters in names in data declarations.
renInvData :: [Data] -> [Data]
renInvData dts =
  let prepData (Data dt as dcs) = Data (prepDT dt) as (map prepDC dcs)
      prepDT dtName = prepQN dtName
      prepDC (DConstr c dtcs rt) =
        DConstr (prepQN c) (map prepDTC dtcs) (fmap prepT rt)
      prepDTC (DT t s sel) =
        let sel' = case sel of Nothing -> Nothing ; Just qn -> Just (prepQN qn)
        in  DT (prepT t) s sel'
      prepG (T g) = T (prepQN g)
      prepG (TDF df dt) = TDF df (prepT dt)
      prepT   (Tg g)     = Tg (prepG g)
      prepT t@(Tv _)     = t
      prepT   (Tf t1 t2) = Tf (prepT t1) (prepT t2)
      prepT   (Ta t1 t2) = Ta (prepT t1) (prepT t2)
  in  map prepData dts
                      
-- | Renames invalid characters in names in function definitions.
renInvDefs :: RenameInvPat a => [DefFL a] -> [DefFL a]
renInvDefs defs = map prepDef defs

-- | Renames invalid characters in names in type class instance method definitions.
renInvTcInsts :: [TcInstFH] -> [TcInstFH]
renInvTcInsts insts =
  let aux (TcInst tcn tv methods) =
        TcInst (prepStr tcn) tv (renInvDefs methods)
  in  map aux insts

prepDef :: RenameInvPat a => DefFL a -> DefFL a
prepDef (DefF f fs e) = 
  let prepFrm (Frm frm s) = Frm (prepQN frm) s
  in  DefF (prepQN f) (map prepFrm fs) (prepE e)

prepE :: RenameInvPat a => ExprFL a -> ExprFL a
prepE (XF v) = XF (prepV v)
prepE (ConF c el) = ConF c (map prepE el)
prepE (FF f el) = FF (prepV f) (map prepE el)
prepE (ConstrF c el) = ConstrF (prepQN c) (map prepE el)
prepE (CaseF (cn, func) e scrut pats) =
  let prepPat (PatF pat eP) = PatF (renameInvPat pat) (prepE eP)      
  in  CaseF (cn, prepQN func) (prepE e) (prepQN scrut) (map prepPat pats)
prepE (LetF d ldefs e) = LetF d (map prepDef ldefs) (prepE e)
prepE (LamF d v e) = LamF d (prepQN v) (prepE e)

prepV :: V -> V
prepV (V v) = V (prepQN v)
prepV (BV qn (cloc, func)) = BV (prepQN qn) (cloc, prepQN func)

class RenameInvPat a where
  renameInvPat :: a -> a
instance RenameInvPat SimplePat where
  renameInvPat (SPat c bvs) = SPat (prepQN c) (map prepQN bvs)
instance RenameInvPat FullPat where
  renameInvPat (FPatV v) = FPatV (prepQN v)
  renameInvPat (FPatC c ps) = FPatC (prepQN c) (map renameInvPat ps)
  renameInvPat fpat@(FPatI _) = fpat
