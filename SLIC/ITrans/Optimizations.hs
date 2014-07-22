-- | Misc. optimizations.
-- 
--   * Dead code elimination.
-- 
--   * Optimize enumerations (data types with nullary constructors only).
--     Combined with defunctionalization, it can transform HOFL (higher order
--     intensional language without closures and data types) to FL (first
--     order intensional language) without introducing data types.
--     This works by making each nullary constructor  into an integer constant
--     (its assigned id) and changing the environment so that the enums no
--     longer belong to their data type, but to Int.
-- 
--   * A sharing analysis spots formals that do not need to be stored in thunks
--   but can be evaluated as call-by-name.
-- 

module SLIC.ITrans.Optimizations (canOptEnums, optEnumsKernel,
                                  markCBNVars, optimize) where

import Data.List (nub)
import Data.Map (Map, empty, fromList, toList, unionsWith, unionWith)
import qualified Data.Map as M (map)
import SLIC.AuxFun (ierr)
import SLIC.Types
import SLIC.ITrans.Syntax
import SLIC.State
import SLIC.SyntaxAux
import SLIC.SyntaxFL

-- | Optimize a whole ZOIL program.
optimize :: MName -> ProgZ -> ProgZ
optimize m p =
    let stage1 = elimDeadCode m p
    in  stage1

-- | Finds the variables used in a ZOIL definition.
findUsedVarsD :: DefZ -> [QName]
findUsedVarsD (DefZ _ e) = findUsedVarsE e
findUsedVarsD (ActualsZ _ _ el) = concatMap findUsedVarsE el

-- | Finds the variables used in a ZOIL expression.
findUsedVarsE :: ExprZ -> [QName]
findUsedVarsE (XZ (V v))    = [v]
findUsedVarsE (XZ (BV v _)) = [v]
findUsedVarsE (ConZ _ el)   = concatMap findUsedVarsE el
findUsedVarsE (FZ _ v _)    = [v]
findUsedVarsE (ConstrZ _)   = []
findUsedVarsE (CaseZ _ e pats) =
    let procPat (PatB _ e0) = findUsedVarsE e0
    in  (findUsedVarsE e) ++ (concatMap procPat pats)

-- | Returns the ZOIL definition of a variable.
findDef :: QName -> [DefZ] -> DefZ
findDef v [] = error $ "Definition of "++(qName v)++" not found."
findDef v (d@(DefZ v' _) : tl) = if v==v' then d else findDef v tl
findDef v (d@(ActualsZ v' _ _) : tl) = if v==v' then d else findDef v tl

-- | Eliminates dead code.
--   Start from /result/: mark all variables used by it that are not already
--   known, and for every one of them, do the same.
elimDeadCode :: MName -> ProgZ -> ProgZ
elimDeadCode m (Prog cs defs) = 
    let mainDef = mainDefQName m
        resultVars = mainDef : (gatherUsed mainDef defs [])
        isLiveDef (DefZ v _) = v `elem` resultVars 
        isLiveDef (ActualsZ _ _ []) = False
        isLiveDef (ActualsZ v _ _) = v `elem` resultVars         
        liveDefs = filter isLiveDef defs
    in  Prog cs liveDefs

-- | Gathers all the vars used by a definition and its calls (recursively).
gatherUsed :: QName -> [DefZ] -> [QName] -> [QName]
gatherUsed v defs used =
    let def0 = findDef v defs
        vars0 = findUsedVarsD def0
        newVars = filter (\var -> notElem var used) vars0
        allVars = used ++ newVars
        recCall = concatMap (\v' -> gatherUsed v' defs allVars) newVars
    in  nub (used ++ newVars ++ recCall)

-- * Enumerations optimization

-- | Transforms data types having only nullary constructors into enumerations,
--   isomorphic to integers.
optEnumsKernel :: ([Data], TEnv) -> ([Data], TEnv)
optEnumsKernel (dt, env) =
    let enums     = filter isEnum dt        
        enumNames = map (\(Data d _ _) -> d) enums
        env'      = M.map (\(t, ar) -> (ifEnumThenInt enumNames t, ar)) env
        enumDC (DConstr c dts rt) = DConstr c (map enumDT dts) (fmap enumT rt)
        enumDT (DT t s sel) = DT (enumT t) s sel
        enumT t   = ifEnumThenInt enumNames t
        dt'       =
          map (\(Data d as dcs) -> Data d as (map enumDC dcs)) dt
    in  (dt', env')

-- | Test if the enumeration optimization and whole program compilation are set.
canOptEnums :: Options -> Bool
canOptEnums opts = optOptEnums opts && optCMode opts == Whole

-- | Checks if a data definition qualifies as an enumeration.
isEnum :: Data -> Bool
isEnum (Data _ _ cs) = 
    let isNullary (DConstr _ [] _) = True
        isNullary _                = False
    in  all isNullary cs

-- | Changes all non-built-in types (appearing in a type) that are 
--   enumerations to Int. Takes a list of the known enumerations.
ifEnumThenInt :: [DTName] -> Type -> Type
ifEnumThenInt ds typ =
  let transT t@(Tg gt) = if (dtOfGround gt) `elem` ds then tInt else t
      transT (Tf a b)  = Tf (transT a) (transT b)
      transT (Ta a b)  = Tf (transT a) (transT b)
      transT t@(Tv _)  = t
  in  transT typ

-- * Sharing analysis

-- | Analyzes a FL program to find which formals of each function do not need 
--   to be stored in thunks. This is done with a sharing analysis that counts
--   how many times a formal variable is used; if it is used <2 times and it
--   is lazy, then it is marked as call-by-name. This analysis also marks
--   all bound variables used <2 times in pattern matching throughout the
--   program. Exception: if formal scrutinees are direcly used as nesting by
--   the code generator (see 'optStruct' in 'SLIC.State'), then these are
--   assumed to require thunks.
markCBNVars :: Options -> ModF -> (ModF, CBNVars)
markCBNVars opts modF =
  let Prog dts defs = modProg modF
      scrOpt = optScrut opts
      cbnConstrParams = findCBNComps scrOpt defs
      markCBNVarsD :: DefF -> (DefF, (QName, [QName]))
      markCBNVarsD defF@(DefF f frms e) =
        let cbnInfo@(_, cbns) = findCBNVarsD defF
            transFrm frm@(Frm vf Lazy) =
              if vf `elem` cbns then (Frm vf ByName) else frm
            transFrm frm = frm
        in  (DefF f (map transFrm frms) e, cbnInfo)
      findCBNVarsD :: DefF -> (QName, [QName])
      findCBNVarsD (DefF f frms (ConstrF _ _))
        | optSharing opts =
        (f, frmsToNames (filter (\(Frm v _)-> v `elem` cbnConstrParams) frms))
        | otherwise = (f, [])
      -- returns all the formals used <2 times in the function body
      findCBNVarsD (DefF f frms e)
        | optSharing opts = 
        let si = (scrOpt, frmsToNames frms)
            frmUses = map (\(Frm v _) -> (v, countVarUses si (V v) e)) frms
            cbnFrms = filter (\(_, i) -> i<2) frmUses
        in  (f, map fst cbnFrms)
        | otherwise = (f, [])
      (defs', cbns') = unzip $ map markCBNVarsD defs
  in  (modF{modProg=(Prog dts defs')}, fromList cbns') -- (map findCBNVarsD defs)

-- | Find the call-by-name components of the constructors. These are the
--   bound variables that are always used <2 times in the pattern branches 
--   of the program.
findCBNComps :: ScrutOpt -> [DefF] -> [QName]
findCBNComps scrOpt defs =
  let aux (DefF _ frms e) = findCBNbvs (scrOpt, frmsToNames frms) e
      bvUses = toList (unionsWith max (map aux defs))
  in  map fst (filter (\(_,i)->i<2) bvUses)

-- | Analyzes an FL expression to find the maximum number of uses for
--   bound variables in pattern matching clauses.
findCBNbvs :: ScrutInfo -> ExprF -> Map QName Int
findCBNbvs _ (XF _) = empty
findCBNbvs _ (ConstrF _ _) = empty
findCBNbvs si@(scrOpt, frms) (CaseF loc eC _ pats) =
  let findBVUses (PatB (SPat _ bs, _) e) = 
        let bvUses  =
              fromList $ zip bs $ map (\v->countVarUses si (BV v loc) e) bs
            bvInner = findCBNbvs si e
        in  unionWith max bvUses bvInner
      scrutUses =
        case eC of
          XF (V v) | scrOpt && (v `elem` frms) -> fromList [(v, 2)]
          _                                    -> findCBNbvs si eC
  in  -- "case" is strict in its scrutinee
      unionWith (+) scrutUses $ unionsWith max (map findBVUses pats)
findCBNbvs si (ConF (CN c) el) = 
  case c of
    -- "if" is strict in its first argument only
    CIf -> unionWith (+) (findCBNbvs si (el!!0)) $
           unionWith max (findCBNbvs si (el!!1)) (findCBNbvs si (el!!2))
    _   -> unionsWith max (map (findCBNbvs si) el)
findCBNbvs _ (ConF (LitInt _) es) =
  case es of
    [] -> empty
    _  -> ierr "findCBNbvs: found literal application to expressions"
findCBNbvs si (FF _ el _) = unionsWith max (map (findCBNbvs si) el)
findCBNbvs _ (LetF {}) = ierr "findCBNbvs: encountered let-binding"
findCBNbvs _ (LamF {}) = ierr "findCBNbvs: encountered lambda"


