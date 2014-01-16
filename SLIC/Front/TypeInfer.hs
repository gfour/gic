-- | Type inference for simple types.
-- 
--   Use it only when the GHC-based type inference is not available or for
--   faster linking during development. Buggy, may explode.
-- 

module SLIC.Front.TypeInfer (isValidFL, makeTEnv, readTypeSigs, 
                             typeInferMod) where

import Prelude hiding (lookup)
import Data.List (lookup, nub, sort)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import SLIC.AuxFun (ierr, errM, trace2)
import SLIC.Constants (space)
import SLIC.Front.Preprocessor (genProjSelTEnv)
import SLIC.SyntaxAux
import SLIC.SyntaxFL
import SLIC.Types

type STVar = Int

data SType =
    STvar STVar
  | STvarV TVar         -- ^ interface with 'Type'
  | STground Ground
  | STfunc SType SType
  deriving Eq

type STAnnot = STEnv

instance PPrint SType where
  pprintPrec _ (STground g) = pprint g
  pprintPrec _ (STvar v) = shows v
  pprintPrec _ (STvarV v) = shows v
  pprintPrec p (STfunc t1 t2) =
    showParen (p > 0) (pprintPrec 1 t1 . (" -> " ++) . pprint t2)

-- machinery to allocate fresh variables for the polymorphic variables of Type

type TVarVSubst = [(TVar, SType)]

addTv :: [TVar] -> TVarVSubst -> STM TVarVSubst
addTv [] accum = return accum
addTv (tv:tvs) accum =
  case Data.List.lookup tv accum of
    Just _  -> addTv tvs accum
    Nothing ->
      freshST >>= \t ->
      addTv tvs ((tv, t):accum)

substTVs :: TVarVSubst -> SType -> SType
substTVs _     st@(STground _) = st
substTVs _     st@(STvar _)    = st
substTVs tvsub (STvarV v)      =
  case Data.List.lookup v tvsub of
    Just st'@(STvar _) -> st'
    Just _  -> ierr "substTVs: substitution is not a fresh variable"
    Nothing -> ierr "substTVs: no substitution found"
substTVs tvsub (STfunc a b) = STfunc (substTVs tvsub a) (substTVs tvsub b)

-- | Convert a Type to an SType.
t2st :: Type -> SType
t2st (Tg g)   = STground g
t2st (Tv tv)  = STvarV tv
t2st (Tf a b) = STfunc (t2st a) (t2st b)
t2st t@(Ta (Tg gList_) b) | gList_ == gList =
  case b of
    Tv _ -> STground gList                -- assume [a] is [Int]
    Tg i | i == gInt -> STground gList    -- accept [Int]
    _ ->
      error $ "Built-in type inference does not support list "++(pprint t "")
t2st t@(Ta (Ta tTuple2 t1) t2) | tTuple2 == tTuple 2 =
  case (t1, t2) of
    (Tg i1, Tg i2) | (i1 == gInt) && (i2 == gInt) ->
      STground (gTuple 2)  -- accept (Int, Int) pairs
    (Tv _, Tv _) ->
      STground (gTuple 2)  -- assume pairs are (Int, Int)
    _ ->
      error $ "Built-in type inference does not support pair "++(pprint t "")
t2st t@(Ta (Ta (Ta tTuple3 t1) t2) t3) | tTuple3 == tTuple 3 =
  case (t1, t2, t3) of
    (Tg i1, Tg i2, Tg i3) | (i1 == gInt) && (i2 == gInt) && (i3 == gInt) ->
      STground (gTuple 3)  -- accept (Int, Int, Int) triplet
    (Tv _, Tv _, Tv _) ->
      STground (gTuple 3)  -- assume triplets are (Int, Int, Int)
    _ ->
      error $ "Built-in type inference does not support pair "++(pprint t "")
t2st (Ta a _) = t2st a

t2stEnv :: TEnv -> STEnv
t2stEnv env = Map.map (\(t, _)->t2st t) env

-- | Returns the type variables of a type.
tvsOf :: SType -> [TVar]
tvsOf st =
  let isVV (STvarV _) = True
      isVV _          = False
  in  nub $ map (\(STvarV vv)->vv) $ filter isVV $ stTypes st
  
-- | Returns all types appearing in a type.
stTypes :: SType -> [SType]
stTypes st@(STvar _)    = [st]
stTypes st@(STvarV _)   = [st]
stTypes st@(STground _) = [st]
stTypes (STfunc a b)    = a : (stTypes b)

-- | Translates the types used by type inference to the real types used 
--   in the implementation.
mhRealType :: SType -> Type
mhRealType (STground g)   = Tg g
mhRealType (STfunc t1 t2) = Tf (mhRealType t1) (mhRealType t2)
mhRealType (STvar v)      = ierr $ "variable " ++ show v ++ " not generalized"
mhRealType (STvarV v)     = Tv v -- ierr $ "variable " ++ v ++ " not used"

freeST :: SType -> [STVar]
freeST (STvar v)      = [v]
freeST (STvarV _)     = []
freeST (STground _)   = []
freeST (STfunc t1 t2) =
    let l1 = freeST t1
        l2 = freeST t2
    in  l1 ++ filter (\v -> not (elem v l1)) l2

type STEnv = Map.Map QName SType
type STSub = Map.Map STVar SType
type STMstate = (Int, STEnv, STSub)

showSTEnv :: STEnv -> String
showSTEnv gamma =
   let aux [] = id
       aux ((vname, t) : l) =
          pprint vname . (" :: " ++) . pprint t . ("\n" ++) .
          aux l
   in  aux (Map.toList gamma) ""

unbind :: QName -> STEnv -> STEnv
unbind v = Map.filterWithKey (\v' _ -> v /= v')

type STD a = IO a

extractSTD :: STD a -> IO a
extractSTD = id

stdPrint :: String -> STD ()
stdPrint = putStr

newtype STM a = STM (STMstate -> STD (STMstate, Maybe a))

instance Monad STM where
    return v    = STM (\st -> return (st, return v))
    STM x >>= f = STM (\st -> x st >>= \(st', m) ->
                              case m of
                                   Just v ->
                                       let STM y = f v
                                       in  y st'
                                   Nothing ->
                                       return (st', Nothing))

messageST :: String -> STM ()
messageST msg =
  STM (\st -> stdPrint msg >> return (st, return ()))

-- | Flag to control debugging information for type inference.
deb :: Bool
deb = False

debugST :: String -> String -> STM ()
debugST cls msg =
  if deb then
    let adjust n str = take n (str ++ repeat ' ')
    in  messageST (">> " ++ adjust 8 (take 7 cls ++ ":") ++ " " ++ msg)
  else
    return ()
    
liftST :: Maybe a -> STM a
liftST m = STM (\st -> return (st, m))

extractST :: STM a -> IO a
extractST (STM x) =
  let st0 = (1, Map.empty, Map.empty)
      std = x st0 >>= \(_, m) ->
        case m of
          Just v  -> return v
          Nothing -> error "type inference failed"
  in  extractSTD std

getEnvST :: STM STEnv
getEnvST =
    STM (\st@(_, gamma, _) ->
         return (st, return gamma))

setEnvST :: STEnv -> STM ()
setEnvST gamma' =
    STM (\(n, _, sigma) ->
          return ((n, gamma', sigma), return ()))

getSubST :: STM STSub
getSubST =
    STM (\st@(_, _, sigma) ->
         return (st, return sigma))

setSubST :: STSub -> STM ()
setSubST sigma' =
    STM (\(n, gamma, _) ->
         return ((n, gamma, sigma'), return ()))

freshST :: STM SType
freshST =
    STM (\(n, gamma, sigma) ->
         return ((n+1, gamma, sigma), return (STvar n)))

unify :: String -> SType -> SType -> STM ()
unify vn st st' =
    let unify_ t@(STvar v) t'@(STvar v') =
            if v == v' then
                debugST "test" ("type match succeeded for " ++
                                 pprint t "" ++ "\n")
            else
                getSubST >>= \sigma ->
                case (Map.lookup v sigma, Map.lookup v' sigma) of
                    (Just tr, Just tr') ->
                      unifyTV vn tr tr'
                    (Just tr, Nothing) ->
                      unifyTV vn tr t'
                    (Nothing, Just tr') ->
                      unifyTV vn t tr'
                    (Nothing, Nothing) ->
                        debugST "set" (show v ++
                                       " := " ++ pprint t' "" ++ "\n") >>
                        setSubST (Map.insert v t' sigma)
        unify_ (STvar v) t =
            getSubST >>= \sigma ->
            case Map.lookup v sigma of
                Just t' ->
                  unifyTV vn t' t
                Nothing ->
                  debugST "set" (show v++" := "++pprint t ""++"\n") >>
                  setSubST (Map.insert v t sigma)
        unify_ t0 tv@(STvar _) =
          unifyTV vn tv t0
        unify_ (STfunc t1 t2) (STfunc t1' t2') =
          unifyTV vn t1 t1' >>
          unifyTV vn t2 t2'
        unify_ t1 t2 =
            if t1 == t2 then
                debugST "test" ("type match succeeded for " ++ 
                                 pprint t1 "" ++ "\n")
            else {-
              let hOrdEqDef (STfunc _ _) (STground (Thk (ThkDF _ _)))=
                    True
                  hOrdEqDef _ _ = False
              in  if (hOrdEqDef t1 t2) then
                    unify t2 t2
                  else if (hOrdEqDef t2 t1) then
                         unify t1 t1
                       else -}
                         messageST ("Unification failed for "++vn++":\n" ++
                                    "   t1 = " ++ pprint t1 "" ++ "\n" ++
                                    "   t2 = " ++ pprint t2 "" ++ "\n") >>
                         liftST Nothing
    in  debugST "unify" (pprint st "" ++ " with " ++ pprint st' "" ++ "\n") >>
        addTv (tvsOf st)  [] >>= \tvis  ->
        addTv (tvsOf st') [] >>= \tvis' ->
        unify_ (substTVs tvis st) (substTVs tvis' st')

unifyTV :: String -> SType -> SType -> STM ()
unifyTV vn st st' =
  addTv (tvsOf st)  [] >>= \tvis  ->
  addTv (tvsOf st') [] >>= \tvis' ->
  unify vn (substTVs tvis st) (substTVs tvis' st')

unifyL :: [SType] -> [SType] -> STM ()
unifyL [] [] =
    return ()
unifyL (t : tl) (t' : tl') =
    unifyTV "some-list" t t' >>
    unifyL tl tl'
unifyL tl tl' =
    messageST ("Unification failed:\n" ++
               "   tl1 = " ++ pprint tl  "" ++ "\n" ++
               "   tl2 = " ++ pprint tl' "" ++ "\n") >>
    liftST Nothing

finalize :: SType -> STM SType
finalize t@(STvar v) =
    getSubST >>= \sigma ->
    case Map.lookup v sigma of
        Just t' -> finalize t'
        Nothing -> return t
finalize (STfunc t1 t2) =
    finalize t1 >>= \t1' ->
    finalize t2 >>= \t2' ->
    return (STfunc t1' t2')
finalize t = return t

generalize :: SType -> STM SType
generalize t =
    let aux _ [] = return ()
        aux t0 (v : vl) =
            getSubST >>= \sigma ->
            debugST "ground" (show v ++ "\n") >>
            setSubST (Map.insert v (STground gInt) sigma) >>
            aux t0 vl
    in  finalize t >>= \t' ->
        aux t' (freeST t') >>
        finalize t'
        -- return t'

generalizeEnv :: STEnv -> STM STEnv
generalizeEnv ts =
  let generalizeEnv_aux [] =
        return Map.empty
      generalizeEnv_aux ((v, t) : vtl) =
        generalize t >>= \t' ->
        generalizeEnv_aux vtl >>= \vtl' ->
        return $ Map.insert v t' vtl'
  in  generalizeEnv_aux (Map.toList ts)

-- auxiliary function, used by both normal vars and bound vars
inferV :: QName -> STM SType
inferV v = getEnvST >>= \gamma ->
  case Map.lookup v gamma of
    Just t  ->
      return t
    Nothing -> 
      messageST ("undefined variable " ++ (qName v) ++ "\n") >>
      liftST Nothing

inferE :: [Data] -> STAnnot -> ExprF -> STM SType
inferE cs an e =
    let inferE_ (XF (V v)) = inferV v
        inferE_ (XF (BV bv _)) = inferV bv
        inferE_ (ConF (CN c) el) =
	    let theta CIf     = freshST >>= \t ->
                                debugST "new" (pprint t "" ++ "\n") >>
		                return (t, [STground gBool, t, t])
		theta CPlus   = return (STground gInt, [STground gInt, STground gInt])
		theta CMinus  = return (STground gInt, [STground gInt, STground gInt])
		theta CMult   = return (STground gInt, [STground gInt, STground gInt])
		theta CDivide = return (STground gInt, [STground gInt, STground gInt])
		theta CDiv    = return (STground gInt, [STground gInt, STground gInt])
		theta CMod    = return (STground gInt, [STground gInt, STground gInt])
		theta CEqu    = freshST >>= \t ->
                                debugST "new" (pprint t "" ++ "\n") >>
		                return (STground gBool, [t, t])
		theta CNEq    = freshST >>= \t ->
                                debugST "new" (pprint t "" ++ "\n") >>
		                return (STground gBool, [t, t])
		theta CLt     = return (STground gBool, [STground gInt, STground gInt])
		theta CGt     = return (STground gBool, [STground gInt, STground gInt])
		theta CLe     = return (STground gBool, [STground gInt, STground gInt])
		theta CGe     = return (STground gBool, [STground gInt, STground gInt])
		theta CAnd    = return (STground gBool, [STground gBool, STground gBool])
		theta COr     = return (STground gBool, [STground gBool, STground gBool])
                theta CNeg    = return (STground gInt, [STground gInt])
		theta CTrue   = return (STground gBool, [])
		theta CFalse  = return (STground gBool, [])
                theta CMulI   = return (STground gInteger, [STground gInteger, STground gInteger])
                theta (CMOp _)= ierr "theta encountered merged operator"
	    in  theta c >>= \(t, tl) ->
	        inferEL cs an el >>= \tl' ->
	        unifyL tl tl' >>
		return t
        inferE_ (ConF (LitInt _) []) = return (STground gInt)
        -- special: "error" has type a->b
        -- inferE_ (FF (V v) el) | v==bf_error =                    
        --      freshST >>= \a ->
        --      freshST >>= \b ->
        --      debugST "new" (pprint a "" ++ "\n") >>
        --      debugST "new" (pprint b "" ++ "\n") >>
        --      inferEL cs an el >>= \tl -> 
        --      unify (makeFunc tl b) a >>
        --      return b
        inferE_ (FF func el) =
          let v = nameOfV func               
          in  getEnvST >>= \gamma ->
              case Map.lookup v (Map.unions [builtinSTEnv, an, gamma]) of
                Just t ->
                  freshST >>= \t' ->
                  debugST "new" (pprint t' "" ++ "\n") >>
                  inferEL cs an el >>= \tl -> 
                  unifyTV (lName v) (makeFunc tl t') t >>
                  return t'
                Nothing ->
                  messageST ("undefined function " ++ (qName v) ++ "\n") >>
                  liftST Nothing
        -- TODO: the binder should be assumed to be of the same as the scrutinee
        inferE_ cexp@(CaseF _ e0 _ pats) =
            let matchType = freshST >>= \t ->
                            debugST "new" (pprint t "" ++ "\n") >>
		            return t
            in  matchType >>= \tl ->
                inferE cs an e0 >>= \t' ->
                -- infer the types of all branches and the data type examined
	        inferPats cs an pats >>= \patsT ->
                (case patsT of
                  Nothing ->
                    error $ "cannot check the type of empty case expression: "++
                             (pprint cexp "")
                  Just (tl', dt) ->
                    -- verify that e0 has same type as all pattern constructors
                    unifyTV "pat-dt" dt t'  >>
                    unifyTV "pat-tl" tl tl' >>
                    return tl)
        inferE_ (ConstrF c el) =
          let cTypes = typeOfConstr c cs
          in  inferEL cs an el >>= \_ -> -- \tl -> 
          -- TODO: unify the inferred types of the parameters list with the constructor type
              -- unify (STthunk tl) (STthunk cTypes) >>
              -- return tl
          liftST (Just (STground (T cTypes)))
          -- liftST (Just (STthunk tl))
        inferE_ (LetF _ _ _) = ierr "type inference does not support let-bindings (is lambda-lifting on?)"
        inferE_ (LamF _ _ _) = ierr "type inference does not support lambdas (is lambda-lifting on?)"
        inferE_ e' =
          ierr $ "Malformed expression during type inference: "++(pprint e' "")
    in  debugST "goal" ((pprint e "") ++ "\n") >>
        inferE_ e >>= \t ->
        debugST "infer" ((pprint e "") ++ " : " ++ pprint t "" ++ "\n") >>
        return t

makeFunc :: [SType] -> SType -> SType
makeFunc [] tr0 = tr0
makeFunc (t : tl) tr0 = STfunc t (makeFunc tl tr0)

typeOfConstr :: CstrName -> [Data] -> DTName
typeOfConstr c ds =
  let matchedDT = filter (hasConstr c) ds
      hasConstr c' (Data _ _ constrs) =
        (length (filter (isTheConstr c') constrs)) > 0
      isTheConstr cName (DConstr cName' _ _) = (cName==cName')
      Data dtName _ _ =
        case matchedDT of
          [d] -> d
          _   -> error $ "Constructor "++(show c)++" not contained in data types: "++(show ds)
  in  dtName

-- | Infers the type of a list of pattern-bound branches. Takes the list of
--   the defined data types. Returns the type that every branch has, together
--   with the type of the value examined. Returns Nothing when no brances are
--   given.
inferPats :: [Data] -> STAnnot -> [PatF] -> STM (Maybe (SType, SType))
inferPats _ _ [] =
  return Nothing
inferPats cs an (p : pl) =
  inferPat cs an p >>= \(t, tRes) ->
  case pl of
    [] ->
      return $ Just (t, tRes)
    _  ->
      inferPats cs an pl >>= \(Just (tl, tlRes)) ->
      unifyTV (pprint p "") t tl >>
      -- these are always ground types, no unification needed to check equality
      if tRes == tlRes then
        return $ Just (t, tRes)
      else
        error $ "Found branches with constructors of different types: "++
                (pprint tRes "")++", "++(pprint tlRes "")

-- | Infers the type of the constructor components bound in a pattern.
--   If the data type is locally defined, the 'data' definition is found and used.
--   If the data type is imported, the constructor will be represented by an
--   imported typed function. Returns the type of the branch and the type
--   of the pattern constructor.
inferPat :: [Data] -> STAnnot -> PatF -> STM (SType, SType)
inferPat cs an (PatF (SPat c bs) e) =
  getEnvST >>= \gamma ->
  let (newEnv, tConstr) =
        case findTypesOf c cs of
          Just (vTypes, dt) -> (Map.fromList $ zip bs vTypes, dt)
          Nothing     ->
            case Map.lookup c gamma of
              Just st -> 
                let ts       = stTypes st
                    stParams = init ts
                    stRes    = last ts
                in  (Map.fromList $ zip bs stParams, stRes)
              Nothing -> error $ "The type of constructor "++(qName c)++" does not appear in local data definitions or imported names in:\n"++(showSTEnv gamma)
  in  setEnvST (Map.union newEnv gamma) >>
      inferE cs an e >>= \t ->
      return (t, tConstr)
  
inferEL :: [Data] -> STAnnot -> [ExprF] -> STM [SType]
inferEL _ _ [] = return []
inferEL cs an (e : el) =
    inferE cs an e >>= \t ->
    inferEL cs an el >>= \tl ->
    return (t : tl)

inferDL :: [Data] -> STAnnot -> [DefF] -> STM ()
inferDL cs an dl =
    let prepare [] =
            getEnvST >>= \gamma ->
            -- inlude the types of the built-in functions
            setEnvST (Map.union (builtinFuncTypes cs) gamma) >>
            return []
        prepare (DefF v _ _ : defs) =
            freshST >>= \t ->
            debugST "new" (pprint t "" ++ "\n") >>
            getEnvST >>= \gamma ->
            debugST "var+" (show v ++ " : " ++ pprint t "" ++ "\n") >>
            setEnvST (Map.insert v t gamma) >>
            prepare defs >>= \l ->
            return (t : l)
        process [] [] =
            return ()
        process (DefF _ frms e : defs) (t : tl) =
            let vl = frmsToNames frms
                aux_formals [] =
                    freshST >>= \t0 ->
                    debugST "new" (pprint t0 "" ++ "\n") >>
                    return (t0, t0)
                aux_formals (v : vars) =
                    freshST >>= \t0 ->
                    debugST "new" (pprint t0 "" ++ "\n") >>
                    getEnvST >>= \gamma ->
                    debugST "var+" (show v ++ " : " ++ pprint t0 "" ++ "\n") >>
                    setEnvST (Map.insert v t0 gamma) >>
                    aux_formals vars >>= \(tf, tr') ->
                    return (STfunc t0 tf, tr')
                aux_unformals [] =
                    return ()
                aux_unformals (v : vs) =
                    getEnvST >>= \gamma ->
                    debugST "var-" (show v ++ "\n") >>
                    setEnvST (unbind v gamma) >>
                    aux_unformals vs
            in  aux_formals vl >>= \(tf, tr') ->
                unifyTV "t_tf" t tf >>
                inferE cs an e >>= \te ->
                unifyTV "te_tr'" te tr' >>
                aux_unformals vl >>
                process defs tl
	process _ _ =
          ierr "process(): the parameters have different sizes"
    in  prepare dl >>= \tl ->
        process dl tl

-- | Runs type inference on an FL program. Uses an external environment to
--   resolve the types of imported functions.
mhTypeInfer :: MName -> ProgF -> STAnnot -> TEnv -> IO TEnv
mhTypeInfer m (Prog cs dl) an dfiEnv =
    let cs' = typeCheckData cs dfiEnv
        dfiSEnv = t2stEnv dfiEnv
        dcT dt (DConstr c dts Nothing) =
          (c, constrST (map (\(DT t _ _)->t2st t) dts) (STground (T dt)))
        dcT _ (DConstr c _ (Just _)) =
          error$"Built-in type inference cannot handle GADT constructr "++(qName c)
        csEnv = Map.fromList $ concatMap (\(Data dt _ dcs)-> map (dcT dt) dcs) cs
        -- after type inference, find again arities of external names from the DFIs
        findAr qn =
          case Map.lookup qn dfiEnv of
            Just (_, ar) -> ar
            Nothing -> Nothing
    in  extractST (
          setEnvST (Map.union csEnv dfiSEnv) >>
          inferDL cs' an dl >>
          getEnvST >>= \gamma ->
          generalizeEnv gamma >>= \gamma' ->
          debugST (qName $ mainDefQName m) ("\n" ++ showSTEnv gamma' ++ "\n") >>
          return (Map.mapWithKey (\qn t->(mhRealType t, findAr qn)) gamma')
        )

constrST :: [SType] -> SType -> SType
constrST []       t = t
constrST (st:sts) t = STfunc st (constrST sts t)

-- Closure of type environments (adding types of parameters)

getArgType :: Type -> Int -> Type
getArgType tt n =
    let aux (t : _) 1 = t
        aux (_ : rest) m = aux rest (m-1)
	aux [] _ = error $ "getArgType\n" ++
                           "   type " ++ pprint tt "" ++ " has no " ++
		  	   show n ++ " argument"
        tl :: [Type]
        tl = takeParams tt
    in  aux tl n

-- | Returns the names of the formals of a given program variable.
formals :: ProgF -> QName -> [QName]
formals (Prog _ dl) ov =
    let aux [] = []
                 -- It catches bound bvars
                 -- error $ "formals\n" ++
                 --         "   definition of " ++ ov ++ " not found"
        aux (DefF vd vl _ : defs) =
            if ov == vd then vl else aux defs
    in  frmsToNames (aux dl)

closeEnv :: ProgF -> TEnv -> TEnv
closeEnv p xl =
    let top [] rho yl = par yl rho
        top ((v, (t, ar)) : rest) rho yl =
            let vl = formals p v
                zl = map (\(var, i) -> (var, (getArgType t i, Nothing)))
                         (zip vl (enumFrom 1))
            in  top rest ((v, (t, ar)) : rho) (yl ++ zl)
        par [] rho = rho
        par ((v, (t, ar)) : rest) rho = par rest ((v, (t, ar)) : rho)
    in  Map.fromList $ top (Map.toList xl) [] []

-- | Calculates the environment.
makeTEnv :: MName -> ProgF -> STAnnot -> TEnv -> IO TEnv
makeTEnv m p an env =
  mhTypeInfer m p an (Map.union env builtinTEnv) >>= \env' ->
  return (closeEnv p env')

-- | Given a constructor, returns the types of its components and its data type.
findTypesOf :: CstrName -> [Data] -> Maybe ([SType], SType)
findTypesOf _ [] = Nothing
findTypesOf c0 ((Data dt _ cs0) : tl) =
    let findTypesOf_ _ [] = Nothing
        findTypesOf_ c0' ((DConstr c1 dtns _) : tl1) =
            if c0' == c1 then
                Just (map (\(DT d _ _) -> t2st d) dtns, STground (T dt))
            else
                findTypesOf_ c0' tl1
    in  case (findTypesOf_ c0 cs0) of
          Nothing       -> findTypesOf c0 tl
          typs@(Just _) -> typs

-- | Check sanity of user-defined datatypes.
--   TODO: check imported data types too.
typeCheckData :: [Data] -> TEnv -> [Data]
typeCheckData hData ve =
    let -- datatype names defined
        dtNames1 :: [DTName]
        dtNames1 = (map (\(Data dName _ _) -> dName) hData) ++ builtinDTypes
        -- datatype names that can be used
        getTypesOf :: Data -> [DTName]
        getTypesOf (Data _ _ dtc) =
          let ftNames (Tg (T dt)) = [dt]
              ftNames (Tv _)      = []
              ftNames (Tg (TDF _ _)) = ierr "typeCheckData found defunc. data type"
              ftNames (Tf a b) = (ftNames a)++(ftNames b)
              ftNames (Ta _ _) = error "ftNames: found abstract data type"
              dtToFTs (DT ft _ _) = ftNames ft
          in  concatMap dtToFTs (concatMap (\(DConstr _ dts _) -> dts) dtc)
        dtNames2 = (concatMap getTypesOf hData) ++ builtinDTypes ++ dtNames1
    in  if sort (nub dtNames1) == sort (nub dtNames2) then
            hData
        else
            trace2 ("WARNING: Datatypes sanity check failed.\nDefined:\n"++(show dtNames1)++"\nFor use in constructors:\n"++(show dtNames2))
            hData

prettyPrintersFor :: [Data] -> STEnv
prettyPrintersFor ds =
  let prettyPrinterFor dt =
        [ (pprDT dt "", STfunc (STground (T dt)) (STground gInt)),
          (pprDT dt "_arg", STground (T dt)) ]
  in  Map.fromList (concatMap (\(Data dt _ _) -> prettyPrinterFor dt) ds)
        
-- * Built-in type information

-- | Types of implementation-provided Haskell functions.
builtinSTEnv :: STEnv
builtinSTEnv = t2stEnv builtinTEnv

-- | Types of the built-in variables and functions of the implementation.
builtinFuncTypes :: [Data] -> STEnv
builtinFuncTypes ds = Map.union builtinSTEnv (prettyPrintersFor ds)

calcImportsTEnv :: [IDecl] -> TEnv
calcImportsTEnv imports =
  let mkImpType (n, imp) =
        case impC imp of
          NDType -> Nothing       -- ignore imported data type names
          _ ->
            case impT imp of
              Just t  -> Just (n, (t, impA imp))
              Nothing -> ierr "type checking found untyped imports"
  in  Map.fromList $ catMaybes $ map mkImpType $
      concatMap Map.toList $ Prelude.map ideclINames imports

-- | Runs type inference on a module, returning the typing environment.
--   The module imports must have all types filled in.
typeInferMod :: Bool -> ModF -> IO TEnv
typeInferMod useAnnot (Mod (m, _) _ imports p@(Prog dts defs) tAnnot (TcInfo [] [])) =
  let importsTEnv  = calcImportsTEnv imports
      initTEnv = Map.union importsTEnv (genProjSelTEnv dts)
      importedFNames = Map.keys initTEnv
      extractFS (Just (fs, _)) = fs
      extractFS (Nothing) = ierr "imported function signatures are empty"      
      funcSigs :: Map.Map QName [QName]
      funcSigs = Map.filterWithKey onlyIFuns $ Map.unions $ map (extractFS.ideclInfo) imports
      onlyIFuns f _ = f `elem` importedFNames
      typesOfFrms :: QName -> [QName] -> TEnvL
      typesOfFrms f vs =
        case Map.lookup f initTEnv of
          Just (fT, Just fArity) ->
            let paramTypes = take fArity (takeParams fT)
            in  if length vs == fArity then
                  zip vs (zip paramTypes (repeat Nothing))
                else
                  ierr "param types list, params: different lengths"
          Just (_, Nothing) ->
            ierr $ "arity not filled in for imported name "++(qName f)
          Nothing ->
            ierr $ "No type found for imported function "++(qName f)
      frmTypes = Map.mapWithKey (\f frms->typesOfFrms f frms) funcSigs
      frmsEnv  = Map.fromList $ concat $ Map.elems frmTypes 
      -- final environment, no arities
      fEnv = refuncEnv $ Map.union initTEnv frmsEnv
      -- type annotations
      stAnnot = if useAnnot then (t2stEnv tAnnot) else Map.empty
  in  do env0 <- makeTEnv m p stAnnot fEnv
         let env1 = updEnvWithArs env0 (Map.fromList $ map defSig defs)
         return env1
typeInferMod _ modF =   
  case modTCs modF of
    TcInfo [] [] -> ierr  "Built-in type inference found malformed module."
    _ -> error "Built-in type inference cannot handle type classes."

-- | Refunctionalizes the environment, to be used by type inference of other
--   modules importing the current module.
refuncEnv :: TEnv -> TEnv
refuncEnv ve =
  let rfEnvT (t, ar) = (refuncT t, ar)
      refuncT   (Tg (TDF _ t)) = t
      refuncT t@(Tv _)         = t
      refuncT t@(Tg _)         = t
      refuncT   (Tf a b)       = Tf (refuncT a) (refuncT b)
      refuncT   (Ta a b)       = Ta (refuncT a) (refuncT b)      
  in  Map.map rfEnvT ve
      
-- | Given a list of function signatures, updates the environment for the arities
--   of these functions.
updEnvWithArs :: TEnv -> FuncSigs -> TEnv     
updEnvWithArs env sigs =
  let updateArity _ info@(_, (Just _)) = info
      updateArity f (t, Nothing) =
        case Map.lookup f sigs of
          Just fs -> (t, Just (length fs))
          Nothing -> (t, Nothing)
  in  Map.mapWithKey updateArity env

-- * Used by the GHC-API-based type checker

-- | Forms an environment from the type signatures that appear in the program.
readTypeSigs :: ModF -> IO TEnv
readTypeSigs modF =
  let fm = modNameF modF
      imports = modImports modF
      Prog dts defs = modProg modF
      codeTEnv = Map.union (modTAnnot modF) (mkConstrTEnv dts)
      importsTEnv  = calcImportsTEnv imports      
      tEnv = Map.unions [codeTEnv, importsTEnv, builtinTEnv]
      -- check that all definitions agree with the typing environment
      defTEnv :: DefF -> TEnv
      defTEnv (DefF f fs _) =
        let frmsLen = length fs            
            frmsTEnv t =
              let tTypes = takeParams t
              in  if length tTypes == frmsLen then
                    Map.fromList $
                    map (\(Frm fr _, ft)->(fr, (ft, Nothing))) $ zip fs tTypes
                  else errM fm $ (lName f)++" has type "++(pprint t "")++
                       ", but definitions has "++(show frmsLen)++" formals"
        in  case Map.lookup f tEnv of
              Nothing ->
                errM fm $ "No type signature found for function "++(qName f)++
                          " in:\n"++(pprintE tEnv "")
              Just (t, Nothing) ->
                frmsTEnv t
              Just (t, Just ar) ->
                if ar==frmsLen then frmsTEnv t
                else errM fm $ "Wrong arity found for "++(lName f)++
                     ", "++(show ar)++" but it has "++(show frmsLen)++
                     " formals: "++(pprintList space fs "")
                     
  in  return $ Map.unions (tEnv:(map defTEnv defs))
  
mkConstrTEnv :: [Data] -> TEnv
mkConstrTEnv dts =
  let dcT dt (DConstr c comps rt) =
        let dt' = case rt of Nothing -> Tg (T dt) ; Just rt' -> rt'
        in  (c, (constrT (map (\(DT t _ _)->t) comps) dt', Just $ length comps))
  in  Map.fromList $ concatMap (\(Data dt _ dcs)-> map (dcT dt) dcs) dts
