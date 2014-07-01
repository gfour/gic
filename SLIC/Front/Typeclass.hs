-- | The front-end that desugars typeclasses to FL.

module SLIC.Front.Typeclass (TcInst(..), TcInstF, TcInstFH, addTcInsts,
                             builtinTcDecls, builtinTcInsts, builtinTcInfo,
                             inlineTcMethods, tcISig, tc_Monad, tc_Num, tc_Show,
                             tc_Num_Int, tc_Show_Int) where

import Data.List (elemIndex)
import Data.Map (Map, fromList, toList, union)
import qualified Data.Map as M (lookup)
import Data.Maybe (fromJust)
import SLIC.AuxFun (errM, foldDot, ierr, spaces)
import SLIC.Constants
import SLIC.SyntaxAux
import SLIC.SyntaxFL
import SLIC.Types

-- | A type class instance declaration.
data TcInst a = TcInst TcName Type [DefFL a]

instance (PPrint a) => PPrint (TcInst a) where
  pprint (TcInst tcn t methods) =
    let aux d = spaces 2.pprint d.nl
    in  ("instance "++).(tcn++).(" "++).pprint t.(" where "++).nl.
        foldDot aux methods

-- | Type class instances before full patterns are eliminated.
type TcInstFH = TcInst FullPat

-- | Type class instances with simple patterns.
type TcInstF  = TcInst SimplePat

-- | Generates the signature of a type class instance (for use in the DFI). Takes
--   the name of the module where the instance is defined, and the instance.
tcISig :: MName -> TcInst a -> TcInstSig
tcISig m (TcInst tcn t _) = ((tcn, t), m)

type TcEnv = Map MethodName (Type, Arity)

pprintTcE :: TcEnv -> ShowS
pprintTcE tcE =
  let aux (mn, (t, ar)) = (mn++).(" / "++).shows ar.(" :: "++).pprint t.nl
  in  foldDot aux $ toList tcE

-- | Generates a fresh name for a simple name method in a type class instance.
mkMethodName :: MNameF -> TcName -> Type -> SName -> SName
mkMethodName fm tcn ti n = 
  let mkStrT (Tg (T dt'))    = pprint dt'
      mkStrT (Ta t' (Tv tv)) = mkStrT t'.("$"++).(tv++)
      mkStrT _ = errM fm "Cannot handle type in instance declaration."
  in  "TC$"++tcn++"$"++(mkStrT ti "")++"$"++n

-- | Integrates a list of type class instance methods with the regular 
--   definitions found in a module. Also generates the required type 
--   annotations by consulting all available type classes declarations.
addTcInsts :: [TcInstF] -> ModF -> ModF
addTcInsts tcInsts modF =
  let fm = modNameF modF
      Prog dt defs = modProg modF
      tAnnots = modTAnnot modF
      -- TODO: add here the imported type class declarations
      TcInfo tcDecls _ = modTCs modF
      tcDeclsTbl :: Map TcName TcEnv
      tcDeclsTbl = fromList $ map (\(TcDecl tcn _ ms)->(tcn, msTEnv ms)) tcDecls
      tcInstToDefs :: TcInstF -> [(DefF, TEnvI)]
      tcInstToDefs (TcInst tcn ti methods) =
        let qualN (QN m n) = QN m (mkMethodName fm tcn ti n)
            methodToDef (DefF f fs e) =
              case M.lookup tcn tcDeclsTbl of
                Nothing -> error $ "Class declaration for "++tcn++" was not found"
                Just minfo ->
                  case M.lookup (lName f) minfo of
                    Nothing ->
                      ierr $ "No class declaration for instance method "++(qName f)
                             ++" in:\n"++(pprintTcE minfo "")
                    Just (t, ar) ->
                      let f' = qualN f
                      in  (DefF f' fs e, (f', (t, Just ar)))
        in  map methodToDef methods
      (instDefs, iAnnots) = unzip $ concatMap tcInstToDefs tcInsts
      extAnnots = union tAnnots (fromList iAnnots)
  in  modF{modProg=(Prog dt (defs++instDefs))}{modTAnnot=extAnnots}

-- | Transform a type class declaration table to a typing environment.
msTEnv :: [TcMethodInfo] -> TcEnv
msTEnv methods = fromList $ map (\((f, fs), t)->(f, (t, length fs))) methods

-- * Built-in type classes

-- | All the built-in type class declarations.
builtinTcDecls :: [TcDecl]
builtinTcDecls = [ tc_Monad, tc_Num, tc_Ord, tc_Show ]

-- | The built-in 'Num' type class.
tc_Num :: TcDecl
tc_Num = TcDecl "Num" v_a
         [ (("(+)", ["x", "y"]), Tf tv_a (Tf tv_a tv_a))
         , (("(*)", ["x", "y"]), Tf tv_a (Tf tv_a tv_a))
         , (("(-)", ["x", "y"]), Tf tv_a (Tf tv_a tv_a))
         , (("negate", ["x"]), Tf tv_a tv_a)
         , (("abs", ["x"]), Tf tv_a tv_a)
         , (("signum", ["x"]), Tf tv_a tv_a)
         , (("fromInteger", ["x"]), Tf tInteger tv_a)
         ]

-- | The built-in 'Ord' type class.
tc_Ord :: TcDecl
tc_Ord = TcDecl "Ord" v_a
         [ (("compare", ["x", "y"]), Tf tv_a (Tf tv_a tInt))
         , (("(<)",  ["x", "y"]), Tf tv_a (Tf tv_a tBool))
         , (("(>=)", ["x", "y"]), Tf tv_a (Tf tv_a tBool))
         , (("(>)",  ["x", "y"]), Tf tv_a (Tf tv_a tBool))
         , (("(<=)", ["x", "y"]), Tf tv_a (Tf tv_a tBool))
         , (("max",  ["x", "y"]), Tf tv_a (Tf tv_a tv_a))
         , (("min",  ["x", "y"]), Tf tv_a (Tf tv_a tv_a))
         ]

-- | The built-in 'Show' type class.
tc_Show :: TcDecl
tc_Show = 
  let tShowS = Tf tString tString
  in  TcDecl "Show" v_a
      [ (("showsPrec", ["p", "x"]), Tf tInt (Tf tv_a tShowS))
      , (("shows", ["x"]), Tf tv_a tShowS)
      , (("showList", ["x"]), Tf (Ta tList tv_a) tShowS)
      ]

-- | The built-in 'Monad' type class.
tc_Monad :: TcDecl
tc_Monad =
  let ma = Ta tv_m tv_a
      mb = Ta tv_m tv_b
  in  TcDecl "Monad" v_m
      [ (("(>>=)", ["x", "y"]), Tf ma (Tf (Tf tv_a mb) mb))
      , (("(>>)" , ["x", "y"]), Tf ma (Tf mb mb))
      , (("return", ["x"]), Tf tv_a ma)
      , (("fail", ["s"]), Tf tString ma)
      ]

-- | Built-in type class instances.
builtinTcInsts :: [TcInstSig]
builtinTcInsts = [ tc_Num_Int, tc_Ord_Int, tc_Show_Int ]

-- | Built-in instance of 'Num' for 'Int'.
tc_Num_Int :: TcInstSig       ; tc_Num_Int = (("Num", tInt), bModN)

-- | Built-in instance of 'Ord' for 'Int'.
tc_Ord_Int :: TcInstSig       ; tc_Ord_Int = (("Ord", tInt), bModN)

-- | Built-in instance of 'Show' for 'Int'.
tc_Show_Int :: TcInstSig      ; tc_Show_Int = (("Show", tInt), bModN)

-- | Built-in type class information.
builtinTcInfo :: TcInfo
builtinTcInfo = TcInfo builtinTcDecls builtinTcInsts

-- * Type class inliner

-- | When a type class method is applied to arguments whose type is known to
--   offer a specific type class instance, inline the actual instance method.
--   Takes the list of all known type class declarations and instances.
--   Assumes that all function calls are saturated, so it must be called after
--   defunctionalization.
inlineTcMethods :: TcInfo -> TEnv -> ModF -> ModF
inlineTcMethods tcInfo env modF =
  let Prog dts defs = modProg modF
      fm = modNameF modF
      defs' = map inlineTcD defs
      tcDMethods (TcDecl tcn tv ms) = map (\((f, _), t)->(f, (tcn, tv, t))) ms
      allTcMethods :: Map MethodName (TcName, TVar, Type)
      allTcMethods = fromList $ concatMap tcDMethods $ tcIDecls tcInfo
      allTcInsts :: Map (TcName, Type) MName
      allTcInsts = fromList $ tcISigs tcInfo
      inlineTcD (DefF f fs e) = DefF f fs (inlineTcE e)
      inlineTcE (FF (BV _ _) _ _) = ierr "inlineTcE: bound variable application"
      inlineTcE (FF f@(V fName) el ci) =
        let el' = map inlineTcE el
            fNameL = lName fName
        in  case M.lookup fNameL allTcMethods of
              Nothing -> FF f el' ci
              Just (tcn, tv, t) ->
                let -- find which argument is the one with the parametric type
                    Just argIdx = Data.List.elemIndex (Tv tv) (types t)
                    arg = el' !! argIdx
                    -- find the type of the argument (if possible)
                    rT x = Just $ last $ types $ fst $ fromJust x
                    argT a = case a of
                      FF (V f') args _     ->
                        case M.lookup f' env of
                          Just (fT', Just ar') ->
                            if ar'==length args then
                              Just $ last $ types fT'
                            else
                              Nothing
                          _ -> ierr $ "argT: lookup failure: "++(pprint f' "")
                      XF (V v')           -> rT $ M.lookup v' env
                      XF (BV v' _)        -> rT $ M.lookup v' env
                      ConstrF c' _        -> rT $ M.lookup c' env
                      ConF (CN CPlus) _   -> Just tInt
                      ConF (CN CMinus) _  -> Just tInt
                      ConF (CN CMult) _   -> Just tInt
                      ConF (CN CDivide) _ -> Just tInt
                      ConF (CN CMod) _    -> Just tInt
                      ConF (CN CDiv) _    -> Just tInt
                      ConF (CN CAnd) _    -> Just tBool
                      ConF (CN COr) _     -> Just tBool
                      ConF (CN CEqu) _    -> Just tBool
                      ConF (CN CNEq) _    -> Just tBool
                      ConF (CN CLt) _     -> Just tBool
                      ConF (CN CGt) _     -> Just tBool
                      ConF (CN CLe) _     -> Just tBool
                      ConF (CN CGe) _     -> Just tBool
                      ConF (CN CIf) _     -> Nothing
                      ConF (CN CNeg) _    -> Just tInt
                      ConF (CN CTrue) _   -> Just tBool
                      ConF (CN CFalse) _  -> Just tBool
                      ConF (CN CMulI) _   -> Just tInteger
                      ConF (LitInt _) _   -> Just tInt
                      CaseF _ _ _ []      -> Nothing
                      CaseF _ _ _ ((PatB _ eP):_) -> argT eP -- use 1st pattern
                      LetF _ _ eL         -> argT eL
                      LamF {}             -> Nothing         -- can't handle lambda
                      FF (BV _ _) _ _     -> Nothing         -- can't handle bvars
                in  case argT arg of
                      Nothing -> FF f el' ci   -- cannot determine type
                      Just t' -> 
                        case M.lookup (tcn, t') allTcInsts of
                          Nothing -> FF f el' ci  -- no instance found
                          Just m -> -- found instance in module m
                            let f' = QN (Just m) (mkMethodName fm tcn t' fNameL)
                            in  FF (V f') el' ci
      inlineTcE v@(XF _) = v
      inlineTcE (ConF c el) = ConF c (map inlineTcE el)
      inlineTcE (ConstrF c el) = ConstrF c (map inlineTcE el)
      inlineTcE (CaseF d e s pats) = CaseF d (inlineTcE e) s (map inlineTcP pats)
      inlineTcE (LetF d binds e) = LetF d (map inlineTcD binds) (inlineTcE e)
      inlineTcE (LamF d v e) = LamF d v (inlineTcE e)
      inlineTcP (PatB p e) = PatB p (inlineTcE e)
  in  modF{modProg=(Prog dts defs')}