-- | Uses GHC as a front-end to do type checking.
-- 

{-# LANGUAGE CPP #-}
module SLIC.Front.GHCFrontEnd (getVTypes, showPPr, showSDoc', tcGHC, transT) where

import Bag (bagToList)
import GHC 
import Name (nameOccName, occNameString)
import Outputable (Outputable, SDoc, ppr, showSDoc)
import Type (Type, splitFunTys)
import Var (varName, varType)

import Data.List ((\\))
import Data.Map (fromList)
import SLIC.AuxFun (ierr, pathOf)
import SLIC.Constants
import SLIC.Types

-- | Reads type information contained in source typechecked by GHC.
getVTypes :: DynFlags -> TypecheckedSource -> TEnv
getVTypes dflags prog =
  let vtBinds bindsBag = concatMap (vtBind.unLoc) $ bagToList bindsBag
      vtBind ab@(AbsBinds {}) =
        let binds = map unLoc $ bagToList $ abs_binds ab
            -- TODO: these contain the dictionaries, use with type classes
            -- ev_vars = abs_ev_vars ab
            -- dicts = map (show.occNameString.nameOccName.varName) ev_vars
        in  concatMap vtBind binds
      vtBind fb@(FunBind {}) =
        let (f_qn, f_t) = vtId $ unLoc $ fun_id fb
            mGroup@(MatchGroup lMatches _) = fun_matches fb
            f_ar = case lMatches of
                     [lMatch] ->
                       let Match lPats _ _ = unLoc lMatch
                       in  length lPats
                     _ -> error "vtBind: only one match is allowed"
            fI   = (f_qn, (f_t, Just f_ar))            
        in  [fI]++(vtMatches mGroup)
      vtBind (PatBind {}) = ierr "vtBind: found PatBind"
      vtBind vb@(VarBind {}) = 
        let (v_qn, v_t) = vtId $ var_id vb
        in  [(v_qn, (v_t, Nothing))]++(vtExprU $ var_rhs vb)
      vtMatch (Match mPats _ gs) = (concatMap (vtPat.unLoc) mPats)++(vtGRHSs gs)
      vtMatches (MatchGroup lMatches t) = concatMap (vtMatch.unLoc) lMatches
      vtGRHSs gs =
        case grhssGRHSs gs of
          []  ->
            case grhssLocalBinds gs of
              HsValBinds _ -> error "vtGRHSs: HsValBinds are not supported."
              HsIPBinds  _ -> error "vtGRHSs: HsIPBinds are not supported."
              EmptyLocalBinds ->
                error "vtGRHSs: EmptyLocalBinds are not supported."
          grhs ->
            let gs' = map unLoc grhs
            in  concatMap vtGRHS gs'
      vtGRHS (GRHS [] e) = vtExpr $ unLoc e
      vtGRHS (GRHS _ _) = error "vtGRHSs: full guard is not supported."
      vtExpr (HsVar _) = []
      vtExpr (HsIPVar _) = error "vtExpr: HsIPVar is not supported"
      vtExpr (HsOverLit _) = []
      vtExpr (HsLit _) = []
      vtExpr (HsLam matches) = vtMatches matches
      vtExpr (HsApp f x) = (vtExprU f)++(vtExprU x)
      vtExpr (OpApp op a _ b) = (vtExprU op)++(vtExprU a)++(vtExprU b)
      vtExpr (NegApp e _) = vtExprU e
      vtExpr (HsPar e) = vtExprU e
      vtExpr (SectionL _ _) = error "vtExpr: found SectionL"
      vtExpr (SectionR _ _) = error "vtExpr: found SectionR"
      vtExpr (ExplicitTuple args _) = 
        let vtArg (Present e) = vtExprU e
            vtArg (Missing _) = [] 
        in  concatMap vtArg args
      vtExpr (HsCase e matches) = (vtExprU e)++(vtMatches matches)
      vtExpr (HsIf _ cond eT eF) = (vtExprU cond)++(vtExprU eT)++(vtExprU eF)
      vtExpr (HsLet (HsValBinds vBinds) e) = (vtVBinds vBinds)++(vtExpr $ unLoc e)
      vtExpr (HsDo _ _ _) = error "vtExpr: found HsDo"
      vtExpr (ExplicitList _ el) = concatMap vtExprU el
      vtExpr (ExplicitPArr _ _) = error "vtExpr: found ExplicitPArr"
      vtExpr (RecordCon _ _ _) = error "vtExpr: found RecordCon"
      vtExpr (RecordUpd _ _ _ _ _) = error "vtExpr: found RecordUpd"
      vtExpr (ExprWithTySig _ _) = error "vtExpr: found ExprWithTySig"
      vtExpr (ExprWithTySigOut _ _) = error "vtExpr: found ExprWithTySigOut"
      vtExpr (ArithSeq _ _) = error "vtExpr: found ArithSeq"
      vtExpr (PArrSeq _ _) = error "vtExpr: found PArrSeq"
      vtExpr (HsSCC _ _) = error "vtExpr: found HsSCC"
      vtExpr (HsCoreAnn _ _) = error "vtExpr: found HsCoreAnn"
      vtExpr (HsBracket _) = error "vtExpr: found HsBracket"
      vtExpr (HsBracketOut _ _) = error "vtExpr: found HsBracket"
      vtExpr (HsSpliceE _) = error "vtExpr: found HsSpliceE"
      vtExpr (HsQuasiQuoteE _) = error "vtExpr: found HsQuasiQuoteE"
      vtExpr (HsProc _ _) = error "vtExpr: found HsProc"
      vtExpr (HsArrApp _ _ _ _ _) = error "vtExpr: found HsArrApp"
      vtExpr (HsArrForm _ _ _) = error "vtExpr: found HsArrForm"
      vtExpr (HsTick _ _) = error "vtExpr: found HsTick"
      vtExpr (HsBinTick _ _ _) = error "vtExpr: found HsBinTick"
      vtExpr (HsTickPragma _ _) = error "vtExpr: found HsTickPragma"
      vtExpr EWildPat = []
      vtExpr (EAsPat _ _) = error "vtExpr: found EAsPat"
      vtExpr (EViewPat _ _) = error "vtExpr: found EViewPat"
      vtExpr (ELazyPat e) = vtExprU e
      vtExpr (HsType _) = error "vtExpr: found HsType"
      vtExpr (HsWrap _ e) = vtExpr e
      vtExpr e = error $ "vtExpr: unsupported expression: "++(showSDoc dflags $ ppr e) -- ++" of type "++(show $ typeOf1 e)
      vtExprU e = vtExpr $ unLoc e
      vtVBinds (ValBindsIn _ _) = error "vtVBinds: found ValBindsIn"
      vtVBinds (ValBindsOut vBindsOut _) = 
        concatMap (\(_, lBinds) -> vtBinds lBinds) vBindsOut
      vtPat (VarPat vId) =
        let (vId_qn, vId_t) = vtId vId
        in  [(vId_qn, (vId_t, Nothing))]
      vtPat (ListPat lPats _)    = concatMap (vtPat.unLoc) lPats
      vtPat (TuplePat lPats _ _) = concatMap (vtPat.unLoc) lPats
      vtPat (LitPat _) = []
      vtPat (ConPatOut {}) = []
      vtPat (CoPat _ p _) = vtPat p
      vtPat p = error $ "vtPat: found pattern: "++(showSDoc dflags $ ppr p)
      vtId vId =
        let v_vn = varName vId
            v_qn = QN Nothing (occNameString (nameOccName v_vn))
            v_t  = transGHC_T $ transT dflags (varType vId)
        in  (v_qn, v_t)
  in  fromList $ vtBinds prog

-- | Translate GHC built-in types to equivalent GIC ones.
transGHC_T :: SLIC.Types.Type -> SLIC.Types.Type
transGHC_T (Tg (T (QN (Just "GHC.Types") "Int"))) = tInt
transGHC_T (Tg (T (QN (Just "GHC.Types") "Bool"))) = tBool
-- TODO: eliminate this when type classes are implemented.
transGHC_T (Tg (T (QN (Just "GHC.Types") "IO ()"))) = tUnit
transGHC_T (Tg (T (QN (Just "GHC.Types") s))) =
  error $ "built-in GHC type missing from GIC: ["++s++"]"
transGHC_T (Tg (TDF _ _)) = ierr "transGHC_T: defunctionalized type found."
transGHC_T tg@(Tg _) = tg
transGHC_T tv@(Tv _) = tv
transGHC_T (Tf a b) = Tf (transGHC_T a) (transGHC_T b)
transGHC_T (Ta a b) = Ta (transGHC_T a) (transGHC_T b)

-- | Translates a GHC type to a GIC type.
transT :: DynFlags -> Type.Type -> SLIC.Types.Type
transT dfs ty =
  let (tArgs, tRes) = splitFunTys ty
      ftRes = Tg (T (stringToQName $ showPPr dfs tRes))     -- TODO: does this work with h.o. types?
  in  case tArgs of
        [] -> ftRes
        _  -> let aux [] = ftRes
                  aux (t:ts) = Tf (transT dfs t) (aux ts)
              in  aux tArgs

-- | Calls the GHC pretty printer for an Outputable argument.
showPPr :: DynFlags -> (Outputable a) => a -> String
showPPr dfs s = showSDoc' dfs (ppr s)

showSDoc' :: DynFlags -> Outputable.SDoc -> String
-- showSDoc' :: DynFlags -> (Outputable a) => a -> String
#if __GLASGOW_HASKELL__ >= 706
showSDoc' dfs s = showSDoc dfs s
#else
showSDoc' _ s = showSDoc s
#endif

-- | Runs a source file through the GHC parser/type inference engine.
tcGHC :: GhcMonad m => DynFlags -> FPath -> [MName] -> [MName] ->
         m (DynFlags, TypecheckedModule)
tcGHC _ file mNames mg =
  do let mn = if length mNames == 1 then (mNames!!0) else "Main"
     let fPath = pathOf file
     let fNames = map (\m->fPath++[dirSeparator]++m++".hs") mg
     -- TODO: handle whole program compilation here
     targets <- mapM (\f -> guessTarget f Nothing) fNames
     setTargets targets
     dflags <- getSessionDynFlags
     -- TODO: is this needed?
     let inclPaths' = (includePaths dflags)++[fPath]
     _ <- setSessionDynFlags dflags{includePaths=inclPaths'}{hscOutName="/dev/null"}
     let ctxtMods = map (\x -> IIDecl $ (simpleImportDecl . mkModuleName) x)
                    ((\\) mg [mn])
     setContext ctxtMods
     Succeeded <- load LoadAllTargets
     modSum <- getModSummary $ mkModuleName mn
     p <- GHC.parseModule modSum
     tMod <- typecheckModule p
     -- d <- desugarModule t
     -- l <- loadModule d
     -- n <- getNamesInScope
     -- c <- return $ coreModule d
     dflagsFinal <- getSessionDynFlags
     return $ (dflagsFinal, tMod)
