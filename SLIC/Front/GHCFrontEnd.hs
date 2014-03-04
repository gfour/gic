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
  let progBinds = map unLoc $ bagToList prog
      vt ab@(AbsBinds {}) =
        let binds = map unLoc $ bagToList $ abs_binds ab
        in  concatMap vtBind binds
      vt fb@(FunBind {}) = vtBind fb
      vt _ = error "vt: unknown binding"          
      vtBind fb@(FunBind {}) =
        let -- function name
            f    = unLoc $ fun_id fb
            f_vn = varName f
            f_qn = QN Nothing (occNameString (nameOccName f_vn))
            f_t  = transT dflags (varType f)
            fI   = (f_qn, (f_t, Just 0))
        in  [fI]
      vtBind _ = ierr "vtBind: unknown binding"
  in  fromList $ concatMap vt progBinds

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
