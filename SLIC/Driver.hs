-- | The main entry point of the compiler.
-- 
--   Compilation steps:
-- 
--   * preprocessing (using "SLIC.Front.Preprocessor") and renaming (using
--     "SLIC.Front.Renamer")
-- 
--   * lambda-lifting (using "SLIC.Front.LLifter.Lifter")
-- 
--   * defunctionalization (using "SLIC.Front.Defunc")
-- 
--   * intensional transformation (using "SLIC.ITrans.ITrans",
--     "SLIC.ITrans.HItoZI")
-- 
--   * C code generation (using "SLIC.LAR.ZItoLAR", "SLIC.LAR.LAR")
-- 
--   Misc. features: ZOIL call-by-name interpreter ("SLIC.ITrans.Eval"),
--   ZOIL lazy eduction interpreter ("SLIC.ITrans.EvalEduction"),
--   Erlang-based back-end ("SLIC.Distr.EvalErl"), 
--   Maude back-end ("SLIC.Maude.ZItoMaude").
-- 

{-# LANGUAGE CPP #-}
module SLIC.Driver (processFL) where

import Data.Map (unions)
import Data.Maybe (catMaybes)
import SLIC.AuxFun (ierr)
import SLIC.DFI
import SLIC.Front.Defunc (ModD(ModD), defuncMod, dfiAppSigs, genModDFI, linkF,
                          optEnums)
import SLIC.Front.EvalFL (evalFL)
import SLIC.Front.Preprocessor
import SLIC.Front.Renamer
import SLIC.Front.Typeclass (builtinTcInfo, inlineTcMethods)
import SLIC.Front.TypeInfer
import SLIC.Distr.EvalErl
import SLIC.ITrans.Eval (evalZOILCBN)
import SLIC.ITrans.EvalEduction (evalZOILLazy)
import SLIC.ITrans.HFtoHI
import SLIC.ITrans.HItoZI
import SLIC.ITrans.ITrans
import SLIC.ITrans.Optimizations
import SLIC.ITrans.Syntax as ITrans.Syntax
import SLIC.LAR.LAR
import SLIC.LAR.LARAux
import SLIC.LAR.LARLinker (compileWholeL)
import SLIC.LAR.OptimizationsLAR
import SLIC.LAR.ZItoLAR
import SLIC.LAR.SyntaxLAR
import SLIC.Maude.ZItoMaude
import SLIC.State
import SLIC.SyntaxAux
import SLIC.SyntaxFL
import SLIC.TTD.TTD (callTTDBackend)
import SLIC.Types
import SLIC.Front.CAF (getCAFDcts)
import SLIC.Front.LLifter.Lifter

-- | Processes the given module according to the flags supplied
--   by the user.
-- 
--   Compilation chain:
--     program -> a-rename -> depth-counting -> lambda-lifting 
--     -> type inference -> defunctionalization
--     -> optionally dispatch to a back-end
-- 
processFL :: Options -> [DFI] -> ModF -> IO (Maybe DFI)
processFL opts dfis inputModule =
    do
      -- ---------------- FL ---------------------
      -- fix syntactic restrictions
      let p0Syntax = procModSource fixSyntax inputModule
      -- a-rename variables (and forget module information)
      let p0Alpha = uniqueNames p0Syntax
      -- rename functions that clash with built-in functions and preprocess
      -- code that may have come from the GHC front-end
      let p0Preproc = procModSource (\_ p -> convertFromGHC p) p0Alpha
      -- do datatypes-as-functions transformation
      let p0Constrs = constrToFuncs opts p0Preproc
      -- _ <- (putStrLn ("-- * Datatypes-as-functions in modules") >> printLn p0Constrs)
      -- check that the modules are now in good shape for the lambda lifter
      let p0Checked = checkMod p0Constrs
      -- lift let bindings
      let p0Lifted = lambdaLiftMod opts p0Checked
      -- final preprocessed source
      let p1 = p0Lifted
      -- _ <- (putStrLn ("-- * Original") >> printLn inputModules)
      -- _ <- (putStrLn ("-- * Syntax") >> printLn p0Syntax)
      -- _ <- (putStrLn ("-- * Alpha") >> printLn p0Alpha)
      -- _ <- (putStrLn ("-- * Lifted") >> printLn p0Lifted)
      -- _ <- (putStrLn ("-- * Preprocessed modules") >> printLn p1)

      -- run type inference on each module
      e1 <-
        case optTC opts of
          GICTypeInf useAnnot -> typeInferMod useAnnot p1
          GHCTypeInf          -> readTypeSigs p1

      -- merge all typing environments (single environment used by some back-ends)
      -- let e1 = unions modEnvs

      -- sanity check for FL
      let ok = isValidFL p0Lifted e1
      
      -- let modsAndEnvs = zip p1 modEnvs
          
      -- defunctionalize module and create accompanying DFI
      let dfModF = defuncMod opts e1 p1
      -- _ <- printLn dfMods
      -- do the -enum opimization
      let dfModFFinal = optEnums opts dfModF
      
      -- get the defunctionalized FL modules and defunctionalization interfaces
      let ModD p0Def p0DefDfi = dfModFFinal    
      -- _ <- (putStrLn ("* Defunctionalized") >> printLn p0Def)
      
      -- get the total environment
      let env = dfiTEnv $ p0DefDfi
      
      -- enable strictness mode if set with the command-line switch
      let p0DefStr = procModSource (\_ p->markStrict (optStrict opts) p) p0Def
      -- _ <- (putStrLn ("* Strictness marked") >> printLn p0Def)          
      -- enumerate case expressions and bound variables
      let p0BVars = procModSource procBV p0DefStr
      -- update the DFI with this new information
      let p0Dfi = updPMDepths p0DefDfi p0BVars
      -- _ <- (putStrLn ("* Bound variables processed") >> printLn p0BVars)
          
      -- inline type classes for statically known instances
      let tcInfo = mergeTcInfos $ [builtinTcInfo, modTCs p0BVars]
      let p0Tc = inlineTcMethods tcInfo env p0BVars
      
      -- final preprocessed and defunctionalized source to be used
      let p0Final = p0Tc
      -- _ <- printLn p0Final
      
      -- do variable usage analysis
      let cbnVars = findCBNVars p0Final
      let stricts = gatherStrictVars p0Final
      -- let mergedVarUsage = (cbnVars, stricts)
          
      if not ok then
        do putStrLn "Invalid input program."
           return Nothing
      else
        case optAction opts of 
          ACheck ->
            do putStrLn "Valid program"
               return Nothing
          -- program printers for FL, preprocessed FL, defunctionalized code,
          -- and the final program that is fed to the intensional transformation
          APrintFL -> printLn p1
          APrintFLPre -> printLn p0Final
          APrintDF ->
            (if optVerbose opts then
               putStrLn "== Environment ==" >>
               putStrLn (pprintE env "")
             else return ()) >>
            case optCMode opts of
                CompileModule ->
                  putStrLn "== Separately defunctionalized module ==" >>
                  printLn p0Final
                Whole ->
                  let wholeProgFinal = linkF opts p0Final p0Dfi
                  in  putStrLn "== Whole defunctionalized program ==" >>
                      printLn wholeProgFinal
          AEvalFL ->
            case optCMode opts of
                CompileModule ->
                  error "The non-strict FL interpreter does not support separate compilation."
                Whole ->
                  let wholeProgFinal = linkF opts p0Final p0Dfi
                  in  do evalFL wholeProgFinal
                         return Nothing
          APrintHIL1 -> printLn p1
          -- environment printer
          APrintEnv ->
            do putStr (pprintE e1 "")
               return Nothing

          -- all other options need the intensional transformation to take place
          _ ->
            let -- translates program to the intermediate intensional language
                mHIL    = fromHFtoHI p0Final
                -- run the intensional transformation
                pTrans  = itransM mHIL
                -- convert the result to ZOIL
                p3      = fromHItoZI pTrans
            in  case optAction opts of

                  -- program printers for the transformed program, the 0-order
                  -- result program, and the TTD program
                  APrintHIL2 -> printLn pTrans
                  APrintZOIL -> printLn p3
                  APrintTTD  ->
                    do callTTDBackend opts p0Dfi p3
                       return Nothing

                  -- generates the dataflow graph for the TTD program
                  AGenerateDFG ->
                     do callTTDBackend opts p0Dfi p3
                        return Nothing

                  -- interpreters: call-by-name, integrated lazy, Erlang-based
                  AEvalZOILCBN ->
                    do evalZOILCBN opts p0Dfi p3
                       return Nothing
                  AEvalZOILLazy ->
                    do evalZOILLazy opts p0Dfi p3
                       return Nothing
                  AEvalErl ->
                    case optCMode opts of
                      CompileModule -> error "The Erlang-based interpreter does not support separate compilation."
                      Whole ->
                        let m    = fst $ modNameF p3
                            code = optimize m $ modProg p3
                        in  do callErlBackend m code opts
                               return Nothing

                  -- Call the Maude back-end.
                  ACompileMaude ->
                    do callMaudeBackend (modProg p3) opts
                       return Nothing

                  -- Call the TTD back-end.
                  AEvalTTD ->
                    do callTTDBackend opts p0Dfi p3
                       return Nothing

                  -- Call the LAR back-end.
                  _  ->
                    let cUnit = (p0Final, p3, p0Dfi, cbnVars, stricts)
                    in  itransfLAR opts env cUnit

-- | A compilation unit: preprocessed FL code, 0-order intensional code, 
--   generated DFI, call-by-name/strictness information.
type CUnit = (ModF, ModZ, DFI, CBNVars, Stricts)

-- | Transfors a list of compilation units to C using the LAR back-end.
itransfLAR :: Options -> TEnv -> CUnit -> IO (Maybe DFI)
itransfLAR opts env (p0Final, p3, p0Dfi, cbnVars, stricts) =
  let -- generate all apply() signatures from the defunctionalization interfaces  
      dfAppSigs = dfiAppSigs p0Dfi
      dfiExtInfo = unzip $ catMaybes $ map ideclInfo $ modImports pLAR
      fSigs     = unions (fst dfiExtInfo)
      -- add function signatures for built-in and defunctionalization functions
      extSigs = unions [builtinFuncSigs, dfAppSigs, fSigs]
      allSigs = unions [extSigs, sigsF $ modProg p0Final]
      dts     = progData $ modProg p3
      -- calculate CIDs
      cidsExt   = unions (snd dfiExtInfo)
      cidsLocal = calcCIDs dts
      cids      = unions [cidsLocal, cidsExt, builtinCIDs]
      -- translates the ZOIL program to the LAR language      
      pLAR  = fromZOILtoLAR allSigs cids p3
      allImps  = unions $ map ideclINames $ modImports pLAR
      -- get the names of CAFs
      cafInfo = getCAFDcts p0Final
      pmDepths = countPMDepthsL pLAR
      allArities = unions [builtinArities, calcFuncArities pLAR]
      conf = ConfigLAR { getCBNVars   = cbnVars
                       , getStricts   = stricts
                       , getCIDs      = cidsLocal
                       , getArities   = allArities
                       , getOptions   = opts
                       , getPMDepths  = pmDepths
                       , getCAFnmsids = cafInfo
                       , getModName   = fst $ modNameF p0Final
                       }
  in  case optAction opts of 
        APrintLAR   ->
          do printLAR env pLAR
             return Nothing
        ACompileLAR ->
          let finalProgL = modProg $ inlineActs pLAR
          in  case optCMode opts of
                Whole         ->
                  compileWholeL p0Dfi finalProgL conf allImps >>
                  return (Just $ error "TODO: the dfi of the module")
                CompileModule ->
                  let code = modProg p0Final
                      dfi = genModDFI p0Dfi code cafInfo pmDepths
                      fm = modNameF pLAR
                  in  do compileModL fm conf env dfi allImps cidsExt finalProgL
                         return $ Just dfi
        action -> ierr $ "Invalid LAR compiler action: "++(show action)

callErlBackend :: MName -> ITrans.Syntax.ProgZ -> Options -> IO ()
callErlBackend m p opts =
  case optAction opts of
    AEvalErl -> putStrLn (makeErlRepr m p)
    a -> ierr $ "The Erlang back-end cannot handle action "++(show a)
