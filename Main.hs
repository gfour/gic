 -- | Main program.
-- 
--   Source:
-- 
--   * If a source file name is not given in the command line, the source FL
--     program is assumed to come from standard input as a Haskell expression
--     (use @mhparse@ to generate it from a given file).
-- 
--   * If a source file name is given, it is passed through the Haskell
--     lexer/parser and transformed internally to FL.
-- 

{-# LANGUAGE CPP, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import SLIC.AuxFun (ierr, pathOf, showNum)
import SLIC.CompManager
import SLIC.Constants
import SLIC.DFI (parseDFI)
import SLIC.ITrans.II (parseII)
import SLIC.Front.HStoHF (fromHStoHF)
import SLIC.LAR.LARLinker (linkLAR)
import SLIC.State
import SLIC.SyntaxAux (Mod(..))
import SLIC.Types (FileName, FPath, MName, PPrint(..))

#ifdef USE_GHC
import GHC (DynFlags(..), GhcLink(..), GhcMode(..), TypecheckedModule(..),
            defaultErrorHandler, getSessionDynFlags, runGhc, setSessionDynFlags)
import GHC.Paths (libdir)
#if __GLASGOW_HASKELL__ > 704        
import DynFlags (ExtensionFlag(..), defaultFatalMessager, defaultFlushOut, xopt_set)
#else
import DynFlags (ExtensionFlag(..), defaultLogAction, xopt_set)
#endif
import Outputable (Outputable, ppr)
import SLIC.Front.GHCBackEnd (coreGHC, showPPr, tcGHC, transfCore)
#endif

import Data.List (isPrefixOf, map)
import Language.Haskell.Exts.Parser as Parser
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Syntax
import System.Environment

-- | Prints a usage message.
usage :: IO ()
usage = do putStrLn "Usage: gic <options> <file.hs>"
           putStrLn "* General options:"
           putStrLn "  -help   : display this help and exit"
           putStrLn "  -s      : check the validity of the FL program"
           putStrLn "  -p0     : print the FL program"
           putStrLn "  -p0pre  : print the FL program (after preprocessing)"
           putStrLn "  -p1     : preprocess and print the initial HIL program"
           putStrLn "  -p2     : preprocess and print the transformed HIL program"
           putStrLn "  -pz     : print the transformed 0-order intensional program"
           putStrLn "  -plar   : print the transformed 0-order LAR program"
           putStrLn "  -penv   : preprocess and print the typing environment"
           putStrLn "  -df     : defunctionalize and print resulting HIL program"
           putStrLn "             -v : print defunctionalized types"
           putStrLn "  -enum   : optimize enumeration datatypes (can do FL->FOFL without new data types)"
#ifdef USE_GHC
           putStrLn "  -ghc-core   : use GHC as a front-end until Core"
           putStrLn "* Type inference:"
           putStrLn "  -ghc-tc     : use GHC to parse/typecheck source code"
           putStrLn "  -gic-tc     : use the built-in type inference engine"
           putStrLn "  -gic-tc-nsig: same as -gic-tc, ignore type signatures"
           
#endif           
           putStrLn "* LAR back-end:"
           putStrLn "  -cl     : transform and compile the 0-order program to C (using lazy activation records)"
           putStrLn "             -heap    : allocate all lazy activation records on the heap"
           putStrLn "             -debug   : keep extra debugging information"
           putStrLn "             -v       : produce a graph trace file after program execution"
           putStrLn "             -nogc    : no garbage collection"
           putStrLn "             -semigc  : enable semispace garbage collection (EXPERIMENTAL)"
           putStrLn "             -libgc   : use Boehm GC [libgc] (default)"
           putStrLn("             -mem M   : use M bytes of memory (default="++(showNum defaultMemSize)++")")
           putStrLn "             -strict  : insert strictness annotations for all function" 
           putStrLn "                        formals and constructor args"           
           putStrLn "             -tag     : embed tags in constructors"
           putStrLn("             -pdfi    : print information about a "++dfiSuffix++" file")
           putStrLn "* Eduction back-end:"
           putStrLn "  -e      : transform and evaluate the 0-order program (lazy eduction)"
           putStrLn("             -maxwh N : maximum warehouse entries before GC (default="++(showNum defaultMaxWHSize)++")")
           putStrLn("             -pii     : print information about a "++iiSuffix++" file")
           putStrLn "             -v       : show debugging trace during evaluation"           
           putStrLn "* Erlang back-ends:"
           putStrLn "  -erl    : transform and pretty print the 0-order program for the Erlang intepreter"
           putStrLn "  -cerl   : transform and compile the 0-order program to Erlang"
           putStrLn("             -ctxts N : each warehouse can hold at most N contexts (default="++(showNum defaultMaxCtxts)++")")
           putStrLn "             -auto|par: parallelize all built-in operators/only par and seq"
           putStrLn "             -redis   : use the Redis-based warehouse"
           putStrLn "             -v       : show debugging trace during evaluation"
           putStrLn("             -wh N    : use N warehouses (default="++(show defaultWhs)++")")
           putStrLn "* Maude back-end:"
           putStrLn "  -cm     : transform and compile the 0-order program to Maude"
           putStrLn("             -wh N    : use N warehouses (default="++(show defaultWhs)++")")
           putStrLn "* Built-in testing interpreters:"
--           putStrLn "  -fl     : evaluate the FL program (call-by-name) (BROKEN)"
           putStrLn "  -ecbn   : transform and evaluate the 0-order program (call-by-name)"
           putStrLn "             -v : trace the evaluation"
           putStrLn "* Tagged-Token Dataflow back-end: (EXPERIMENTAL)"
           putStrLn "  -ttd    : transform and compile the 0-order program for tagged-token dataflow"
           putStrLn "  -dfg    : generate tagged-token dataflow graph"
           putStrLn "  -pttd   : transform and print the TTD program"
           putStrLn "* Compilation mode:"
           putStrLn "  -whole  : whole program defunctionalization and compilation"
           putStrLn "  -cmod   : separately compile a single module"
           putStrLn "             -v : verbose compilation mode"
           putStrLn "  -link   : link a set of compiled modules and module interfaces to generate an executable"
           putStrLn "             -v : verbose linking mode"
           -- putStrLn "  -make   : automatic mode, compile and link all given modules"

-- | Processes the command line args given.
processArgs :: [String] -> IO Options
processArgs cmdArgs =
    let aux [] opts = return opts
        aux ("-p0"    : args) opts = aux args opts{optAction = APrintFL}
        aux ("-p0pre" : args) opts = aux args opts{optAction = APrintFLPre}
        aux ("-p1"    : args) opts = aux args opts{optAction = APrintHIL1}
        aux ("-p2"    : args) opts = aux args opts{optAction = APrintHIL2}
        aux ("-s"     : args) opts = aux args opts{optAction = ACheck}
        aux ("-penv"  : args) opts = aux args opts{optAction = APrintEnv}
        aux ("-pz"    : args) opts = aux args opts{optAction = APrintZOIL}
        aux ("-plar"  : args) opts = aux args opts{optAction = APrintLAR}
        aux ("-pttd"  : args) opts = aux args opts{optAction = APrintTTD}
        aux ("-pdfi"  : args) opts = aux args opts{optAction = APrintDFI}
        aux ("-pii"   : args) opts = aux args opts{optAction = APrintII}
        aux ("-cl"    : args) opts = aux args opts{optAction = ACompileLAR}
        aux ("-ecbn"  : args) opts = aux args opts{optAction = AEvalZOILCBN}
        aux ("-e"     : args) opts = aux args opts{optAction = AEvalZOILLazy}
        aux ("-cm"    : args) opts = aux args opts{optAction = ACompileMaude}
        aux ("-ttd"   : args) opts = aux args opts{optAction = ACompileTTD}
        aux ("-erl"   : args) opts = aux args opts{optAction = AEvalErl}
        aux ("-cerl"  : args) opts = aux args opts{optAction = ACompileErl}
        aux ("-df"    : args) opts = aux args opts{optAction = APrintDF}
        aux ("-debug" : args) opts = aux args opts{optDebug = True}
        aux ("-v"     : args) opts = aux args opts{optVerbose = True}
        aux ("-enum"  : args) opts = aux args opts{optOptEnums = True}
        aux ("-strict": args) opts = aux args opts{optStrict = True}
        aux ("-nogc"  : args) opts = aux args opts{optGC = SemiGC False}
        aux ("-semigc": args) opts = aux args opts{optGC = SemiGC True }
        aux ("-libgc" : args) opts = aux args opts{optGC = LibGC}
        aux ("-tag"   : args) opts = aux args opts{optTag = True}
        aux ("-ghc-tc": args) opts = aux args opts{optGHC = GHCTc}{optTC=GHCTypeInf}
        aux ("-gic-tc": args) opts = aux args opts{optGHC = NoGHC}{optTC=GICTypeInf True}
        aux ("-gic-tc-nsig": args) opts = aux args opts{optGHC = NoGHC}{optTC=GICTypeInf False}
        aux ("-ghc-core": args) opts = aux args opts{optGHC = GHCCore}{optTC=GHCTypeInf}
        aux ("-heap"  : args) opts = aux args opts{optHeap = True}
        aux ("-redis" : args) opts = aux args opts{optWhRedis = True}
        aux ("-whole" : args) opts = aux args opts{optCMode = Whole}
        aux ("-cmod"  : args) opts = aux args opts{optCMode = CompileModule}
        aux ("-link"  : args) opts = aux args opts{optLink = True}
        aux ("-dfg"   : args) opts = aux args opts{optAction = AGenerateDFG}
        aux ("-wh"    : arg : args) opts =
            let nml :: [(Int, String)]
                nml = reads arg
            in  case nml of
                   [(n, "")] -> aux args opts{optWhNum = n}
                   _         -> usage >> return opts{optAction = ANone}
        aux ("-maxwh" : arg : args) opts =
            let nml :: [(Int, String)]
                nml = reads arg
            in  case nml of
                   [(n, "")] -> aux args opts{optWhSize = n}
                   _         -> usage >> return opts{optAction = ANone}
        aux ("-mem"   : arg : args) opts =
            let mem :: [(Int, String)]
                mem = reads arg
            in  case mem of
                   [(memSz, "")] -> aux args opts{optMaxMem = memSz}
                   _             -> usage >> return opts{optAction = ANone}
        aux ("-ctxts" : arg : args) opts =
            let ctxts :: [(Int, String)]
                ctxts = reads arg
            in  case ctxts of
                   [(n, "")] -> aux args opts{optMaxCtxts = n}
                   _         -> usage >> return opts{optAction = ANone}
        aux ("-help"  : _) opts = usage >> return opts{optAction = ANone}
        aux (arg : args) opts = 
            if "-" `isPrefixOf` arg then
                error ("Unknown flag " ++ arg ++ ", use -help to see all available options.")
            else
                case optInput opts of
                  Nothing -> aux args opts{optInput = Just [arg]}
                  Just files  -> aux args opts{optInput = Just (arg:files)} 
                -- usage >> return opts{optAction = ANone}
    in  aux cmdArgs defaultOptions

-- | Entry point, reads from a file (or stdin if no file given) and calls 
--   the main part of the compiler.
main :: IO ()
main =
    do args  <- getArgs
       opts <- processArgs args
       case optAction opts of
         ANone -> return ()
         APrintDFI ->
           case optInput opts of
             Nothing    ->
               error $ "You must give one or more "++dfiSuffix++" files."
             Just files ->
               do dfis <- mapM parseDFI files
                  _ <- mapM putStrLn $ Data.List.map (\dfi->pprint dfi "") dfis
                  return ()
         APrintII ->
           case optInput opts of
             Nothing ->
               error $ "You must give one or more "++iiSuffix++" files."
             Just files ->
               do dfis <- mapM parseII files
                  _ <- mapM putStrLn $ Data.List.map (\ii->pprint ii "") dfis
                  return ()               
         _ -> case optInput opts of
                Nothing    -> error "No source file was given."
                Just files -> parseAndProcessFL files opts

-- | Processes a source file containg FL.
parseAndProcessFL :: [FileName] -> Options -> IO ()                    
parseAndProcessFL files opts =
  let iMode = case optAction opts of
        s | s==AEvalZOILLazy -> ImpII
        _                    -> ImpDFI
      parseFiles :: [FileName] -> IO [ModFPre]
      parseFiles [] = return []
      parseFiles (f:fs) = do text <- readFile f
                             modftc <- parseFL opts f text
                             progs <- parseFiles fs 
                             return (modftc : progs)
      useGHC (modsFL, mg) =
        do let mNames = map (fst.modNameF.fst) modsFL
           let fPath = (snd.modNameF.fst) (modsFL !! 0)   -- 1st directory is used
           s <- runThroughGHC mNames fPath mg opts
           (if s then
              contProcFL opts iMode path modsFL []
            else
              putStrLn "The GHC front-end failed.")
      path = pathOf (files !! 0)
  in  if optLink opts then
        case optAction opts of
          APrintDF ->
            error "The -df option cannot be combined with -link."
          ACompileLAR   -> linkLAR files opts
          AEvalZOILLazy ->
            let files' = map (++".hs") files
            in  parseAndProcessFL files' opts
          _ -> error "The selected back-end does not support linking."
      else
        do modsFL <- parseFiles files
           modsFLGraph <- rearrangeMods (optDebug opts) modsFL
           case optGHC opts of
             GHCCore -> useGHC modsFLGraph
             GHCTc   -> useGHC modsFLGraph
             NoGHC   -> contProcFL opts iMode path (fst modsFLGraph) []

-- | Runs source modules through the GHC front-end.
runThroughGHC :: [MName] -> FPath -> [MName] -> Options -> IO Bool
#ifdef USE_GHC
runThroughGHC mNames fPath mg opts =
  let wrapper func =
#if __GLASGOW_HASKELL__ > 704        
        defaultErrorHandler defaultFatalMessager defaultFlushOut $ do      
#else
        defaultErrorHandler defaultLogAction $ do
#endif
          runGhc (Just libdir) $ do
            dflags <- getSessionDynFlags
            let dflags1 =
                  case optCMode opts of
                    CompileModule -> dflags{ghcMode=OneShot}{ghcLink=NoLink}
                    Whole         -> dflags -- {ghcMode=OneShot}{hscTarget=HscNothing}{ghcLink=NoLink}{verbosity=4}{outputHi=Nothing}
            let dflags2 = foldl xopt_set dflags1
                          [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash,
                           Opt_GADTs, Opt_GADTSyntax]
            _ <- setSessionDynFlags dflags2
            func dflags2 fPath mNames mg
  in  case optGHC opts of
        NoGHC -> ierr "runThroughGHC: the GHC API is not selected"
        GHCTc ->
          do (dflags, tMod) <- wrapper tcGHC
             -- force the result (and therefore type checking)
             return $ (length (showPPr dflags tMod)) >= 42
        GHCCore ->
          do (dflags', cMod) <- wrapper coreGHC
             binds <- cMod
             transfCore dflags' binds []
             return True

instance Outputable TypecheckedModule where
  ppr tmod = ppr $ tm_typechecked_source tmod

#else
runThroughGHC _ _ = error "GHC preprocessing not compiled in."
#endif

-- | Parses FL module source code. If the code imports names from other modules,
--   it reads the corresponding DFIs to find their types. The DFIs are to be 
--   found in the same directory as the input module. Returns the module and the 
--   external signatures needed to run it through the intensional transformation.
parseFL :: Options -> FPath -> String -> IO ModFPre
parseFL opts f text =
  let pMode = ParseMode { parseFilename = f
      -- does not work: , baseLanguage = Haskell2010
                        , Parser.extensions = [GADTs]
                        , ignoreLanguagePragmas = False
                        , ignoreLinePragmas = False
                        , fixities = Nothing
                        }
  in  case Parser.parseModuleWithMode pMode text of
        ParseFailed (SrcLoc _ srcLine0 srcColumn0) message ->
          error ("Parse error: "++(show srcLine0)++","++
                 (show srcColumn0)++": "++message)
        ParseOk hsMod ->
          return $ fromHStoHF opts f hsMod    -- translate Haskell source to FL
