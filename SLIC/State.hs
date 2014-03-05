-- | Compiler state structure and control flags.
-- 

{-# LANGUAGE CPP #-}
module SLIC.State (Action(..), CompileMode(..), DoNullDf, GC(..), GHCAPI(..),
                   Options(..), TypeChecker(..), Verb, defaultOptions,
                   opt) where

import SLIC.Constants (defaultEStackSize, defaultWhs, defaultMaxWHSize,
                       defaultMemSize, defaultMaxCtxts, defaultWorkers, nl)
import SLIC.Types

-- | What the compiler is to do, according to the command-line parameters.
data Action =
     ANone          -- ^ do absolutely nothing
   | ACheck         -- ^ check the validity of the FL program
   | APrintDF       -- ^ defunctionalize and print resulting HIL program
   | APrintEnv      -- ^ preprocess and print the HIL environment
   | APrintFL       -- ^ print the FL program
   | APrintFLPre    -- ^ print the FL program (preprocessed)
   | APrintHIL1     -- ^ preprocess and print the initial HIL program
   | APrintHIL2     -- ^ preprocess and print the transformed HIL program
   | APrintZOIL     -- ^ transform and print the 0-order program
   | APrintTTD      -- ^ transform and print the TTD program
   | APrintLAR      -- ^ transform and print the 0-order (LAR) program
   | APrintDFI      -- ^ pretty-print a defunctionalization interface file
   | ACompileLAR    -- ^ transform and compile the 0-order program to C (LAR)
   | ACompileMaude  -- ^ transform and compile to Maude
   | AEvalTTD       -- ^ evaluate the program using the dataflow interpreter
   | AEvalErl       -- ^ transform and translate for the Erlang interpreter
   | AEvalFL        -- ^ evaluate the FL program using the non-strict FL semantics
   | AEvalZOILCBN   -- ^ transform and print the 0-order program (call-by-name)
   | AEvalZOILLazy  -- ^ transform and print the 0-order program (call-by-need)
   | AGenerateDFG   -- ^ generate the dataflow graph
   deriving (Eq, Show)

data CompileMode =
     Whole         -- ^ whole program compilation
   | CompileModule -- ^ compile a single Module.hs to Module.o (and accompanying interface)
   deriving (Eq, Show)
              
-- | The garbage collector to use.
data GC =
    SemiGC       -- ^ custom semispace garbage collector
  | LibGC        -- ^ Boehm's garbage collector (libgc)
  deriving (Eq)
                 
-- | What part of GHC the compiler may use.
data GHCAPI =
    NoGHC      -- ^ the GHC API is not used (no need to link against it)
  | GHCTc      -- ^ only use the type checker of the GHC API
  | GHCCore    -- ^ use GHC until (Prepared) Core is emitted
  deriving (Eq)

-- | The different type checking/inference engines that can be used.
--   
--   * Full type inference from the GHC API. Supports all features supported
--     by GHC (polymorphism, GADTs, type classes). Needs explicit type
--     signatures in all definitions in the source code. The compiler
--     must have been built with support for the GHC API.
-- 
--   * Simple type inference: monomorphic type inference (without explicit
--     type signatures) with fallback to polymorphic type signatures. This
--     is an incomplete type inference engine and should only be used when
--     the GHC API is not available/desirable (e.g. due to codesize). Can
--     only support parametric polymorphism in functions (no parameterized
--     data types\/GADTs\/type classes).
-- 
data TypeChecker =
    GHCTypeInf       -- ^ full type inference using the GHC API
  | GICTypeInf Bool  -- ^ simple built-in type inference (flag controls if type signatures are used)
  deriving (Eq)

-- | A flag that controls verbsity.
type Verb = Bool

-- | Optionally does an IO action.
opt :: Verb -> IO () -> IO ()
opt v x = if v then x else return ()

-- | The flag that enables nullary defunctionalization.
type DoNullDf = Bool

-- | The state of the compiler, as defined by the user options.
data Options = Options
  { optAction  :: Action           -- ^ what to do
  , optInput   :: Maybe [FileName] -- ^ where to read the input (Nothing=stdin, otherwise list of files)
  , optVerbose :: Verb             -- ^ verbose flag
  , optDebug   :: Bool             -- ^ debugging flag
  , optHeap    :: Bool             -- ^ allocate all lazy activation records on the heap
  , optGC      :: GC               -- ^ enable garbage collection
  , optTC      :: TypeChecker      -- ^ which type checker to use
  , optGHC     :: GHCAPI           -- ^ enable preprocessing using GHC
  , optStrict  :: Strictness       -- ^ enable automatic optrictness everywhere
  , optTag     :: Bool             -- ^ enable tags inside constructors
  , optMaxMem  :: Int              -- ^ size of total memory to use (in bytes)
  , optWhNum   :: Int              -- ^ the number of the warehouses to use for parallel eduction
  , optWhSize  :: Int              -- ^ maximum warehouse size before GC
  , optOptEnums:: Bool             -- ^ compile enums (datatypes wih nullary constructors) to integers
  , optNullDf  :: DoNullDf         -- ^ do nullary defunctionalization
  , optMaxCtxts:: Int              -- ^ maximum number of contexts per warehouse
  , optLink    :: Bool             -- ^ linking mode
  , optCMode   :: CompileMode      -- ^ the current compilation mode (whole program, partial compilation)
  , optWhRedis :: Bool             -- ^ use the Redis-based warehouse for the Erlang back-end
  , optCompact :: Bool             -- ^ use the compact x86-64 representation
  , optFastOp  :: Bool             -- ^ use the fast integer operations
  , optNWorkers:: Int              -- ^ number of workers used by the TTD back-end
  , optEStackSz:: Int              -- ^ explicit stack size
  }

-- | The default options of the compiler. It is used as a default for
--   applying user command-line switches, or by the GHC back-end.
defaultOptions :: Options
defaultOptions = Options
  { optAction  = ACompileLAR
  , optInput   = Nothing
  , optVerbose = False
  , optDebug   = False
  , optStrict  = False
  , optWhNum   = defaultWhs
  , optWhSize  = defaultMaxWHSize
  , optMaxMem  = defaultMemSize
  , optGC      = LibGC
#ifdef USE_GHC
  , optTC      = GHCTypeInf
#else
  , optTC      = GICTypeInf True
#endif /* USE_GHC */
  , optTag     = False
  , optGHC     = NoGHC
  , optHeap    = False
  , optOptEnums= False
  , optNullDf  = False
  , optMaxCtxts= defaultMaxCtxts
  , optLink    = False
  , optCMode   = Whole
  , optWhRedis = False
  , optCompact = False
  , optFastOp  = False
  , optNWorkers= defaultWorkers
  , optEStackSz= defaultEStackSize
  }

instance Show Options where
  showsPrec _ opts =
    ("State:"++).nl.
    ("Input: "++).(case optInput opts of Nothing -> id; Just inp -> shows inp).nl.
    ("Compilation mode: "++).shows (optCMode opts).nl.
    ("Garbage collector: "++).
    (case optGC opts of
        SemiGC -> ("semispace"++)
        LibGC  -> ("libgc"++)).nl
    
