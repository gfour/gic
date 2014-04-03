-- | Debugging code for the LAR back-end.

module SLIC.LAR.LARDebug where

import SLIC.AuxFun (foldDot)
import SLIC.Constants (nl)
import SLIC.LAR.LARAux (wrapIfGC)
import SLIC.State (DebugFlag, GC(..))
import SLIC.Types

debugPrologueSemiGC :: String -> QName -> ShowS
debugPrologueSemiGC descr f =
  wrapIfGC
  (("printf(\"\\\\-> Entered "++).(descr++).(" "++).pprint f.
   ("(T0 = %p -> %p)\\n\", T0, *T0); "++).nl)
   id
  
-- | Generates a debugging prologue before each function body. If the
--   first argument is False, it returns an empty prologue.
debugFuncPrologue :: DebugFlag -> GC -> QName -> ShowS
debugFuncPrologue True SemiGC f = debugPrologueSemiGC "function" f
debugFuncPrologue _ _ _ = id

-- | Generates a debugging prologue before an actual. If the
--   first argument is False, it returns an empty prologue.
debugVarPrologue :: DebugFlag -> GC -> QName -> ShowS
debugVarPrologue True SemiGC v = debugPrologueSemiGC "actual" v
debugVarPrologue _ _ _ = id

-- | Generate debugging information after program finishes with a value.
debugMainFinish :: DebugFlag -> ShowS
debugMainFinish True =
  wrapIfGC
  (("printf(\"Pointer stack [%p-%p], elements: %ld\\n\", "++).
   ("sstack_bottom, sstack_ptr, (sstack_ptr-sstack_bottom));"++).nl) id
debugMainFinish False = id

-- | Takes a list of symbols and generates a function that takes an address
--   and prints out the symbol name. If the first parameter is False, returns
--   a dummy function.
debugPrintSymbol :: DebugFlag -> [QName] -> ShowS
debugPrintSymbol True qns =
  let aux qn = 
        let qS = pprint qn
        in  ("if (sym == "++).qS.(") { printf(\""++).qS.("\"); return; };"++).nl
  in  ("void DEBUG_printSymbol(LarArg sym) {"++).nl.
      foldDot aux qns.
      ("printf(\"<unknown>\");"++).nl.
      ("}"++).nl
debugPrintSymbol False _ =
  ("void DEBUG_printSymbol(LarArg sym) {"++).nl.
  ("printf(\"<optimized out>\");"++).nl.
  ("}"++).nl
