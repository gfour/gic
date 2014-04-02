-- | Debugging code for the LAR back-end.

module SLIC.LAR.LARDebug where

import SLIC.AuxFun (foldDot)
import SLIC.Constants (nl)
import SLIC.LAR.LARAux (wrapIfGC)
import SLIC.State (GC(..), Options(..))
import SLIC.Types

-- | Generates a debugging prologue before each function body. If the
--   first argument is False, it returns an empty prologue.
debugFuncPrologue :: Bool -> GC -> QName -> ShowS
debugFuncPrologue True SemiGC f =
  -- verify that the LAR on the stack is the same as the current LAR visible
  wrapIfGC
  (("printf(\"\\\\-> Entered "++).pprint f.
   ("(T0 = %p -> %p)\\n\", T0, *T0); "++).nl)
   id
debugFuncPrologue _ _ _ = id

-- | Generate debugging information after program finishes with a value.
debugMainFinish :: Options -> ShowS
debugMainFinish opts =
  if optDebug opts then
    ("printf(\"Shadow stack [%p-%p], elements: %ld\\n\", "++).
    ("sstack_bottom, sstack_ptr, (sstack_ptr-sstack_bottom));"++).nl
  else
    id

-- | Takes a list of symbols and generates a function that takes an address
--   and prints out the symbol name. If the first parameter is False, returns
--   a dummy function.
debugPrintSymbol :: Bool -> [QName] -> ShowS
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
