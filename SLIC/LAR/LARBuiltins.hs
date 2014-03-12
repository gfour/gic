-- | The built-in functions and variables used in the generated 
--   C code of the LAR back-end.
-- 

module SLIC.LAR.LARBuiltins (bfsLARInfo, blockNotApp, builtins,
                             builtinConstrsDecls, mulISig, prettyPrintersC, 
                             prettyPrintersFor, pprinterName, pprinterSig) where

import Data.List (intersperse)
import Data.Map (lookup)
import SLIC.AuxFun
import SLIC.Constants
import SLIC.Front.Defunc (dfDT)
import SLIC.LAR.LARAux
import SLIC.LAR.SMacrosAux (declF, mkDefineVar, mkAllocAR, mkGETARG, mkVALS)
import SLIC.LAR.SyntaxLAR
import SLIC.State
import SLIC.SyntaxAux
import SLIC.SyntaxFL
import SLIC.Tags
import SLIC.Types

t0 :: String
t0 = "T0"
  
-- | Creates the C built-in functions that are needed by the generated code.
--   This contains automatically generated 'Show' functions and auxiliaries.
builtins :: Options -> ShowS
builtins opts =
  let gc = optGC opts
      compact = optCompact opts
  in  b_readIntIO.nl.
      b_printIntIO gc.nl.
      b_putStr gc compact.nl.
      b_putStrLn.nl.
      b_toInteger opts.nl.
      b_strToList gc compact.nl.
      b_show gc.nl.
      b_mulI opts.nl.
      builtinConstrsC.
      b_runMainIO.nl.
      b_error.nl.
      b_par gc.nl.
      b_pseq gc.nl

-- | The memory allocation function for arbitrary precision numbers.
gmpMalloc :: Options -> String -> ShowS
gmpMalloc opts sz =
  if optGC opts == LibGC then ("GC_MALLOC("++).(sz++).(")"++) 
  else ("malloc("++).(sz++).(")"++)

{-
-- | The free() function for arbitrary precision numbers.
gmpFree :: Options -> String -> ShowS
gmpFree opts ptr =
  if optGC opts == LibGC then id
  else ("free("++).(ptr++).(")"++)
-}

-- | Pretty-printing (and forcing) functions.
prettyPrintersC :: CompactOpt -> ShowS
prettyPrintersC compact =
  prettyPrintInt compact.nl.
  prettyPrintBool.nl.
  prettyPrintInteger.nl.
  prettyPrintDefunc.nl.
  prettyPrintList compact.nl.
  prettyPrintUnit.nl.
  prettyPrintMagic.nl

-- * Data types pretty printer

-- | Generates the pretty printing C functions for all data types
--   that are not generated by defunctionalization.
prettyPrintersFor :: [Data] -> CIDs -> ShowS
prettyPrintersFor ds cids = prettyPrinters (filter dtNotDF ds) cids

-- | The pretty printer generator for data type declarations. The resulting
--   C functions force data structures, so should not be used for infinite
--   data. Doesn't support Unicode.
prettyPrinters :: [Data] -> CIDs -> ShowS
prettyPrinters dTypes cids =
    foldDot (prettyPrinterDT cids) dTypes

-- | The name of the pretty printer of a data type.
pprinterName :: DTName -> ShowS
pprinterName dt = ("_pretty_print_"++).pprint dt

-- | The signature of the pretty printer of a data type.
pprinterSig :: DTName -> ShowS
pprinterSig dt = ("Susp "++).pprinterName dt.("(Susp i)"++)

-- | Given a constructor enumeration and a data type declaration, it generates 
--   a pretty printer for it.
prettyPrinterDT :: CIDs -> Data -> ShowS
prettyPrinterDT cids (Data dtName _ constrs) =
    let maxArity = foldl max 0 (map dLength constrs)
        dLength (DConstr _ ds _) = length ds
        comps = intersperse ", " (map (\n -> "comp"++(show n)) [1..maxArity])
    in  pprinterSig dtName.(" {"++).nl. -- ("(int constr, TP_ T0) {"++).nl.
        (if length comps > 0 then
             tab.("Susp "++).((concat comps)++).semi.nl
         else
             id
        ).
        tab.("switch (CONSTR(i)) {"++).nl.
        foldDot (prettyPrinterConstr cids) constrs.
        tab.("}"++).nl.
        tab.("return (SUSP(1, "++).uTag.(", 0));"++).nl.
        ("}"++).nl

-- | The part of the pretty printer that handles a constructor.
prettyPrinterConstr :: CIDs -> DConstr -> ShowS
prettyPrinterConstr cids (DConstr c cts _) =
    let ctsLength = length cts
        aux1 :: [(DTName -> ShowS)]
        aux1 = map (prettyPrintDTName c) [1..ctsLength]
        printComma = tab.tab.("printf(\", \");"++).nl
        -- the strictness annotations of the data type components are ignored
        -- since we force the constructors anyway
        cts' = map fstBaseDT cts
        fstBaseDT (DT    (Tg (T g)) _ _)     = g
        fstBaseDT (DT    (Tg (TDF _ _)) _ _) = dfDT
        -- TODO: for polymorphic components, we need Show and friends
        fstBaseDT (DT    (Tv _) _ _)         = dtMagic
          -- ierr "component is polymorphic, can't create pretty printer"
        fstBaseDT (DT    (Ta _ _) _ _)       = dtMagic
          -- ierr "data type is polymorphic, can't create pretty printer"
        fstBaseDT (DT ft@(Tf _ _      ) _ _) =
          ierr ("Higher-order data type declaration "++(pprint ft "")++" found")
        comps = intersperse printComma (map (\(x, y) -> x y) (zip aux1 cts'))
        cl = length comps
    in  tab.("case "++).shows (findID c cids).(":"++).nl.
        tab.tab.("printf(\""++).((lName c)++).
        (if cl > 0 then ("("++) else id).
        ("\");"++).nl.
        (foldl (.) id comps).
        (if cl > 0 then tab.tab.("printf(\")\");"++).nl else id).
        tab.tab.("break;"++).nl

-- | The part of the pretty printer that handles a constructor component.
prettyPrintDTName :: CstrName -> Int -> DTName -> ShowS
prettyPrintDTName c n dtName =
  let comp = ("comp"++).(shows n)
      bv   = pprint $ ithFrmOfCstr (n-1) c
  in  tab.tab.comp.(" = "++).bv.("(CPTR(i));"++).nl.
      tab.tab.pprinterName dtName.
      ("("++).comp.(");"++).nl

-- * Hard-coded built-in functions

-- | Built-in pretty printer for Int.
prettyPrintInt :: CompactOpt -> ShowS
prettyPrintInt compact = 
  pprinterSig dtInt.(" {"++).nl.
  (if compact then
     tab.("printf(\"%ld\", PVAL_R(i));"++)
   else
     tab.("printf(\"%d\", CONSTR(i));"++)).nl.
  tab.("return (SUSP(1, "++).uTag.(", 0));"++).nl.
  ("}"++).nl

-- | Built-in pretty printer for Bool.
prettyPrintBool :: ShowS
prettyPrintBool = 
  pprinterSig dtBool.(" {"++).nl.
  tab.("printf(\"%d\", CONSTR(i));"++).nl.
  tab.("return (SUSP(1, "++).uTag.(", 0));"++).nl.
  ("}"++).nl

-- | Built-in pretty printer for Integer.
prettyPrintInteger :: ShowS
prettyPrintInteger = 
    pprinterSig dtInteger.(" {"++).nl.
    wrapIfGMP
    (tab.("printf(\"%s\", mpz_get_str(0, 10, *((mpz_t*)(CPTR(i)))));"++).nl)
    (tab.("printf(\"Cannot print Integer, no libgmp support.\\n\");"++).nl).
    tab.("return (SUSP(1, "++).uTag.(", 0));"++).nl.
    ("}"++).nl

-- | Built-in pretty printer for defunctionaized values (needed for the
--   MagicTypleN work-around).
prettyPrintDefunc :: ShowS
prettyPrintDefunc = 
    pprinterSig dfDT.(" {"++).nl.
    tab.("printf(\"<defunc. closure constr. %d>\", CONSTR(i));"++).nl.
    tab.("return (SUSP(1, "++).uTag.(", 0));"++).nl.
    ("}"++).nl

-- | Built-in pretty printer for Bool.
prettyPrintMagic :: ShowS
prettyPrintMagic = 
    pprinterSig dtMagic.(" {"++).nl.
    tab.("printf(\"*magic:%d*\", CONSTR(i));"++).nl.
    tab.("return (SUSP(1, "++).uTag.(", 0));"++).nl.
    ("}"++).nl

-- | Dummy version of the runMainIO function used by GHC.
b_runMainIO :: ShowS
b_runMainIO =
  ("FUNC("++).pprint bf_runMainIO.(") {"++).nl.
  tab.("return (SUSP(0, "++).uTag.(", 0));"++).nl.
  ("}"++).nl

-- | Reads an integer from the standard input.
b_readIntIO :: ShowS
b_readIntIO =
  ("FUNC("++).pprint bf_readIntIO.(") {"++).nl.
  tab.("int i;"++).nl.
  tab.("if (scanf(\"%d\", &i)==EOF) { printf(\"error: scanf: EOF\\n\"); exit(-1); };"++).nl.
  tab.("return (SUSP(i, "++).uTag.(", 0));"++).nl.
  ("}"++).nl

-- | Writes an integer to the standard input and returns it.
b_printIntIO :: GC -> ShowS
b_printIntIO gc =
  ("FUNC("++).pprint bf_printIntIO.(") {"++).nl.
  tab.("Susp i = "++).mkGETARG gc bf_printIntIO 0 t0.(";"++).nl.
  tab.("printf(\"%d\\n\", CONSTR(i));"++).nl.
  tab.("return i;"++).nl.
  ("}"++).nl

-- | Writes a string to the standard input.
b_putStr :: GC -> CompactOpt -> ShowS
b_putStr gc compact =
  let Just (_, cidCons) = Data.Map.lookup bf_Cons builtinCIDs
  in  ("FUNC("++).pprint bf_putStr.(") {"++).nl.
      tab.("Susp i = "++).mkGETARG gc bf_putStr 0 t0.(";"++).nl.
      tab.("while ((CONSTR(i)) == "++).shows cidCons.(") {"++).nl.
      tab.tab.("printf(\"%c\", "++).
      (if compact then ("(unsigned char)PVAL_R"++) else ("CONSTR"++)).
      ("("++).mkGETARG gc bf_Cons 0 "CPTR(i)".("));"++).nl.
      tab.tab.("i = "++).mkGETARG gc bf_Cons 1 "CPTR(i)".(";"++).nl.
      tab.("}"++).nl.
      tab.("return "++).pprint bf_Unit.("(0);"++).nl.
      ("}"++).nl
        
-- | Writes a string followed by a newline to the standard input.
b_putStrLn :: ShowS
b_putStrLn =
  ("FUNC("++).pprint bf_putStrLn.(") {"++).nl.
  tab.("Susp ret = "++).pprint bf_putStr.("(T0);"++).nl.
  tab.("printf(\"\\n\");"++).nl.
  tab.("return ret;"++).nl.
  ("}"++).nl

b_error :: ShowS
b_error =
  ("FUNC("++).pprint bf_error.(") {"++).nl.
  -- TODO: this should go to stderr, together with the putStrLn output
  tab.("printf(\"error: \");"++).nl.
  tab.("Susp ret = "++).pprint bf_putStrLn.("(T0);"++).nl.
  tab.("exit(-1);"++).nl.
  tab.("return ret;    // this should be dead code"++).nl.
  ("}"++).nl

-- | Generates the dummy constructor/tag prefix for special data containing
--   pointers embeded in the context field (such as Integer values).
dummyPre :: ShowS
dummyPre = ("0"++)

-- | Converts an Int to an Integer.
b_toInteger :: Options -> ShowS
b_toInteger opts =
  ("FUNC("++).pprint bf_toInteger.(") {"++).nl.
  (if optCompact opts then
     ("printf(\"TODO: toInteger for -compact\");"++).nl
   else
     wrapIfGMP
     (tab.("Susp i = "++).mkGETARG (optGC opts) bf_toInteger 0 t0.(";"++).nl.
      -- tab.("printf(\"Initializing big int %d\\n\", i.constr);"++).nl.
      tab.("mpz_t *ret = (mpz_t *)"++).gmpMalloc opts "sizeof(mpz_t)".(";"++).nl.
      tab.("mpz_init_set_ui(*ret, i.constr);"++).nl.
      -- tab.("printf(\"Initialized big int %d in addr %p: \", i.constr, ret);"++).nl.
      -- tab.("gmp_printf(\"verify=%d\\n\", mpz_get_ui(*ret));"++).nl.
      tab.("return (SUSP("++).dummyPre.(", "++).uTag.(", (TP_)ret));"++).nl)
     id).
  (tab.("return (SUSP("++).dummyPre.(", "++).uTag.(", 0));"++).nl).
  ("}"++).nl

-- | Converts a C string to a Haskell list. For internal use.
b_strToList :: GC -> CompactOpt -> ShowS
b_strToList gc compact =
  let Just (_, cidCons) = Data.Map.lookup bf_Cons builtinCIDs
      Just (_, cidNil)  = Data.Map.lookup bf_Nil  builtinCIDs
  in  ("Susp strToList(char *str, int chars, TP_ T0) {"++).nl.
      tab.("// construct list from back-to-front"++).nl.
      tab.("int d;"++).nl.
      tab.("Susp lastCell = SUSP("++).shows cidNil.(", "++).listTag.
          (", 0);  // the end of the list"++).nl.
      tab.("TP_ consTP;"++).nl.
      tab.("for (d=chars-1; d>=0; d--) {"++).nl.
      tab.tab.("consTP = "++).
              mkAllocAR gc True compact bf_Cons 2 0 [("0"++), ("0"++)].("; "++).
              ("// generate evaluated LAR for Cons"++).nl.
      tab.tab.("// read character as integer"++).nl.
      tab.tab.(mkVALS gc 0 2 "consTP").(" = SUSP((int)str[d], "++).listTag.(", 0);"++).nl.
      tab.tab.("// add last cons cell to the back"++).nl.
      tab.tab.(mkVALS gc 1 2 "consTP").(" = lastCell;"++).nl.
      tab.tab.("lastCell = SUSP("++).shows cidCons.(", "++).listTag.(", consTP);"++).nl.
      tab.("}"++).nl.
      tab.("return lastCell;"++).nl.
      ("}"++).nl

-- | Converts its integer argument to a character list.
--   For now, it is the Show Int implementation.
b_show :: GC -> ShowS
b_show gc =
  ("FUNC("++).pprint bf_show.("){"++).nl.
  tab.("Susp i = "++).mkGETARG gc bf_show 0 t0.(";"++).nl.
  tab.("// find number of digits"++).nl.
  tab.("int a = abs(CONSTR(i)), digits = 0;"++).nl.
  tab.("while (a > 0) { a /= 10; digits++; }"++).nl.
  tab.("// allocate space enough for the digits, the sign and the '\\0'"++).nl.
  tab.("char str[digits+2];"++).nl.
  tab.("// convert to string (and keep written digits number)"++).nl.
  tab.("int wdigits = snprintf(str, digits+2, \"%d\", CONSTR(i));"++).nl.
  tab.("return strToList(str, wdigits, T0);"++).nl.
  ("}"++).nl

b_par :: GC -> ShowS
b_par gc =
  ("FUNC("++).pprint bf_par.("){"++).nl.
  wrapIfOMP
  (tab.("Susp a, b;"++).nl.
   ("#pragma omp single nowait"++).nl.
   ("{"++).nl.
   ("#pragma omp task untied"++).nl.
   tab.("{ a = "++).mkGETARG gc bf_par 0 t0.("; }"++).nl.
   ("#pragma omp task untied"++).nl.
   tab.("{ b = "++).mkGETARG gc bf_par 1 t0.("; }"++).nl.
   ("}"++).nl.
   ("#pragma omp taskwait"++).nl.
   ("return b;"++).nl)
  (tab.("Susp b = "++).mkGETARG gc bf_par 1 t0.(";"++).nl.
   tab.("return b;"++).nl).
  ("}"++).nl

b_pseq :: GC -> ShowS
b_pseq gc =
  ("FUNC("++).pprint bf_pseq.("){"++).nl.
  wrapIfOMP (tab.("Susp a = "++).mkGETARG gc bf_pseq 0 t0.(";"++).nl) id.
  tab.("Susp b = "++).mkGETARG gc bf_pseq 1 t0.(";"++).nl.
  tab.("return b;"++).nl.
  ("}"++).nl

-- * Built-in operators

-- | C signature for big integer multiplication.
mulISig :: ShowS
mulISig = ("Susp "++).pprint CMulI.("(Susp a, Susp b)"++)

-- | C code for big integer multiplication.
b_mulI :: Options -> ShowS
b_mulI opts =
  mulISig.(" {"++).nl.
  wrapIfGMP
  (tab.("mpz_t *ret = (mpz_t *)"++).gmpMalloc opts "sizeof(mpz_t)".(";"++).nl.
   tab.("mpz_t *v1 = (mpz_t *)CPTR(a);"++).nl.
   tab.("mpz_t *v2 = (mpz_t *)CPTR(b);"++).nl.
   -- tab.("printf(\"Integer multiplication, addresses %p and %p -> %p\\n\", v1, v2, ret);"++).nl.
   tab.("mpz_init(*ret);"++).nl.
   tab.("mpz_mul(ret[0], v1[0], v2[0]);"++).nl.
   -- tab.gmpFree opts "v1[0]".(";"++).nl.     -- free the memory used by the
   -- tab.gmpFree opts "v2[0]".(";"++).nl.     -- two consumed numbers (unsafe)
   tab.("return (SUSP("++).dummyPre.(", "++).integerTag.(", (TP_)ret));"++).nl)
  (tab.("return (SUSP("++).dummyPre.(", "++).integerTag.(", 0));"++).nl).
  ("}"++).nl

-- * Built-in constructors

mkBuiltinCstr :: DTName -> CstrName -> ShowS
mkBuiltinCstr dt cstr =
  let (cid, nullary) =
        case Data.Map.lookup cstr builtinCIDs of
          Just (arC, cidC) -> (cidC, arC==0)
          Nothing -> ierr $ "mkBuiltinCstr: no info for "++(qName cstr)
      tag = shows (findTagOfDT dt builtinTags)
  in  ("FUNC("++).pprint cstr.("){"++).nl.
      ("return (SUSP("++).shows cid.(", "++).tag.
      (if nullary then (",  0));"++)
       else            (", T0));"++)).nl.
      ("}"++).nl

b_Cons :: ShowS ; b_Cons = mkBuiltinCstr dtList bf_Cons
b_Nil  :: ShowS ; b_Nil  = mkBuiltinCstr dtList bf_Nil
b_Unit :: ShowS ; b_Unit = mkBuiltinCstr dtUnit bf_Unit
b_Tuple:: Int -> ShowS
b_Tuple i = mkBuiltinCstr (dtTuple i) (bf_Tuple i)

builtinConstrsC :: ShowS
builtinConstrsC =
  b_Cons.
  b_Nil .
  b_Unit.
  foldDot b_Tuple b_tupleSizes
  
-- | Generates the declarations for the built-in constructors and their formals.
builtinConstrsDecls :: Options -> ShowS
builtinConstrsDecls opts =
  let gc = optGC opts
      extern = case optCMode opts of CompileModule -> ("extern "++) ; Whole -> id
      declTuple i =
        let tupleI = bf_Tuple i
            Just args = Data.Map.lookup tupleI builtinFuncSigs
            mkCVar (arg, idx) = mkDefineVar gc arg tupleI idx
        in  extern.declF opts (tupleI, (i, i, 0)).
            foldDot mkCVar (zip args [0..(length args)-1])
  in  extern.declF opts (bf_Cons, (2, 2, 0)).
      mkDefineVar gc bf_cons_0 bf_Cons 0.
      mkDefineVar gc bf_cons_1 bf_Cons 1.
      extern.declF opts (bf_Nil, (0, 0, 0)).
      extern.declF opts (bf_Unit, (0, 0, 0)).
      foldDot declTuple b_tupleSizes
      
-- | The (value arity, args arity, nesting) data for built-in functions. 
bfsLARInfo :: [(QName, (Int, Int, Int))]
bfsLARInfo = 
  let ar  v = length (frmsOf v builtinFuncSigs)
      pmd v = findPMDepth v builtinPmDepths
  in  map (\f -> (f, (ar f, ar f, pmd f))) cBuiltinFuncsC

prettyPrintList :: Bool -> ShowS
prettyPrintList compact =
  let Just (_, cidCons) = Data.Map.lookup bf_Cons builtinCIDs
      Just (_, cidNil ) = Data.Map.lookup bf_Nil  builtinCIDs
  in  pprinterSig dtList.(" {"++).nl.
      tab.("Susp comp1, comp2;"++).nl.
      tab.("if (CONSTR(i)=="++).shows cidNil.(") {"++).nl.
      tab.tab.("printf(\"[]\");"++).nl.
      tab.("}"++).nl.
      tab.("else if (CONSTR(i)=="++).shows cidCons.(") {"++).nl.
      tab.tab.("printf(\"[\");"++).nl.
      tab.tab.("while (1) {"++).nl.
      tab.tab.tab.("comp1 = "++).pprint bf_cons_0.("(CPTR(i));"++).nl.
      (if compact then
         tab.tab.tab.("if (IS_PVAL(comp1))"++).nl.
         tab.tab.tab.tab.("printf(\"%ld\", PVAL_R(comp1));"++).nl.
         tab.tab.tab.("else"++).nl.tab
       else id).
      tab.tab.tab.("printf(\"%d\", CONSTR(comp1));"++).nl.
      tab.tab.tab.("comp2 = "++).pprint bf_cons_1.("(CPTR(i));"++).nl.
      tab.tab.tab.("if (CONSTR(comp2)==1) break;"++).nl.
      tab.tab.tab.("printf(\",\");"++).nl.
      tab.tab.tab.("i = comp2;"++).nl.
      tab.tab.("}"++).nl.
      tab.tab.("printf(\"]\");"++).nl.
      tab.("}"++).nl.
      tab.("else { printf(\"Unknown list constructor: %d\\n\", CONSTR(i)); }"++).nl.
      tab.("return (SUSP(1, "++).uTag.(", 0));"++).nl.
      ("}"++).nl

prettyPrintUnit :: ShowS
prettyPrintUnit =
  let Just (_, cidUnit) = Data.Map.lookup bf_Unit builtinCIDs
  in  pprinterSig dtUnit.(" {"++).nl.
      tab.("if (CONSTR(i)=="++).shows cidUnit.(") {"++).nl.
      tab.tab.("printf(\"()\");"++).nl.
      tab.("} else {"++).nl.
      tab.tab.("printf(\"Unknown unit constructor: %d\\n\", CONSTR(i));"++).nl.
      tab.("}"++).nl.
      tab.("return (SUSP(1, "++).uTag.(", 0));"++).nl.
      ("}"++).nl

-- * Defunctionalization generated code

-- | A filter that ignores function definitions belonging to the pseudo-module
--   generated by defunctionalization. Used to remove closure dispatcher code
--   from separately compiled modules.
blockNotApp :: BlockL -> Bool
blockNotApp (DefL (QN (Just m) _) _ _) = m/=dfMod
blockNotApp (DefL (QN Nothing _) _ _) =
  ierr "blockNotApp: encountered unqualified top-level function"
blockNotApp _ = True

dtNotDF :: Data -> Bool
dtNotDF (Data dt _ _) = (dt /= dfDT)
