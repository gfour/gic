-- | Specialized macros for using lazy activation records (LARs).
-- 
--   We represent LARs in two ways: the \"optimized\" (which is the default) and
--   the \"simple\".
-- 
--   In the \"simple\" representation (@c/lar.h@), each LAR contains information
--   about its parent LAR, the arguments that have to be evaluated, space for
--   the memoized results (\"vals\") and pattern-matching values (\"nested\" 
--   values), and information about the function's arity and number of
--   pattern-matching clauses (\"pattern matching depth\").
--   This can be used to do accurate garbage collection (not yet implemented).
--     
--   In the \"optimized\" representation (@c/lar_opt.h@), the arities and
--   pattern matching depths of all functions are used to construct specialized
--   macros for access to their LAR. This means that no space is 
--   reserved for this information during runtime.
-- 
--   * In the single-threaded runtime, we do one more optimization: the arguments 
--     that have to be evaluated are embedded inside the \"vals\" fields, using
--     the @.ctxt@ field.
--     Since this field is always a pointer, its last bit is assumed to be 0 -- we
--     initially store there the pointer to the argument to be evaluated, with
--     the last bit set to 1. Therefore, if a value is not evaluated yet, its
--     @ctxt@ field will have its last bit set to 1; masking that out, we now
--     have the argument pointer to use for evaluation. When evaluation finishes
--     with a value, the structure is overwritten (including @ctxt@) with
--     it. This means that memoized values always have a valid pointer in @ctxt@
--     and can be collected by libgc.
-- 
--   * In the parallel runtime (when the C macro @USE_OMP@ is defined), we use the 
--     \"optimized\" representation, without the tagged-pointer embedding.
--     LAR arguments now occupy their own slot, together with a lock. This is
--     used to guarantee single evaluation of thunks, even in the presence of
--     concurrent access.
--     For garbage collection we use libgc configure for thread-local allocation
--     and parallel garbage collection.
-- 

module SLIC.LAR.SMacrosAux (declF, mkAllocAR, mkDefineVar,
                            mkGETARG, mkGETSTRICTARG, mkLARMacro,
                            mkLARMacroOpt, mkMainCall, mkNESTED, mkVALS,
                            nameGCAF, namegenv, protoFunc, smFun) where

import SLIC.AuxFun (insCommIfMore)
import SLIC.Constants (comma, nl, lparen, rparen, tab)
import SLIC.State (GC(LibGC, SemiGC), Options(optGC, optHeap))
import SLIC.Types (Arity, MName, PMDepth, QName, qName, mainDefQName, pprint)
import SLIC.LAR.LARAux (wrapIfOMP)

-- * LAR construction

-- | Produces uniform names for the arguments of a cpp macro. 
argsA :: Int -> [ShowS]
argsA n = map (\i->("arg"++).shows i) [1..n]

-- | Takes a function name and returns a string to be used
--   as part of the name of a (specialized for this function)
--   macro. For now, the string is just the initial
--   name, with all `$' signs substituted with `_'
asMacroPart :: String -> String
asMacroPart s = map (\c->if c=='$' then '_' else c) s

-- | Takes a name and returns a string to be used as the prefix of the name 
--   of a (specialized) macro. For now, the string is just the initial
--   name capitalized, prefixed with `__' and with all `$' signs 
--   substituted with `_'
asMacroPrefix :: String -> ShowS
asMacroPrefix name = ("__"++).((asMacroPart name)++).("_"++)

-- | Same as 'asMacroPrefix', for functions.
asMacroPrefixFunc :: QName -> ShowS
asMacroPrefixFunc f = asMacroPrefix $ qName f

-- | Takes a function name and the argument-arity, value-arity and
--   nesting of this function's LAR and produces all the specialized
--   macros associated with the function.
smFun :: Options -> QName -> Int -> Int -> Int -> ShowS
smFun opts f arityA arityV nesting =
  mkLARMacro opts (qName f) arityA arityV nesting

-- | Takes a name and the argument-arity, value-arity and
--   nesting of this function's LAR and produces all the specialized
--   macros associated with the function. This is currently used
--   to generate the LARs for functions (see 'smFun') and the LAR for
--   storing a module's CAFs.
mkLARMacro :: Options -> String -> Int -> Int -> Int -> ShowS
mkLARMacro opts name arityA arityV nesting =
  -- if the function LAR is going to be empty, ignore
  if (arityA==0) && (arityV==0) && (nesting==0) then
    id
  else case optGC opts of
         LibGC    -> mkLARMacroOpt opts name arityA arityV nesting
         SemiGC _ -> id

-- | Generates the macro for variable x of function f (stored in position n).
mkDefineVar :: GC -> QName -> QName -> Int -> ShowS
mkDefineVar gc x f n =
  ("#define " ++).pprint x.("(T0) "++).
  (case gc of
      LibGC    -> asMacroPrefixFunc f
      SemiGC _ -> id).
  ("GETARG("++).(shows n).(", T0)"++).nl

-- | The prototype of a function.
protoFunc :: QName -> ShowS
protoFunc v = ("FUNC("++).pprint v.(");"++).nl

-- | Generates the C function prototype and the LAR macros for a function
--   f with args arity aA, values arity vA, and nesting n.
declF :: Options -> (QName, (Arity, Arity, PMDepth)) -> ShowS
declF opts (f, (aA, vA, n)) = protoFunc f.smFun opts f aA vA n

-- | Generates the main call of the program. Takes the GC mode and the
--   name of the module that contains the main function (see 'mainDefQName').
mkMainCall :: GC -> MName -> ShowS
mkMainCall gc m =
  let mainDef = mainDefQName m
      resultCall =
        tab.("res = "++).((qName mainDef)++).
        (case gc of
          LibGC    -> ("("++).asMacroPrefixFunc mainDef.("AR());"++)
          SemiGC _ -> ("(t0);"++))
  in  wrapIfOMP
      -- ("#pragma omp parallel shared(T0)"++).nl.
      (("{"++).nl.
       -- ("#pragma omp single"++).nl.
       resultCall.nl.
       ("}"++).nl)
      (resultCall.nl)

-- | Construct a LAR for function calls. Takes the GC mode to use (this
--   affects LAR representation), a flag to indicate if the LAR is going
--   in the heap (if False, it goes in the stack), the function
--   name\/arity\/depth, and the string containing the arguments.
mkAllocAR :: GC -> Bool -> QName -> Arity -> PMDepth -> ShowS -> ShowS
mkAllocAR gc allocHeap f fArity fNesting argsS =
  let larConstr = if allocHeap then ("AR"++) else ("AR_S"++)
  in  (case gc of
          LibGC    -> asMacroPrefixFunc f.larConstr.lparen
          SemiGC _ ->
            larConstr.lparen.shows fArity.(", "++).shows fNesting.
            if fArity > 0 then (", "++) else id).
      argsS.rparen

-- * Accessors

-- | Generates a GETARG accessor for a function, at a LAR position.
--   Also takes a string representation of the context.
mkGETARG :: GC -> QName -> Int -> String -> ShowS
mkGETARG gc f i ctxt =
  (case gc of
      LibGC    -> asMacroPrefixFunc f
      SemiGC _ -> id).
  ("GETARG("++).shows i.(", "++).(ctxt++).(")"++)

-- | Generates a NESTED accessor for a function, at a nesting position.
mkNESTED :: GC -> QName -> Int -> ShowS
mkNESTED gc f i =
  (case gc of
      LibGC    -> asMacroPrefixFunc f
      SemiGC _ -> id).
  ("NESTED("++).shows i.(", T0)"++)

-- | Generates a GETSTRICTARG accessor for a function, at a LAR position.
mkGETSTRICTARG :: GC -> QName -> Int -> ShowS
mkGETSTRICTARG gc f i =
  (case gc of
      LibGC    -> asMacroPrefixFunc f
      SemiGC _ -> id).("GETSTRICTARG("++).shows i.(", T0)"++)

-- | Generates a VALS accessor for a LAR position (where the LAR has a specified
--   arity). Also takes a string representation of the context.
mkVALS :: GC -> Int -> Int -> String -> ShowS
mkVALS gc i argsNum ctxt =
  ("VALS("++).shows i.(", "++).
  (case gc of
      LibGC    -> shows argsNum.(", "++)
      SemiGC _ -> id).
  (ctxt++).(")"++)

-- * CAF construction

-- | The name of the GCAF macro.
nameGCAF :: MName -> ShowS
nameGCAF m = ("__GCAF_"++).((asMacroPart m)++)

-- | The name of the genv global LAR.
namegenv :: MName -> ShowS
namegenv m = ("__genv_"++).(m++)

-- * Optimized LAR representation

-- | Place the arguments in { ... }. This is used when they are used to initialize the 
--   first field of a struct, i.e. when LarArg is a struct (in the parallel runtime, 
--   'LarArg' values contain locks).
wrapInBraces :: ShowS -> ShowS
wrapInBraces s = ("{"++).s.("}"++)

-- | Embeds an argument (function pointer) directly in the context field of its value
--   (masking its last bit). This is used in the single-threaded runtime, to save space
--   for the LarArg[] fields.
embedArg :: ShowS -> ShowS
embedArg arg = ("{0, ARGC("++).arg.(")}"++)

-- | Used by 'mkLARMacro' to generate function-specific LAR macros.  
mkLARMacroOpt :: Options -> String -> Int -> Int -> Int -> ShowS
mkLARMacroOpt opts name arityA arityV nesting =
  let initArg :: (Int, ShowS) -> ShowS
      initArg (n, arg) = 
        ("      ARGS("++).shows n.(", lar) = ARGC("++).arg.(")"++).
        (";                               \\"++).nl
      fnameM = asMacroPrefix name
      -- generates the AR_S constructor; the flag controls if LarArg is a struct or a pointer
      mkAR_S argWrapper =
        ("#define "++).fnameM.("AR_S"++).
        lparen.insCommIfMore (argsA arityA).rparen.
        ("                   \\"++).nl.
        ("  ((TP_) &((LAR_STRUCT"++).
        lparen.shows arityA.comma.shows arityV.comma.shows nesting.rparen.rparen.
        ("             \\"++).nl.
        (if arityA == 0 then 
           ("   { T0 "++)
         else
           ("   { T0, {"++).insCommIfMore (map argWrapper $ argsA arityA).
           ("}"++)).
        ("}))"++).nl
  in  ("#define "++).fnameM.("GETARG(x, T)        GETARG(x, "++).
      shows arityA. (", T)"++).nl.
      ("#define "++).fnameM.("GETSTRICTARG(x, T)  GETSTRICTARG(x, "++).
      shows arityA. (", T)"++).nl.
      ("#define "++).fnameM. ("NESTED(x, T)        NESTED(x, "++). 
      shows arityA. (", "++). shows arityV. (", T)"++). nl.
      (if optHeap opts then id
       else
         wrapIfOMP
         -- stack AR, parallel runtime, LarArg is a struct
         (mkAR_S wrapInBraces)
         -- stack AR, single-threaded runtime, LarArg is a pointer
         (mkAR_S embedArg)
      ).
      ("#define "++).fnameM.("AR"++).
      lparen.insCommIfMore (argsA arityA).rparen.
      (" ({ \\"++).nl.
      ("      TP_ lar = (TP_) GC_MALLOC(sizeof(T_) +             \\"++).nl.
      ("                               "++).
      ("ZEROIFSEQ("++).shows arityA.(" * sizeof(LarArg)) +      \\"++).nl.
      ("                               "++).
      shows arityV.(" * sizeof(Susp) +        \\"++).nl.
      ("                               "++).
      shows nesting.(" * sizeof(TP_));         \\"++).nl.
      ("      lar->prev = T0;                                    \\"++).nl.
      (foldl (.) id (map initArg (zip ([0..]) (argsA arityA)))).
      ("      lar;                                               \\"++).nl.
      ("    })"++).nl
