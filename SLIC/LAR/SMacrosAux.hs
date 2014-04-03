-- | Specialized macros for using lazy activation records (LARs).
-- 
--   There are three LAR representations that can be used by the runtime:
--   the \"simple\", the \"optimized\" (which is the default), and the \"compact\".
-- 
--   In the \"simple\" representation (@c/lar.h@), each LAR contains information
--   about its parent LAR (/pev/), the arguments that have to be evaluated, space
--   for memoized results (/vals/) and pattern-matching values (/nested/values),
--   and information about the function's /arity/ and number of
--   pattern-matching clauses (/pattern matching depth/).
--   This information describes the structure of each LAR in memory and can 
--   be used to do accurate garbage collection.
--     
--   In the \"optimized\" representation (@c/lar_opt.h@), the arities and
--   pattern matching depths of all functions are used to construct specialized
--   macros for LAR construction and access. This means that no space is 
--   reserved for this information during runtime.
-- 
--   * In the single-threaded runtime, we do one more optimization: the arguments 
--     that have to be evaluated are embedded inside the /vals/, using
--     the @.ctxt@ field.
--     Since this field is always a pointer, its last bit is assumed to be 0 due
--     to alignment assumptions on IA-32 and x86-64 platforms. We can therefore
--     use the pointer field as a union of two different data types, with the
--     last bit discriminating between the two. We initially store in the field
--     the pointer to the argument to be evaluated, with the last bit set to 1.
--     This means that if a value is not evaluated yet, its
--     @ctxt@ field will have its last bit set to 1; masking that out, we now
--     have the argument pointer to use for evaluation. When evaluation finishes
--     with a value, the structure is overwritten (including @ctxt@) with
--     it. Since @ctxt@ is a pointer again (or 0 for nullary constructors), the
--     last bit will now be 0.
--     In this way, memoized values always have a valid pointer in @ctxt@
--     and can be collected by conservative garbage collection such as libgc.
-- 
--   * In the parallel runtime (when the C macro @USE_OMP@ is defined), we use the 
--     \"optimized\" representation, without the tagged-pointer embedding.
--     LAR arguments now occupy their own slot, paired with a lock. This lock is
--     used to guarantee single evaluation of thunks, even in the presence of
--     concurrent access.
--     For garbage collection we use libgc configured for thread-local allocation
--     and parallel garbage collection (see
--     <http://www.hpl.hp.com/personal/Hans_Boehm/gc/scale.html> for details).
-- 
--   In the \"compact\" representation (@c/lar_compact.h@), details of the x86-64
--   ABI are used to fuse together both a LAR argument and the resulting thunk 
--   constructor and context into a singe machine word. The arity and nesting
--   fields of the LAR are also fused with the /prev/ pointer, and
--   built-in nullary data types (Int, Bool) are unboxed.
--   This is an experimental representation that should be combined with accurate
--   garbage collection (such as our semi-space collector).
--   This representation currently does not support parallel evaluation and
--   data type tagging (see "SLIC.Tags").
--   Because of defunctionalization, this technique is applicable to small programs
--   (having at most 2^16 syntactically different partial applications in the
--   whole program text).
-- 

module SLIC.LAR.SMacrosAux (declF, mkAllocAR, mkDefineVar, mkGETARG,
                            mkGETSTRICTARG, mkLARMacro, mkPUSHAR, mkRETVAL,
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
         LibGC  -> mkLARMacroOpt opts name arityA arityV nesting
         SemiGC -> id

-- | Generates the macro for variable x of function f (stored in position n).
mkDefineVar :: GC -> QName -> QName -> Int -> ShowS
mkDefineVar gc x f n =
  ("#define " ++).pprint x.("(T0) "++).
  (case gc of
      LibGC  -> asMacroPrefixFunc f
      SemiGC -> id).  
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
          LibGC  -> ("("++).asMacroPrefixFunc mainDef.("AR());"++)
          SemiGC -> ("(t0);"++))
  in  wrapIfOMP
      (("#pragma omp single"++).nl.
       ("{"++).nl.
       -- ("#pragma omp parallel shared(T0)"++).nl.
       resultCall.nl.
       ("}"++).nl)
      (resultCall.nl)

-- | Construct a LAR for function calls. Takes the GC mode to use (this
--   affects LAR representation), a flag to indicate if the LAR is going
--   in the heap (if False, it goes in the stack), a flag to indicate if
--   the LAR is using the compact representation, the function
--   name\/arity\/depth, and the argument representations.
mkAllocAR :: GC -> Bool -> Bool -> QName -> Arity -> PMDepth -> [ShowS] -> ShowS
mkAllocAR gc allocHeap compact f fArity fNesting args =
  let larConstr = if allocHeap then ("AR"++) else ("AR_S"++)
      args'     = if (compact && (not allocHeap)) then
                     map (\a->("ARGC("++).a.(")"++)) args
                  else args
      argsS     = insCommIfMore args'
  in  (case gc of
          LibGC  -> asMacroPrefixFunc f.larConstr.lparen
          SemiGC ->
            -- LAR constructors take extra "arity, nesting, ..." parameters.
            larConstr.lparen.shows fArity.(", "++).shows fNesting.
            -- If function arguments follow, add comma.
            if fArity > 0 then (", "++) else id).
      argsS.rparen

-- * Accessors

-- | Generates a GETARG accessor for a function, at a LAR position.
--   Also takes a string representation of the context.
mkGETARG :: GC -> QName -> Int -> String -> ShowS
mkGETARG gc f i ctxt =
  (case gc of
      LibGC  -> asMacroPrefixFunc f
      SemiGC -> id).
  ("GETARG("++).shows i.(", "++).(ctxt++).(")"++)

-- | Generates a NESTED accessor for a function, at a nesting position. Also
--   takes the arity of the enclosing function.
mkNESTED :: GC -> Bool -> QName -> Int -> Arity -> ShowS
mkNESTED gc compact f i argsN =
  (case gc of
      LibGC  -> asMacroPrefixFunc f
      SemiGC -> id).
  (if compact then
     ("NESTED("++).shows i.(", "++).shows argsN.(", T0)"++)
   else
     ("NESTED("++).shows i.(", T0)"++))

-- | Generates a GETSTRICTARG accessor for a function, at a LAR position.
mkGETSTRICTARG :: GC -> QName -> Int -> ShowS
mkGETSTRICTARG gc f i =
  (case gc of
      LibGC  -> asMacroPrefixFunc f
      SemiGC -> id).("GETSTRICTARG("++).shows i.(", T0)"++)

-- | Generates a VALS accessor for a LAR position (where the LAR has a specified
--   arity). Also takes a string representation of the context.
mkVALS :: GC -> Int -> Int -> String -> ShowS
mkVALS gc i argsNum ctxt =
  ("VALS("++).shows i.(", "++).
  (case gc of
      LibGC  -> shows argsNum.(", "++)
      SemiGC -> id).
  (ctxt++).(")"++)

-- * CAF construction

-- | The name of the GCAF macro.
nameGCAF :: MName -> ShowS
nameGCAF m = ("__GCAF_"++).((asMacroPart m)++)

-- | The name of the genv global LAR.
namegenv :: MName -> ShowS
namegenv m = ("__genv_"++).(m++)

-- * Optimized LAR representation

-- | Place the arguments in { ... }. This is used when they are used to initialize 
--   the first field of a struct, i.e. when LarArg is a struct (in the parallel
--   runtime, 'LarArg' values contain locks).
wrapInBraces :: ShowS -> ShowS
wrapInBraces s = ("{"++).s.("}"++)

-- | Embeds an argument (function pointer) directly in the context field of its
--   value (masking its last bit). This is used in the single-threaded runtime,
--   to save space for the LarArg[] fields.
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
      -- generates the AR_S constructor; the flag controls if LarArg is a struct
      -- or a pointer
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
      (let -- stack AR, single-threaded runtime, LarArg is a pointer
          compactAR_S = mkAR_S embedArg
          -- stack AR, parallel runtime, LarArg is a struct
          bigAR_S = mkAR_S wrapInBraces
       in  if optHeap opts then id
           else wrapIfOMP bigAR_S compactAR_S).
      ("#define "++).fnameM.("AR"++).
      lparen.insCommIfMore (argsA arityA).rparen.
      (" ({ \\"++).nl.
      ("      TP_ lar = (TP_) GC_MALLOC(sizeof(T_) +             \\"++).nl.
      ("                               "++).
      ("ZEROIFTAG("++).shows arityA.(" * sizeof(LarArg)) +      \\"++).nl.
      ("                               "++).
      shows arityV.(" * sizeof(Susp) +        \\"++).nl.
      ("                               "++).
      shows nesting.(" * sizeof(TP_));         \\"++).nl.
      ("      lar->prev = T0;                                    \\"++).nl.
      (foldl (.) id (map initArg (zip ([0..]) (argsA arityA)))).
      ("      lar;                                               \\"++).nl.
      ("    })"++).nl

-- | Generates the PUSHAR macro that pushes a LAR pointer to the pointer stack.
mkPUSHAR :: Bool -> ShowS
mkPUSHAR dbg =
  let debug_PUSHAR =
        if dbg then
          ("if (sstack_ptr >= sstack_bottom + SSTACK_MAX_SIZE) { printf(\"Pointer stack overflow.\\n\"); exit(EXIT_FAILURE); } ; "++).
          ("printf(\"push sstack_ptr := %p -> \", sstack_ptr); "++).
          -- Use a temporary to escape multiple unfolding of the same macro arg.
          ("TP_ "++).tmp_a.(" = a; "++).
          ("DEBUG_PRINT_AR("++).tmp_a.("); "++).
          ("printf(\"\\n\"); "++)
        else id
      larVar = if dbg then tmp_a else id
      tmp_a  = ("tmp_a"++)
  in  ("// Record LAR pointer in the explicit pointer stack."++).nl.
      ("#define PUSHAR(a) ((TP_*)({ "++).debug_PUSHAR.
      ("*sstack_ptr = "++).larVar.("; sstack_ptr++; }))"++).nl

-- | Generates the RETVAL macro that pops the pointer stack.
mkRETVAL :: Bool -> ShowS
mkRETVAL dbg =
  let debug_RETVAL =
        if dbg then ("printf(\"pop sstack_ptr := %p\\n\", sstack_ptr);"++) else id
  in  ("// get call result and pop activation record"++).nl.
      ("#define RETVAL(x) ((Susp)({ Susp r = (x); sstack_ptr--;"++).
      debug_RETVAL.
      (" r; }))"++).nl