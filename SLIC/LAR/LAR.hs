-- | Implementation with lazy activation records (LARs), C back-end.
-- 
-- The main module that compiles the LAR language to C code.
-- 
-- Memory allocation:
-- 
--   (1) The stack is used for C function calls (trivially) and calls that
--       escape analysis shows they can allocate their LAR on the stack.
-- 
--   (2) A second memory area is allocated in the heap for the rest of the
--       activation records.
-- 

module SLIC.LAR.LAR (compileModL, createSemiGCARInfra, 
                     declarationsBuiltins, epilogue,
                     genInitMod, headersC, macrosC, mainFunc, makeC,
                     prologue) where

import Data.List (nub)
import Data.Map (elems, filter, lookup, keys, toList)
import Data.Set (toList)
import SLIC.AuxFun (foldDot, ierr, insCommIfMore, pathOf)
import SLIC.Constants
import SLIC.DFI (DFC(dfcN), DFI(dfiDfInfo), DfInfo(diDfcs, diEApps),
                 ExtAppFuns, dfiFile)
import SLIC.Front.CAF
import SLIC.Front.Defunc (genNApp)
import SLIC.LAR.LARAux
import SLIC.LAR.LARBuiltins
import SLIC.LAR.LARGraph
import SLIC.LAR.SMacrosAux (declF, nameGCAF, 
                            namegenv, protoFunc, mkAllocAR, mkDefineVar, 
                            mkGETSTRICTARG, mkLARMacro, mkLARMacroOpt,
                            mkNESTED, mkMainCall, mkVALS, smFun)
import SLIC.LAR.SyntaxLAR
import SLIC.State
import SLIC.SyntaxAux
import SLIC.SyntaxFL
import SLIC.Tags
import SLIC.Types

-- | Compiles a program to C, given the variable types and the compilation flags.
--   The constructor enumeration is also used for the pretty printer.
--   A LAR module is given, to configure aspects of the runtime.
makeC :: ProgL -> TEnv -> ConfigLAR -> (DFI, ImportedNames, CIDs) -> ShowS
makeC (Prog dTypes defs) env config (dfi, imports, extCIDs) =
    let strictVars  = getStricts config
        cbns        = getCBNVars config
        arities     = getArities config
        pmDepths    = getPMDepths config
        cids        = getCIDs config
        importFuns  = keys imports
        opts        = getOptions config
        gc          = optGC opts        
        modName     = getModName config
        cMode       = optCMode opts
        arityCAF    = length (getCAFnmsids config)
        defs'    = case cMode of
          Whole -> defs
          CompileModule ->
            -- remove external constructors and functions (this includes closure
            -- constructors and defunctionalization functions, they should be
            -- stored separately in the DFI)
            Prelude.filter (blockIsLocal modName) defs
    in  headersC opts.
        macrosC opts modName arities pmDepths arityCAF.
        prologue opts modName arityCAF.
        predeclarations defs' config.
        pdeclGCAF config arityCAF.nl.
        declarations dTypes defs'.
        argDefs defs env strictVars cbns gc.nl.
        declarationsBuiltins opts.nl.
        (case cMode of
            Whole -> id
            CompileModule ->
              (case gc of
                  SemiGC -> id
                  LibGC  ->
                    pdeclExts opts imports.
                    pdeclExtApps opts (diEApps $ dfiDfInfo dfi)).
              defInterface gc (dfiDfInfo dfi) importFuns extCIDs.nl).
        genv modName arityCAF.nl.                    -- module CAF
        initMod modName config.nl.                   -- module initializer
        (case cMode of
            Whole -> mainFunc env opts (depthOfMainDef defs) [modName].nl
            CompileModule -> id).
        mainProg defs' env config.
        (case cMode of
            Whole -> prettyPrintersC.epilogue opts
            CompileModule -> id).
        prettyPrintersFor dTypes cids.nl

-- | Tests if a block is local. Non-local functions are those from another module 
--   (so far only the closure dispatchers of defunctionalization satisfy this
--   condition).
blockIsLocal :: MName -> BlockL -> Bool
blockIsLocal m (DefL f@(QN (Just m') _) _ _) =
  if m==m' then True               -- local function
  else if m' == dfMod then False   -- defunctionalization function
  else ierr $ "Found non-local function "++(qName f)
blockIsLocal _ (ActualL _ _ _) = True
blockIsLocal _ _ = ierr "blockIsLocal: found unqualified block definition"

-- | The C headers of the generated code.
headersC :: Options -> ShowS
headersC opts =
  let gc   = optGC opts
  in  (case gc of
          LibGC  ->
            ("#include \"c/lar_opt.h\""++).nl.
            ("#include \"gc.h\""++).nl
          SemiGC ->
            (if optCompact opts then
               ("#include \"c/lar_compact.h\""++).nl
             else
               ("#include \"c/lar.h\""++).nl).
            ("#include \"c/gc.h\""++).nl).nl.
      ("#include <c/gic_builtins.h>"++).nl.
      wrapIfGMP (("#include <gmp.h>"++).nl) id.nl
      
-- | The C macros of the generated code.
macrosC :: Options -> MName -> Arities -> PMDepths -> Int -> ShowS
macrosC opts modName arities pmDepths arityCAF =
  let gc   = optGC opts
      co   = optCompact opts
  in  ("// Macros"++).nl.nl.
      (if optTag opts then
          ("#ifndef USE_TAGS"++).nl.
          ("#error you must enable the USE_TAGS macro for tags to work"++).nl. 
          ("#endif /* USE_TAGS */"++).nl
       else id).
      wrapIfSSTACK
      -- Use a shadow stack for function calls (use with the semi-space collector).
      (("// shadow stack maximum size"++).nl.
       ("#define SSTACK_MAX_SIZE 100000000"++).nl.
       ("// push activation record to shadow stack"++).nl.
       (if optDebug opts then
          ("#define PUSHAR(a) ({ if (sstack_ptr >= sstack_bottom + SSTACK_MAX_SIZE) { printf(\"Shadow stack overflow.\\n\"); exit(EXIT_FAILURE); } ; *sstack_ptr++ = a; })"++).nl
        else
          ("#define PUSHAR(a) (*sstack_ptr++ = a)"++).nl).
       ("// get call result and pop activation record"++).nl.
       ("#define RETVAL(x) ((Susp)({ Susp r = (x); sstack_ptr--; r; }))"++).nl)
      -- No shadow stack, dummy macros (use with libgc).
      (("#define PUSHAR(a) a"++).nl.
       ("#define RETVAL(x) x"++).nl).
      (case gc of
          SemiGC -> ("#define GC_MALLOC MM_alloc"++)
          LibGC  -> id).nl.
      defineGCAF modName gc arityCAF.nl.
      (case gc of
          LibGC  -> createLibGCARInfra opts modName pmDepths.nl
          SemiGC ->
            createSemiGCARInfra modName gc co arities pmDepths arityCAF.nl).nl

-- | Create the necessary macros for handling the optimized LARs. To be used
--   by the libgc garbage collector.
createLibGCARInfra :: Options -> MName -> PMDepths -> ShowS
createLibGCARInfra opts m pmds =
  -- add libgc handlers for the GMP library
  wrapIfGMP
  (("void *GMP_GC_malloc(size_t sz) { return GC_malloc(sz); }"++).nl.
   ("void *GMP_GC_realloc(void *p, size_t old, size_t new) { return GC_realloc(p, new); }"++).nl.
   ("void GMP_GC_free(void *p, size_t sz) { GC_free(p); }"++).nl) id.
  mkMainAR opts m pmds.nl

mkMainAR :: Options -> MName -> PMDepths -> ShowS
mkMainAR opts m pmds =
  let mainDef = mainDefQName m
  in  case Data.Map.lookup mainDef pmds of
        Just mainDepth ->
          mkLARMacroOpt opts (qName mainDef) 0 0 mainDepth
        _ -> id
  
-- | Create all possible activation record shapes (to be allocated in the heap).
--   Not to be used with the optimized LARs (that omit the size fields).
createSemiGCARInfra :: MName -> GC -> Bool -> Arities -> PMDepths -> Int -> ShowS
createSemiGCARInfra m gc compact fArities pmDepths arityCAF =
  let arities     = nub $ elems fArities
      nestings    = nub $ elems pmDepths
      maxArity    = maximum (arityCAF:arities)
      minNesting  =
        case Data.Map.lookup (mainDefQName m) pmDepths of
          Just mainN -> mainN                        -- default nesting of main
          Nothing    -> 0                            -- default max for [] is 0
      maxNestings = maximum (minNesting:nestings)        
      hAR_COPY 0  = ("#define AR_COPY_0(lar, n) do { } while(0)"++).nl
      hAR_COPY a  =
        ("#define AR_COPY_"++).shows a.("(lar, n, a0, ...) do {         \\"++).nl.
        tab.tab.("ARGS(n, lar) = ARGC(a0);                    \\"++).nl.
        tab.tab.("AR_COPY_"++).shows (a-1).("(lar, n+1, ## __VA_ARGS__);        \\"++).nl.
        tab.("} while(0)"++).nl
      -- AR_CLEAR, simple representation
      hAR_CLEAR 0 = ("#define AR_CLEAR_0(lar, n) do { } while(0)"++).nl
      hAR_CLEAR d =
        ("#define AR_CLEAR_"++).shows d.("(lar, n) do {            \\"++).nl.
        (case gc of
            SemiGC -> tab.tab.("NESTED(n, lar) = NULL;                \\"++).nl
            LibGC  -> id).
        tab.tab.("AR_CLEAR_"++).shows (d-1).("(lar, n+1);                 \\"++).nl.
        tab.tab.("} while(0)"++).nl
      -- AR_CLEAR, compact representation
      hAR_CLEARc 0 = ("#define AR_CLEAR_0(lar, ar, n) do { } while(0)"++).nl
      hAR_CLEARc d =
        ("#define AR_CLEAR_"++).shows d.("(lar, ar, n) do {            \\"++).nl.
        (case gc of
            SemiGC -> tab.tab.("NESTED(n, ar, lar) = NULL;                \\"++).nl
            LibGC  -> id).
        tab.tab.("AR_CLEAR_"++).shows (d-1).("(lar, ar, n+1);           \\"++).nl.
        tab.tab.("} while(0)"++).nl
  in  if maxArity > maxLARArity then
        error $ "Maximum function/LAR arity exceeded: "++(show maxArity)++", maximum is "++(show maxLARArity)
        else if maxNestings > maxNestedLARs then
               error $ "Maximum function/LAR nesting exceeded: "++(show maxNestings)++", maximum is "++(show maxNestedLARs)
             else                    
               (foldDot hAR_COPY [0..maxArity]).nl.
               (if compact then
                  foldDot hAR_CLEARc [0..maxNestings]
                else
                  foldDot hAR_CLEAR [0..maxNestings]).nl

-- | Generates the "extern" declarations that link to the defunctionalization
--   interface and to functions in other modules. The CIDs table given is used
--   to generate declarations for bound variables from other modules.
defInterface :: GC -> DfInfo -> [QName] -> CIDs -> ShowS
defInterface gc dfInfo importFuns extCIDs = 
  let externF v   = ("extern "++).protoFunc v
      -- generate the constructor variables accessing macros
      macrosConstr (c, (_, ar)) =
        let bvs     = cArgsC c ar
            stricts = []   -- TODO: stricts information for imported constructors
            cbns    = []   -- bound variables are never call-by-name
        in  foldDot (protoF gc stricts cbns c) (enumNames bvs)
      extConstrs = map dfcN $ Data.Set.toList $ diDfcs dfInfo
  in  foldDot externF extConstrs.
      foldDot externF (map genNApp $ Data.Set.toList $ diEApps dfInfo).
      foldDot externF importFuns.
      foldDot macrosConstr (Data.Map.toList extCIDs)

-- | Generates specialized macros for LARs used by the functions defined
--   in the current module
--   * this is not to be used with -semiGC
predeclarations :: [BlockL] -> ConfigLAR -> ShowS
predeclarations lblockList config =
    let opts        = getOptions config
        predeclF (DefL fname _ bind) =
            let arityA  = length bind
                arityV  = arityA
                nesting = findPMDepthSafe fname (getPMDepths config)
            in  smFun opts fname arityA arityV nesting
        predeclF (ActualL _ _ _) = ierr "predeclF: found actual"
        lblockListF = Prelude.filter isFun lblockList
    in  foldDot predeclF lblockListF.nl

-- | Generates specialized macros for LARs used by the imported functions.
pdeclExts :: Options -> ImportedNames -> ShowS
pdeclExts opts imports =
    let -- extConstrs = mkDfConstrs opts dfcs
        -- extApps    = mkApplyFuns opts dfcs
        -- predeclE nst (DefF vname frml _) = 
        --     let fArity = length frml
        --     in  smFun (qName vname) fArity fArity nst
        -- predeclEC = predeclE 0
        -- predeclED = predeclE 1
        imports' = Data.Map.filter (\iinfo->(impC iinfo)/=NDType) imports
        predeclEF fname =
            let (Just iinfo)    = Data.Map.lookup fname imports
                (Just nesting)  = impD iinfo
                Just fArity     = impA iinfo
            in smFun opts fname fArity fArity nesting
        pdecls = foldDot predeclEF (Data.Map.keys imports').nl
    in  case optGC opts of
          SemiGC -> ierr "pdeclExts: not to be used with semiGC"
          LibGC  -> pdecls

-- | Generates specialized macros for LARs used by dispatchers 
--   for (unknown) external constructors
pdeclExtApps :: Options -> ExtAppFuns -> ShowS
pdeclExtApps opts extApps =
  let mkSmFun ar = smFun opts (genNApp ar) (1+ar) (1+ar) 1
  in  foldDot mkSmFun $ Data.Set.toList extApps

-- | Generates specialized macros for a module's global CAF.
pdeclGCAF :: ConfigLAR -> Int -> ShowS
pdeclGCAF config arityCAF =
    let m       = getModName config
        opts    = getOptions config
    in  case (optGC $ getOptions config) of
          SemiGC -> id
          LibGC  -> mkLARMacro opts ("GCAF_"++m) arityCAF arityCAF 0

-- | Generates the C declarations of the LAR blocks and the forcing
--   functions for the module data types.
declarations :: [Data] -> [BlockL] -> ShowS
declarations dts ds =
    let proto (DefL n _ _) = protoFunc n
        proto (ActualL  n _ _) = protoVar (qName n)
        idDefs = foldDot (\x -> proto x) ds
        protoDT d = pprinterSig d.(";"++).nl
        forcingFuncs = foldDot (\(Data d _ _)->protoDT d) dts.
                       foldDot protoDT builtinDTypes
    in  idDefs.forcingFuncs

-- | The prototype of a formal variable.
protoVar :: String -> ShowS
protoVar v = ("VAR("++).(v++).(");"++).nl

-- | Generates the C declarations of the native functions of the run-time.
declarationsBuiltins :: Options -> ShowS
declarationsBuiltins opts =
  foldDot (declF opts) bfsLARInfo.nl.
  builtinConstrsDecls opts.nl
  -- builtinTCs opts.nl

-- | Generates the C code for each LAR block.
mainProg :: [BlockL] -> TEnv -> ConfigLAR -> ShowS
mainProg ds env config = foldDot (\x -> (mkCBlock x env config).nl) ds

-- | Generates auxiliary functions that go in the end of
--   the generated C file (pretty printers, forcing 
--   functions, built-in functions, memory management code).
epilogue :: Options -> ShowS
epilogue opts = builtins opts.nl

-- | Generates a debugging prologue before each function body.
debugFuncProlog :: QName -> ShowS
debugFuncProlog f =
  -- verify that the LAR on the stack is the same as the current LAR visible
  wrapIfSSTACK
  (("printf(\"Entered func "++).pprint f.
   ("@%p(sstack-verified: %s)\\n\", *(sstack_ptr-1), (*(sstack_ptr-1)==T0? \"true\": \"false\"));"++).nl) id
  
-- | Generates C code for a block.
mkCBlock :: BlockL -> TEnv -> ConfigLAR -> ShowS
mkCBlock (DefL f e bind) env config =
  let fArity = length bind
      opts = getOptions config
      gc = optGC opts
  in  ("FUNC("++).pprint f.("){"++).nl.
      (if (fArity>0) && (gc==LibGC) then
         ("INIT_ARG_LOCKS("++).shows fArity.(");"++).nl.
         (if optDebug opts then
            debugFuncProlog f
          else id)
       else id).
      (case Data.Map.lookup f (getStricts config) of 
          Nothing -> id
          Just strictFrms -> forceStricts gc strictFrms fArity).
      logPrev opts.
      (mkCFuncBody config env f e).
      ("}"++).nl
mkCBlock (ActualL v act e) env config =
  ("VAR("++).pprint v.("){"++).nl.
  (mkAct act (getOptions config)).
  ("return "++).(mkCExp env config e).semi.nl.
  ("}"++).nl

-- | Generate C code for a function body.
mkCFuncBody :: ConfigLAR -> TEnv -> QName -> ExprL -> ShowS
mkCFuncBody config env f e =
  let -- the number of nested pattern matching clauses
      pmds = getPMDepths config
      patD  = case Data.Map.lookup f pmds of
                Just d -> d
                Nothing -> ierr $ "Function "++(qName f)++" has no depth in:\n"++(pprintPD pmds "")
      patDS = showsDep $ Just patD
  in  -- keep the nested pointers in the heap if the function is data,
      -- else keep it in the stack (important for GC)
      -- allocate space for patDepth * suspensions
      (if patD > maxNestedLARs then
         error $ "Too deep pattern matching detected: "++(show patD)++", maximum depth allowed is "++(show maxNestedLARs)
       else id).
      (if patD > 0 then tab.("Susp cl["++).patDS.("];"++).nl else id).
      (mkCStmBody e env config)

-- | Forces the strict parameters of a function call. It only works for ground 
--   values, otherwise will just force until the constructor head.
forceStricts :: GC -> StrictInds -> Arity -> ShowS
forceStricts gc strictInds fArity =
  let aux x =
        mkVALS gc x fArity "T0".
        (" = ARGS_FUNC("++).shows x.(", T0)(T0); // strict "++).nl
  in  foldDot aux strictInds

-- | Generates the C code for an expression that is assumed to be the body
--   of a definition.
mkCStmBody :: ExprL -> TEnv -> ConfigLAR -> ShowS
mkCStmBody e@(CaseL _ _ _) env config =
  tab.("Susp Res;"++).nl.
  -- evaluate the pattern matching clause expression
  mkCExp env config e.nl.
  ("return Res;"++).nl
-- For thunk constructors, the constructor id is returned paired with the
-- current context (for nullary constructors the context is 0).
mkCStmBody (ConstrL (CC c cId cArity)) _ config =
  let opts = getOptions config
  in  logConstr opts c.
      -- keep the context if the constructor is not nullary (or when debugging)
      (if (cArity>0) || optDebug opts then
         ("return (SUSP("++).shows cId.(", "++).uTag.(", T0));"++).nl
       else
         ("return (SUSP("++).shows cId.(", "++).uTag.(", (TP_)NULL));"++)).nl
mkCStmBody e env config = ("return "++).(mkCExp env config e).semi.nl

-- | Generates C code for a LAR expression. Takes the name of the containing
--   function (for custom LAR access), the typing environment, the
--   configuration, and the LAR expression.
mkCExp :: TEnv -> ConfigLAR -> ExprL -> ShowS
mkCExp env config (LARC (CN c) exps) =
  let compact = optCompact $ getOptions config
  in  case c of
        CIf  -> ("(PRIMVAL_R("++).(mkCExp env config (exps !! 0) ).(")?"++).
                 ("("++).(mkCExp env config (exps !! 1)).("):"++).
                 ("("++).(mkCExp env config (exps !! 2)).("))"++)
        c' | c' `elem` [ CMinus, CPlus, CMult, CDivide, CEqu, CLe, CGe  
                       , CGt, CLt, CAnd, COr, CMulI, CNEq, CMod, CDiv] ->
          mkBinOp c' exps env config
        CNeg -> 
          ("PRIMVAL_C(-(PRIMVAL_R("++).(mkCExp env config (exps !! 0)).
          ("))"++).mIntTag config.(")"++)
        CTrue  -> intSusp compact "True"
        CFalse -> intSusp compact "False"
        _      -> error $ "mkCExp: unknown built-in constant "++(pprint c "")
mkCExp _ config (LARC (LitInt i) exps) =
  case exps of
    []    -> intSusp (optCompact $ getOptions config) (show i)
    (_:_) -> ierr "Integer literal applied to expressions."
mkCExp env config (LARCall n acts) = 
  if elem n (nmsids2nms (getCAFnmsids config)) then
    let Just n' = (getCAFid n (getCAFnmsids config))
    in  ("("++).nameGCAF (getModName config).(("("++(show n')++"))")++)
  else makeActs n acts env config
mkCExp env config (CaseL (d@(Just depth), efunc) e pats) =
  let matchedExpr = mkCExp env config e
      cases       = foldDot mkCPat pats      
      opts        = getOptions config
      dS          = shows depth
      defaultCase = tab.("default: printf(\"Pattern matching on "++).pprint e.
                    (" failed: constructor %d encountered.\\n\", CONSTR(cl["++).
                    dS.("])); exit(0);"++)
      -- | Generates C code for a pattern. /case/ bodies are contained
      --   in {...} as they may contain declarations.
      mkCPat (PatL (CC c cId _) eP bindsVars) =
        tab.("case "++).shows cId.(": { /* "++).pprint c.(" */ "++).
        mkPatBody eP bindsVars.
        ("; break; }"++).nl
      mkPatBody ePB _ =
        -- let opts = getOptions config
        -- in  -- if explicit deallocation is enabled and the constructor
        --     -- does not bind any variables, ignore the nested context
        --     -- TODO: remove
        --     {-
        --     (if (stGC opts) && (not bindsVars) then
        --        id
        --        -- TODO: enable this and see if it makes any difference, or
        --        -- make it accessible by some command line switch
        --        -- ("NESTED("++).(shows d).(", T0) = 0; "++)
        --      else id). 
        --      -}
        (case ePB of
            CaseL _ _ _ -> id
            _           -> ("Res = "++)).
        mkCExp env config ePB
      compact = optCompact opts
      argsN = getFuncArity efunc (getArities config)
  in  tab.("cl["++).dS.("] = "++).matchedExpr.semi.nl.
      -- TODO: eliminate this when all patterns are nullary constructors
      -- (or are used as such, see 'bindsVars')
      tab.mkNESTED (optGC opts) compact efunc depth argsN.(" = GETTPTR(cl["++).dS.("].ctxt);"++).nl.
      logDict opts d.
      -- if debug mode is off, optimize away constructor choice when there is
      -- only one pattern (will segfault/misbehave if the constructor
      -- reached is missing)
      (if length pats == 0 then
         -- pattern matching without any patterns, does not get compiled
         ("/* Empty pattern matching */"++)
       else if (length pats == 1) && (not (optDebug opts)) then
              -- one pattern only; skip the branching
              let [PatL _ patE bindsVars] = pats
              in  tab.mkPatBody patE bindsVars.semi
            else
              tab.("switch (CONSTR(cl["++).dS.("])) {"++).nl.
              cases.
              -- only add "default:" when debugging
              (if optDebug opts then defaultCase else id).
              tab.("}"++).nl)
mkCExp _ _ e@(CaseL (Nothing, _) _ _) =
  ierr $ "mkCExp: found non-enumerated case expression: "++(pprint e "")
mkCExp _ _ (ConstrL _) =
  ierr "LAR: ConstrL can only occur as the first symbol of a definition"
mkCExp _ config (BVL v (Just depth, fname)) =
  let gc = optGC $ getOptions config
      compact = optCompact $ getOptions config
      argsN = getFuncArity fname (getArities config)
  in  pprint v.("("++).mkNESTED gc compact fname depth argsN.(")"++)
mkCExp _ _ e@(BVL _ (Nothing, _)) =
    ierr $ "mkCExp: found non-enumerated bound variable: "++(pprint e "")

getFuncArity :: QName -> Arities -> Arity
getFuncArity f ars =
  case Data.Map.lookup f ars of
    Just n -> n
    Nothing -> ierr $ "mkCExp: BV: no arity of enclosing function "++(lName f)

-- | Generates C code for a built-in binary operator.
--   All the supported operators are on integers.
mkBinOp :: COp -> [ExprL] -> TEnv -> ConfigLAR -> ShowS
mkBinOp c [e1, e2] env config =
  let e1' = mkCExp env config e1
      e2' = mkCExp env config e2
      val1 = ("PRIMVAL_R("++).e1'.(")"++)
      val2 = ("PRIMVAL_R("++).e2'.(")"++)
      cBin cOp tagFunc =
        ("(PRIMVAL_C("++).val1.(cOp++).val2.tagFunc config.("))"++)
      opts = getOptions config
      useFastOps = (optCompact opts) && (optFastOp opts)
      -- This is used by the fast arithmetic ops.
      fastOp opN = (opN++).("(("++).e1'.("), ("++).e2'.("))"++)
  in  case c of
        -- If using compact LARs, do faster integer arithmetic for some operators.
        CPlus  | useFastOps -> fastOp "PVAL_ADD"
        CMinus | useFastOps -> fastOp "PVAL_SUB"
        CEqu   | useFastOps -> fastOp "PVAL_EQU"
        CNEq   | useFastOps -> fastOp "PVAL_NEQ"
        -- C operators that are different from Haskell
        CMulI -> lparen.(pprint CMulI).lparen.e1'.comma.e2'.rparen.rparen
        CNEq  -> cBin "!=" mBoolTag
        CMod  -> cBin "%"  mIntTag
        CDiv  -> cBin "/"  mIntTag
        -- C operators that return Int values
        iResOp | iResOp `elem` [ CMinus, CPlus, CMult, CDivide, CMod, CDiv] ->
          cBin (pprint c "") mIntTag
        -- C operators that return Bool values
        bResOp | bResOp `elem` [ CEqu, CLe, CGe, CGt, CLt, CAnd, COr, CNEq] ->
          cBin (pprint c "") mBoolTag
        _     -> ierr $ "mkBinOp: unhandled operator " ++ (pprint c "")
mkBinOp _ _ _ _ =
  ierr "mkBinOp: called with wrong arguments"

-- | An integer value or equivalent (nullary constructor or ground value).
intSusp :: Bool -> String -> ShowS
intSusp compact c =
  if compact then
    ("PRIMVAL_C("++).(c++).(")"++)
  else
    ("(SUSP("++).(c++).(", "++).intTag.(", (TP_)NULL))"++)

-- | Generates C macros for accessing function arguments in a LAR block.
--   Takes into consideration strictness/call-by-name information.
protoB :: BlockL -> TEnv -> Stricts -> CBNVars -> GC -> ShowS
protoB (DefL fName _ bind) _ stricts cbnVars gc =
  let Just cbns = Data.Map.lookup fName cbnVars
      Just strs = Data.Map.lookup fName stricts
  in  foldDot (protoF gc strs cbns fName) (enumNames bind)
protoB (ActualL _ _ _) _ _ _ _ = id

-- | Generates the access macros for the formal variables of a function.
protoF :: GC -> StrictInds -> [QName] -> QName -> (Int, QName) -> ShowS
protoF gc strs cbns fName (n, x)
  | n `elem` strs =
    ("#define " ++).pprint x.("(T0) "++).mkGETSTRICTARG gc fName n.nl
  | x `elem` cbns =
    ("#define " ++).pprint x.
    ("(T0) GETCBNARG(" ++).(shows n).(", T0)"++).nl
  | otherwise =
    case gc of
      SemiGC ->
        ("#define " ++).pprint x.("(T0) "++).
        ("GETARG("++).(shows n).(", T0)"++).nl
      LibGC -> mkDefineVar gc x fName n

-- | The macro that accesses a CAF LAR.
defineGCAF :: MName -> GC -> Int -> ShowS
defineGCAF modName gc arityCAF =
  case gc of
    SemiGC ->
      ("#define "++).nameGCAF modName.("(x)  GETARG(x, "++).namegenv modName.(")"++)
    LibGC ->
      ("#define "++).nameGCAF modName.("(x)  GETARG(x, "++).
      shows arityCAF.(", "++).namegenv modName.(")"++)

-- | Creates the global LAR for CAFs.
genv :: MName -> Int -> ShowS
genv m arityCAF = 
  if arityCAF > 0 then ("static TP_ "++).namegenv m.(";"++) else id

-- | Generates: (a) the CAF of a module, (b) the C declarations of the memory 
--   management subsystem, (c) the Graphviz declarations.
prologue :: Options -> MName -> Int -> ShowS
prologue opts modName arityCAF =
  let gc           = optGC opts
  in  genv modName arityCAF.nl.
      (case gc of
         SemiGC ->
           (case optCMode opts of
               Whole ->
                 ("/* Memory management */"++).nl.
                 ("#define DEFAULT_MAXMEM "++).shows (optMaxMem opts).nl.
                 ("unsigned long MAXMEM = DEFAULT_MAXMEM;"++).nl.
                 ("unsigned long MAXMEMSPACE = DEFAULT_MAXMEM / 2;"++).nl.
                 ("// Function prototypes for the allocator"++).nl.
                 ("static inline byte* MM_alloc(size_t bytes);"++).nl.
                 ("byte *space, *space1, *space2, *spaceStart, *spaceEnd;"++).nl
               CompileModule -> 
                 ("extern inline byte* MM_alloc(size_t bytes);"++).nl)
         LibGC -> id).
      wrapIfSSTACK
      (("// Memory management: shadow stack pointers (base/current)"++).nl.
       ("static TP_* sstack_bottom;"++).nl.
       ("static TP_* sstack_ptr;"++).nl)
      id.
      (if (optVerbose opts) then
         ("// Graphviz output functionality"++).nl.
         ("int counter; FILE *p; /* file for graph output */"++).nl
       else id)
        
-- | The main() of the generated C code. Takes a typing environment, the
--   user options, the pattern matching depth of the main function, and
--   the list of modules (to initialize at runtime).
mainFunc :: TEnv -> Options -> PMDepth -> [MName] -> ShowS
mainFunc env opts mainNesting modules =
  let gc      = optGC opts
      m       = case modules of [m'] -> m' ; _ -> "Main"
      mainDef = mainDefQName m
      printResDT dt = tab.pprinterName dt.("(res); printf(\"  \");"++).nl
      compact = optCompact opts
  in  ("int main(int argc, char* argv[]){\n"++).
      tab.("clock_t t1, t2;"++).nl.
      tab.("Susp res;"++).nl.
      tab.("t1 = clock();\n"++).
      (case gc of
         SemiGC ->
           tab.("/* allocate space in the heap */"++).nl.
           tab.("if (argc > 1) {"++).nl.
           tab.tab.("MAXMEM = strtoul(argv[1], NULL, 10);"++).nl.
           tab.tab.("MAXMEMSPACE = MAXMEM / 2;"++).nl.
           tab.tab.("printf(\"heap size = 2 x %lu bytes\\n\", MAXMEMSPACE);"++).nl.
           tab.("}"++).nl.
           tab.("space1 = (byte *) malloc((size_t)MAXMEMSPACE);"++).nl.
           tab.("space2 = (byte *) malloc((size_t)MAXMEMSPACE);"++).nl.
           tab.("if (space1==NULL || space2==NULL) {"++).nl.
           tab.tab.("printf(\"Cannot allocate memory to start program, \""++).nl.
           tab.tab.("       \"tried 2 x %lu bytes.\\n\", MAXMEMSPACE);"++).nl.
           tab.tab.("exit(EXIT_FAILURE);"++).nl.
           tab.("}"++).nl.
           tab.("space = spaceStart = space1;"++).nl.
           tab.("spaceEnd = space + MAXMEMSPACE;"++).nl
         LibGC ->
           tab.("GC_init();"++).nl.
           wrapIfOMP (tab.("GC_thr_init();"++).nl) id.
           wrapIfGMP (tab.("mp_set_memory_functions(GMP_GC_malloc, GMP_GC_realloc, GMP_GC_free);"++).nl) id
           -- tab.("GC_enable_incremental();"++).nl  -- incremental GC
      ).
      -- shadow stack initialization
      wrapIfSSTACK
      (("sstack_bottom = (TP_*)malloc(sizeof(TP_)*SSTACK_MAX_SIZE);"++).nl.
       ("if (sstack_bottom == 0) { printf(\"No space for shadow stack.\\n\"); exit(0); };"++).nl.
       ("sstack_ptr = sstack_bottom;"++).nl
      ) id.      
      tab.("// initial activation record"++).nl.
      tab.("TP_ T0=NULL;"++).nl.
      (case gc of
          LibGC -> id
          SemiGC ->
            tab.("TP_ t0 = AR(0, "++).shows mainNesting.(");"++).nl.
            if compact then
              id
            else
              tab.("#ifdef GC"++).nl.
              tab.("t0->magic = MAGIC;"++).nl.
              tab.("#endif"++).nl).
      initModules modules.
      logGraphStart opts.
      mkMainCall gc m.
      tab.("t2 = clock();"++).nl.
      (case (findType mainDef env) of
          Tg (T dt) | dt==dtInteger ->
            wrapIfGMP
            (tab.("printf(\"Integer result=%s\\n\", mpz_get_str(0, 10, *((mpz_t*)res.ctxt)));"++).nl)
            (tab.("printf(\"cannot compute 'result', gic must be built with libgmp support\\n\");"++).nl)
          Tg (T dt) ->
            if (dt==dtInt || dt==dtBool) then
              if compact then
                -- compact mode, special int representation
                tab.("printf(\"%lu, \", PRIMVAL_R(res));"++).nl
              else
                -- normal mode, ints are isomorphic to nullary constructors
                tab.("if ((GETTPTR(res.ctxt)) == 0) printf(\"%d, \", CONSTR(res));"++).nl.
                tab.("else printf(\"Thunk{%d, %p}\", CONSTR(res), GETTPTR(res.ctxt));"++).nl
            else
              printResDT dt
          typ@(Tg (TDF _ _)) ->
            error $ "Can not determine pretty printer for higher-order variable result of type "++(pprint typ "")
          Tv _   -> ierr "result variable is polymorphic"
          Ta (Tg (T dt)) _ -> printResDT dt
          t ->
            ierr $ "result variable has unsupported type: "++(pprint t "")
      ).
      tab.("printf(\"c time = %.10f sec\\n\", ((double)(t2 - t1)/CLOCKS_PER_SEC));"++).nl.
      logGraphEnd opts.
      tab.("return 0;"++).nl.
      ("}"++).nl
  
-- | Generates the calls to the modules initializers.
initModules :: [MName] -> ShowS
initModules [] = id
initModules (m:ms) = tab.genInitMod m.("(T0);"++).nl.initModules ms

-- | The code for a module initializer. So far, it only initializes the module CAFs.
initMod :: MName -> ConfigLAR -> ShowS
initMod m config =
  let arityCAF = length (getCAFnmsids config)
      opts     = getOptions config
      gc       = optGC opts
      nms      = map pprint $ nmsids2nms (getCAFnmsids config)
  in  ("void "++).genInitMod m.("(TP_ T0) {"++).nl.
      (if arityCAF > 0 then
         tab.namegenv m.(" = PUSHAR("++).
         (case gc of
             SemiGC ->
               ("AR("++).shows arityCAF.
               (", 0"++).((foldl (\x -> \y -> x ++ ", " ++ (y "")) "" nms)++)
             LibGC ->
               (nameGCAF m).("_AR("++).insCommIfMore nms).
         ("));"++).nl
       else id).
      ("}"++).nl.nl
      
-- | Generates the name of a module initializer.
genInitMod :: MName -> ShowS
genInitMod m = ("__initModule_"++).(m++)
  
-- | Generates C macros for accessing the variables of each LAR block.
argDefs :: [BlockL] -> TEnv -> Stricts -> CBNVars -> GC -> ShowS
argDefs ds env stricts cbnVars gc =
  foldDot (\def -> (protoB def env stricts cbnVars gc)) ds

-- | Returns the LAR with the actuals of a function call.
makeActs :: QName -> [QName] -> TEnv -> ConfigLAR -> ShowS
makeActs f args env config =
  let fNesting  = findPMDepthSafe f (getPMDepths config)
      isVar = case args of { [] | fNesting==0 -> True ; _ -> False }
      allocHeap = optHeap (getOptions config) || (returnsThunk env f)
      gc        = optGC (getOptions config)
      -- nullary functions don't create a new LAR but use the current one (T0)
      -- unless they have nesting > 0
      fLAR =
        if isVar then ("T0"++)
        else
          let fArity = length args
              c      = optCompact (getOptions config)
          in  mkAllocAR gc allocHeap c f fArity fNesting (map pprint args)
      simpleCall = pprint f.("("++).fLAR.(")"++)
  in  case gc of
        LibGC  -> simpleCall
        SemiGC ->
          if isVar then simpleCall
          else ("RETVAL("++).pprint f.("(PUSHAR("++).fLAR.(")))"++)

-- | Finds the pattern-matching depth of the 'result' definition.
depthOfMainDef :: [BlockL] -> Int
depthOfMainDef blocks =
  let findRes (DefL v _ _)    = (lName v)==mainDefName
      findRes (ActualL _ _ _) = False
      res = Prelude.filter findRes blocks
  in  case res of
        [DefL _ e _] -> countPMDepthL e
        _ -> ierr "No (unique) result definition was found."

-- | Main entry point for separate module compilation from "SLIC.Driver".
--   Takes the full module name, the compilation configuration, the typing
--   environment of the module, the DFI of the module, the compilation
--   information for imported names, the compile constructor information,
--   and the actual LAR intermediate code to compile to C.
compileModL :: MNameF -> ConfigLAR -> TEnv -> DFI -> ImportedNames -> 
               CIDs -> ProgL -> IO ()
compileModL fm config env dfi allImps cidsExt finalProgLAR =
  let (moduleName, f) = fm
      fPath           = pathOf f
      moduleC         = fPath++[dirSeparator]++moduleName++".c"
      moduleDFI       = fPath++[dirSeparator]++(dfiFile moduleName)
  in  writeFile moduleC (makeC
        finalProgLAR env config (dfi, allImps, cidsExt) "") >>
      writeFile moduleDFI (show dfi)
  