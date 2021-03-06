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
import Data.Map (elems, filter, lookup, keys, toList, union)
import Data.Set (empty, member, toList)
import SLIC.AuxFun (foldDot, ierr, insCommIfMore, pathOf)
import SLIC.Constants
import SLIC.DFI (DFC(dfcN), DFI(dfiDfInfo), DfInfo(diDfcs, diEApps),
                 ExtAppFuns, dfiFile)
import SLIC.Front.CAF
import SLIC.Front.Defunc (genNApp)
import SLIC.LAR.LARAux
import SLIC.LAR.LARBuiltins
import SLIC.LAR.LARDebug
import SLIC.LAR.LARGraph
import SLIC.LAR.SMacrosAux (MutInfo, declF, nameGCAF, namegenv, protoFunc,
       mkAllocAR, mkDefineVar, mkGETSTRICTARG, mkLARMacro, mkLARMacroOpt,
       mkMainCall, mkMutAR, mkNESTED, mkPUSHAR, mkRETVAL, mkVALS, nameMutAR,smFun)
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
        larStyle    = optLARStyle opts
        gc          = gcFor larStyle
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
    in  headersC larStyle.
        macrosC opts modName arities pmDepths arityCAF.
        prologue opts modName arityCAF.
        predeclarations defs' config.
        (if optTCO opts then
            genMutARs larStyle arities pmDepths defs'
         else id).
        pdeclGCAF config arityCAF.nl.
        declarations dTypes defs'.
        argDefs larStyle defs env strictVars cbns.nl.
        declarationsBuiltins opts.nl.
        (case cMode of
            Whole -> id
            CompileModule ->
              (case gc of
                  SemiGC -> id
                  LibGC  ->
                    pdeclExts opts imports.
                    pdeclExtApps opts (diEApps $ dfiDfInfo dfi)).
              defInterface larStyle (dfiDfInfo dfi) importFuns extCIDs.nl).
        initMod modName config.nl.                   -- module initializer
        (case cMode of
            Whole ->
              debugPrintSymbol (optDebug opts) (map getBlockName defs).
              mainFunc env opts (depthOfMainDef defs) [modName].nl
            CompileModule -> id).
        mainProg defs' env config.
        (case cMode of
            Whole -> prettyPrintersC larStyle.epilogue opts
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
blockIsLocal _ (ActualL {}) = True
blockIsLocal _ _ = ierr "blockIsLocal: found unqualified block definition"

-- | The C headers of the generated code.
headersC :: LARStyle -> ShowS
headersC larStyle =
  let gc = ("#include \"c/gc.h\""++).nl
  in  (case larStyle of
         LAROPT  ->
           ("#include \"c/lar_opt.h\""++).nl.
           ("#include \"gc.h\""++).nl
         LAR64   -> ("#include \"c/lar_compact.h\""++).nl.gc
         LAR     -> ("#include \"c/lar.h\""++).nl.gc).nl.
      ("#include <c/gic_builtins.h>"++).nl.
      wrapIfGMP (("#include <gmp.h>"++).nl) id.nl
      
-- | The C macros of the generated code.
macrosC :: Options -> MName -> Arities -> PMDepths -> Int -> ShowS
macrosC opts modName arities pmDepths arityCAF =
  let larStyle = optLARStyle opts
      gc       = gcFor larStyle
      dbg      = optDebug opts
  in  ("// Macros"++).nl.nl.
      (if optTag opts then
          ("#ifndef USE_TAGS"++).nl.
          ("#error you must enable the USE_TAGS macro for tags to work"++).nl. 
          ("#endif /* USE_TAGS */"++).nl
       else id).
      (case gc of
          SemiGC ->
            -- The memory allocator used by the semi-space collector.
            ("#define GC_MALLOC MM_alloc"++).nl.
            -- Use an explicit stack for function calls.
            wrapIfGC
            (("// pointer stack maximum size"++).nl.
             ("#define SSTACK_MAX_SIZE "++).shows (optEStackSz opts).nl.
             mkPUSHAR dbg.
             mkRETVAL dbg)
            -- No pointer stack, dummy macros (use for testing the allocator).
            (("#define PUSHAR(a) (a)"++).nl.
             ("#define RETVAL(x) (x)"++).nl)
          LibGC    -> id).nl.
      defineGCAF modName gc arityCAF.nl.
      (case gc of
          LibGC  -> createLibGCARInfra opts modName pmDepths.nl
          SemiGC ->
            createSemiGCARInfra modName larStyle arities pmDepths arityCAF.nl).nl

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
createSemiGCARInfra :: MName -> LARStyle -> Arities -> PMDepths -> Int -> ShowS
createSemiGCARInfra m larStyle fArities pmDepths arityCAF =
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
        (case gcFor larStyle of
            SemiGC -> tab.tab.("NESTED(n, lar) = NULL;                \\"++).nl
            LibGC  -> id).
        tab.tab.("AR_CLEAR_"++).shows (d-1).("(lar, n+1);                 \\"++).nl.
        tab.tab.("} while(0)"++).nl
      -- AR_CLEAR, compact representation
      hAR_CLEARc 0 = ("#define AR_CLEAR_0(lar, ar, n) do { } while(0)"++).nl
      hAR_CLEARc d =
        ("#define AR_CLEAR_"++).shows d.("(lar, ar, n) do {            \\"++).nl.
        (case gcFor larStyle of
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
               (if larStyle==LAR64 then
                  foldDot hAR_CLEARc [0..maxNestings]
                else
                  foldDot hAR_CLEAR [0..maxNestings]).nl

-- | Generates the "extern" declarations that link to the defunctionalization
--   interface and to functions in other modules. The CIDs table given is used
--   to generate declarations for bound variables from other modules.
defInterface :: LARStyle -> DfInfo -> [QName] -> CIDs -> ShowS
defInterface larStyle dfInfo importFuns extCIDs = 
  let externF v   = ("extern "++).protoFunc v
      -- generate the constructor variables accessing macros
      macrosConstr (c, (_, ar)) =
        let bvs     = cArgsC c ar
            stricts = empty -- TODO: stricts information for imported constructors
            cbns    = []   -- bound variables are never call-by-name
        in  foldDot (protoF larStyle stricts cbns c) (enumNames bvs)
      extConstrs = map dfcN $ Data.Set.toList $ diDfcs dfInfo
  in  foldDot externF extConstrs.
      foldDot externF (map genNApp $ Data.Set.toList $ diEApps dfInfo).
      foldDot externF importFuns.
      foldDot macrosConstr (Data.Map.toList extCIDs)

-- | Generates specialized macros for LARs used by the functions defined
--   in the current module.
predeclarations :: [BlockL] -> ConfigLAR -> ShowS
predeclarations lblockList config =
    let opts = getOptions config
        predeclF (DefL fname _ bind) =
            let arityA  = length bind
                arityV  = arityA
                nesting = findPMDepthSafe fname (getPMDepths config)
            in  smFun opts fname arityA arityV nesting
        predeclF (ActualL {}) = ierr "predeclF: found actual"
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
    in  case gcFor $ optLARStyle opts of
          SemiGC -> ierr "pdeclExts: not to be used with semiGC"
          LibGC  -> pdecls

-- | Generates specialized macros for LARs used by dispatchers 
--   for (unknown) external constructors
pdeclExtApps :: Options -> ExtAppFuns -> ShowS
pdeclExtApps opts extApps =
  let mkSmFun ar = smFun opts (genNApp ar) (1+ar) (1+ar) 1
  in  foldDot mkSmFun $ Data.Set.toList extApps

-- | Generates macros for the LAR mutators of the program tail calls.
genMutARs :: LARStyle -> Arities -> PMDepths -> [BlockL] -> ShowS
genMutARs larStyle localArities localPmdepths defs =
  let arities  = union localArities builtinArities
      pmdepths = union localPmdepths builtinPmDepths
      findCIsB (DefL _ e _) = findCIsE e
      findCIsB (ActualL _ _ e) = findCIsE e
      findCIsE (LARCall _ _ NoCI) = []
      findCIsE (LARCall _ _ (Mut _ Nothing)) =
        ierr "mutation index not set"
      findCIsE (LARCall f qns (Mut mut (Just iidx))) =
        case (Data.Map.lookup f arities, Data.Map.lookup f pmdepths) of
          (Just a, Just n) -> [(f, (mut, iidx), qns, (a, n))]
          _ -> ierr $ "findCIsE: Missing information for "++(qName f)
      findCIsE (LARC _ el) = concatMap findCIsE el
      findCIsE (ConstrL{}) = []
      findCIsE (BVL{}) = []
      findCIsE (CaseL _ _ pats) = concatMap findCIsP pats
      findCIsP (PatB _ e) = findCIsE e
      allMutInfo :: [MutInfo]
      allMutInfo = concatMap findCIsB defs
  in  foldDot (mkMutAR larStyle) allMutInfo

-- | Generates specialized macros for a module's global CAF.
pdeclGCAF :: ConfigLAR -> Int -> ShowS
pdeclGCAF config arityCAF =
    let m       = getModName config
        opts    = getOptions config
    in  case (gcFor $ optLARStyle $ getOptions config) of
          SemiGC -> id
          LibGC  -> mkLARMacro opts ("GCAF_"++m) arityCAF arityCAF 0

-- | Generates the C declarations of the LAR blocks and the forcing
--   functions for the module data types.
declarations :: [Data] -> [BlockL] -> ShowS
declarations dts ds =
    let proto (DefL n _ _) = protoFunc n
        proto (ActualL  n _ _) = protoVar (qName n)
        idDefs = foldDot proto ds
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

-- | Generates C code for a block.
mkCBlock :: BlockL -> TEnv -> ConfigLAR -> ShowS
mkCBlock (DefL f e bind) env config =
  let fArity   = length bind
      opts     = getOptions config
      larStyle = optLARStyle opts
      gc       = gcFor larStyle
  in  ("FUNC("++).pprint f.("){"++).nl.
      (case gc of
          LibGC | fArity > 0 ->
            ("INIT_ARG_LOCKS("++).shows fArity.(");"++).nl
          _ -> id).
      debugFuncPrologue (optDebug opts) f.
      (case Data.Map.lookup f (getStricts config) of 
          Nothing -> id
          Just strictFrms -> forceStricts larStyle strictFrms fArity).
      logPrev opts.
      mkCFuncBody config env f e.
      ("}"++).nl
mkCBlock (ActualL v act e) env config =
  let opts = getOptions config
  in  ("VAR("++).pprint v.("){"++).nl.
      debugVarPrologue (optDebug opts) v.
      mkAct act opts.
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
      patDS = shows patD
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
forceStricts :: LARStyle -> StrictInds -> Arity -> ShowS
forceStricts larStyle strictInds fArity =
  let aux x =
        mkVALS larStyle x fArity "T0".
        (" = ((LarArg)CODE("++).shows x.(", T0))(T0); // strict "++).nl
  in  foldDot aux $ Data.Set.toList strictInds

-- | Generates the C code for an expression that is assumed to be the body
--   of a definition.
mkCStmBody :: ExprL -> TEnv -> ConfigLAR -> ShowS
mkCStmBody e@(CaseL {}) env config =
  tab.("Susp Res;"++).nl.
  -- evaluate the pattern matching clause expression
  mkCExp env config e.nl.
  ("return Res;"++).nl
-- For thunk constructors, the constructor id is returned paired with the
-- current context (for nullary constructors the context is 0).
mkCStmBody (ConstrL (CC c cId cArity)) _ config =
  let opts = getOptions config
  in  logConstr opts c.
      mkSusp opts cId uTag (cArity>0).nl
mkCStmBody e env config = ("return "++).mkCExp env config e.semi.nl

-- | Generates C code for a LAR expression. Takes the name of the containing
--   function (for custom LAR access), the typing environment, the
--   configuration, and the LAR expression.
mkCExp :: TEnv -> ConfigLAR -> ExprL -> ShowS
mkCExp env config (LARC (CN c) exps) =
  let opts       = getOptions config
      larStyle   = optLARStyle opts
      useFastOps = (larStyle==LAR64) && (optFastOp opts)
  in  case c of
        CIf  -> ("(PVAL_R("++).mkCExp env config (exps !! 0).(")?"++).
                       ("("++).mkCExp env config (exps !! 1).("):"++).
                       ("("++).mkCExp env config (exps !! 2).("))"++)
        c' | c' `elem` [ CMinus, CPlus, CMult, CDivide, CEqu, CLe, CGe  
                       , CGt, CLt, CAnd, COr, CMulI, CNEq, CMod, CDiv] ->
          mkBinOp c' exps env config
        CNeg ->
          let nExp = mkCExp env config (exps !! 0)
          in  if useFastOps then
                ("PVAL_NEG("++).nExp.(")"++)
              else
                ("PVAL_C(-(PVAL_R("++).nExp.("))"++).mIntTag config.(")"++)
        CTrue  -> intSusp larStyle "True"
        CFalse -> intSusp larStyle "False"
        _      -> error $ "mkCExp: unknown built-in constant "++(pprint c "")
mkCExp _ config (LARC (LitInt i) exps) =
  case exps of
    []    -> intSusp (optLARStyle $ getOptions config) (show i)
    (_:_) -> ierr "Integer literal applied to expressions."
mkCExp _ config (LARCall n _ (Mut _ iidx)) | optTCO (getOptions config) =
  case iidx of
    Just i  -> pprint n.("("++).nameMutAR n i.(")"++)
    Nothing -> ierr "mkCExp: missing intensional index"
mkCExp env config (LARCall n acts _) =
  if n `elem` (nmsids2nms (getCAFnmsids config)) then
    let Just n' = (getCAFid n (getCAFnmsids config))
    in  ("("++).nameGCAF (getModName config).(("("++(show n')++"))")++)
  else makeActs n acts env config
mkCExp env config (CaseL (cn, efunc) e pats) =
  let matchedExpr = mkCExp env config e
      cases       = foldDot mkCPat pats      
      opts        = getOptions config
      sConstrID   = case cn of
                      CLoc Nothing       -> ierrCLoc
                      CLoc (Just (c, _)) -> ("CONSTR(cl["++).shows c.("])"++)
                      CFrm _             -> ("CONSTR("++).matchedExpr.(")"++)
      defaultCase = tab.("default: printf(\"Pattern matching on "++).pprint e.
                    (" failed: constructor %d encountered.\\n\", "++).
                    sConstrID.("); exit(0);"++).nl
      -- | Generates C code for a pattern. /case/ bodies are contained
      --   in {...} as they may contain declarations.
      mkCPat (PatB (CC c cId _, bindsVars) eP) =
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
            CaseL {} -> id
            _        -> ("Res = "++)).
        mkCExp env config ePB
      larStyle = optLARStyle opts
      argsN = getFuncArity efunc (getArities config)
      ierrCLoc = ierr $ "mkCExp: non-enumerated case expression: "++(pprint e "")
  in  (case cn of
          CLoc Nothing -> ierrCLoc
          CLoc (Just (counter, _)) ->
            let dS = shows counter
            in  tab.("cl["++).dS.("] = "++).matchedExpr.semi.nl.
                -- TODO: eliminate this when all patterns are nullary constructors
                -- (or are used as such, see 'bindsVars')
                tab.mkNESTED larStyle efunc counter argsN.(" = CPTR(cl["++).dS.("]);"++).nl.
                logDict opts counter ;
           CFrm _ -> id).
      -- if debug mode is off, optimize away constructor choice when there is
      -- only one pattern (will segfault/misbehave if the constructor
      -- reached is missing)
      (if null pats then
         -- pattern matching without any patterns, does not get compiled
         ("/* Empty pattern matching */"++)
       else if (length pats == 1) && (not (optDebug opts)) then
              -- one pattern only; skip the branching
              let [PatB (_, bindsVars) patE] = pats
              in  -- If using a formal scrutinee, evaluate it now (since the
                  -- 'switch' that evaluates it will be skipped).
                  (case cn of
                     CFrm _ -> matchedExpr.(";"++).nl
                     CLoc _ -> id).
                  tab.mkPatBody patE bindsVars.semi
            else
              tab.("switch ("++).sConstrID.(") {"++).nl.
              cases.
              -- only add "default:" when debugging
              (if optDebug opts then defaultCase else id).
              tab.("}"++).nl)
mkCExp _ _ (ConstrL _) =
  ierr "LAR: ConstrL can only occur as the first symbol of a definition"
mkCExp _ _ e@(BVL _ (CLoc Nothing, _)) =
  ierr $ "mkCExp: found non-enumerated bound variable: "++(pprint e "")
mkCExp _ config bv@(BVL v (cloc, fname)) =
  let larStyle = optLARStyle $ getOptions config
      argsN    = getFuncArity fname (getArities config)
      gc       = gcFor larStyle
  in  case cloc of
        CLoc Nothing -> ierr $ "non-enumerated bound variable: "++(pprint bv "")
        CLoc (Just (counter, _)) ->
          mkCall gc v (mkNESTED larStyle fname counter argsN)
        CFrm i ->
          -- Read the nested context directly from a formal (no thunk flag check).
          mkCall (gcFor larStyle) v (("FRM_NESTED("++).shows i.(")"++))

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
      val1 = ("PVAL_R("++).e1'.(")"++)
      val2 = ("PVAL_R("++).e2'.(")"++)
      cBin cOp tagFunc =
        ("(PVAL_C("++).val1.(cOp++).val2.tagFunc config.("))"++)
      opts = getOptions config
      useFastOps = (optLARStyle opts==LAR64) && (optFastOp opts)
      -- This is used by the fast arithmetic ops.
      fastOp opN = (opN++).("(("++).e1'.("), ("++).e2'.("))"++)
  in  case c of
        -- If using compact LARs, do faster integer arithmetic for some operators.
        CPlus  | useFastOps -> fastOp "PVAL_ADD"
        CMinus | useFastOps -> fastOp "PVAL_SUB"
        CMult  | useFastOps -> fastOp "PVAL_MUL"
        CDiv   | useFastOps -> fastOp "PVAL_DIV"
        CDivide| useFastOps -> fastOp "PVAL_DIV"
        CMod   | useFastOps -> fastOp "PVAL_MOD"
        -- CAnd   | useFastOps -> fastOp "PVAL_AND"
        COr    | useFastOps -> fastOp "PVAL_OR"
        CEqu   | useFastOps -> fastOp "PVAL_EQU"
        CNEq   | useFastOps -> fastOp "PVAL_NEQ"
        CLt    | useFastOps -> fastOp "PVAL_LT"
        CGt    | useFastOps -> fastOp "PVAL_GT"
        CLe    | useFastOps -> fastOp "PVAL_LE"
        CGe    | useFastOps -> fastOp "PVAL_GE"
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
intSusp :: LARStyle -> String -> ShowS
intSusp LAR64 c = ("PVAL_C("++).(c++).(")"++)
intSusp _     c = ("(SUSP("++).(c++).(", "++).intTag.(", NULL))"++)

-- | Generates C macros for accessing function arguments in a LAR block.
--   Takes into consideration strictness/call-by-name information.
protoB :: LARStyle -> BlockL -> TEnv -> Stricts -> CBNVars -> ShowS
protoB larStyle (DefL fName _ bind) _ stricts cbnVars =
  let Just cbns = Data.Map.lookup fName cbnVars
      Just strs = Data.Map.lookup fName stricts
  in  foldDot (protoF larStyle strs cbns fName) (enumNames bind)
protoB _ (ActualL {}) _ _ _ = id

-- | Generates the access macros for the formal variables of a function.
protoF :: LARStyle -> StrictInds -> [QName] -> QName -> (Int, QName) -> ShowS
protoF larStyle strs cbns fName (n, x)
  | n `member` strs =
    ("#define " ++).pprint x.("(T0) "++).mkGETSTRICTARG larStyle fName n.nl
  | x `elem` cbns =
    ("#define " ++).pprint x.("(T0) GETCBNARG(" ++).(shows n).(", T0)"++).nl
  | otherwise =
    case gcFor larStyle of
      SemiGC ->
        ("#define " ++).pprint x.("(T0) "++).
        ("GETARG("++).(shows n).(", T0)"++).nl
      LibGC -> mkDefineVar larStyle x fName n

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
  if arityCAF > 0 then 
    wrapIfGC
    (("static TP_ *"++).namegenv m.(";"++).nl)
    (("static TP_ "++).namegenv m.(";"++).nl)
  else id

-- | Generates: (a) the CAF of a module, (b) the C declarations of the memory 
--   management subsystem, (c) the Graphviz declarations.
prologue :: Options -> MName -> Int -> ShowS
prologue opts modName arityCAF =
  let gc = gcFor $ optLARStyle opts
  in  genv modName arityCAF.nl.         -- prototype for module CAF
      (case gc of
         SemiGC ->
           (case optCMode opts of
               Whole ->
                 ("/* Memory management */"++).nl.
                 ("#define DEFAULT_MAXMEM "++).shows (optMaxMem opts).nl.
                 ("unsigned long MAXMEM = DEFAULT_MAXMEM;"++).nl.
                 ("unsigned long MAXMEMSPACE = DEFAULT_MAXMEM / 2;"++).nl.
                 ("// Function prototypes for the allocator"++).nl.
                 (if optLink opts then
                    ("inline byte* MM_alloc(size_t bytes);"++)
                  else
                    ("static inline byte* MM_alloc(size_t bytes);"++)).nl.
                 ("byte *space, *space1, *space2, *spaceStart, *spaceEnd;"++).nl
               CompileModule -> 
                 ("extern inline byte* MM_alloc(size_t bytes);"++).nl).
           wrapIfGC
           (("// Memory management: pointer stack pointers (base/current)"++).nl.
            ("static TP_ *sstack_bottom;"++).nl.
            ("static TP_ *sstack_ptr;"++).nl)
           id
         LibGC -> id).
      (if (optVerbose opts) then
         ("// Graphviz output functionality"++).nl.
         ("int counter; FILE *p; /* file for graph output */"++).nl
       else id)
        
-- | The main() of the generated C code. Takes a typing environment, the
--   user options, the pattern matching depth of the main function, and
--   the list of modules (to initialize at runtime).
mainFunc :: TEnv -> Options -> PMDepth -> [MName] -> ShowS
mainFunc env opts mainNesting modules =
  let m       = case modules of [m'] -> m' ; _ -> "Main"
      mainDef = mainDefQName m
      printResDT dt = tab.pprinterName dt.("(res); printf(\"  \");"++).nl
      dbg     = optDebug    opts
      larStyle= optLARStyle opts
      gc      = gcFor larStyle
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
           tab.("spaceEnd = space + MAXMEMSPACE;"++).nl.
           -- Initialize the explicit pointer stack.
           wrapIfGC
           (("sstack_bottom = (TP_*)malloc(sizeof(TP_)*SSTACK_MAX_SIZE);"++).nl.
            ("if (sstack_bottom == 0) { printf(\"No space for pointer stack.\\n\"); exit(0); };"++).nl.
            ("sstack_ptr = sstack_bottom;"++).nl
           ) id
         LibGC ->
           tab.("GC_init();"++).nl.
           wrapIfOMP (tab.("GC_thr_init();"++).nl) id.
           wrapIfGMP (tab.("mp_set_memory_functions(GMP_GC_malloc, GMP_GC_realloc, GMP_GC_free);"++).nl) id
           -- tab.("GC_enable_incremental();"++).nl  -- incremental GC
      ).
      tab.("// initial activation record"++).nl.
      tab.("TP_ T0_ = NULL;"++).nl.
      tab.("TP_ AR_TP(T0) = AR_REF(T0_);"++).nl.
      (case gc of
          LibGC  -> id
          SemiGC ->
            tab.debugCreateTopLAR dbg.
            tab.("TP_ AR_TP(t0) = PUSHAR(AR(0,"++).shows mainNesting.("));"++).nl).
      initModules modules.
      logGraphStart opts.
      mkMainCall gc m.
      tab.("t2 = clock();"++).nl.
      (case (findType mainDef env) of
          Tg (T dt) | dt==dtInteger ->
            wrapIfGMP
            (tab.("printf(\"Integer result=%s\\n\", mpz_get_str(0, 10, *((mpz_t*)(CPTR(res)))));"++).nl)
            (tab.("printf(\"cannot compute 'result', gic must be built with libgmp support\\n\");"++).nl)
          Tg (T dt) ->
            if (dt==dtInt || dt==dtBool) then
              if larStyle==LAR64 then
                -- compact mode, special int representation
                tab.("printf(\"%ld, \", PVAL_R(res));"++).nl
              else
                -- normal mode, ints are isomorphic to nullary constructors
                tab.("if ((CPTR(res)) == 0) printf(\"%d, \", CONSTR(res));"++).nl.
                tab.("else printf(\"Thunk{%d, %p}\", CONSTR(res), CPTR(res));"++).nl
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
      debugMainFinish dbg.
      logGraphEnd opts.
      tab.("return 0;"++).nl.
      ("}"++).nl

-- | Generates the calls to the modules initializers.
initModules :: [MName] -> ShowS
initModules [] = id
initModules (m:ms) = tab.genInitMod m.("(T0);"++).nl.initModules ms

-- | The code for a module initializer. So far, it only initializes the module
--   LAR containing the top-level CAFs of the module.
initMod :: MName -> ConfigLAR -> ShowS
initMod m config =
  let arityCAF = length (getCAFnmsids config)
      opts     = getOptions config
      nms      = map pprint $ nmsids2nms (getCAFnmsids config)
      cafName  = namegenv m
  in  ("void "++).genInitMod m.("(TP_ AR_TP(T0)) {"++).nl.
      (if arityCAF > 0 then
         debugCreateCAF (optDebug opts) cafName.
         tab.cafName.(" = "++).
         (case gcFor (optLARStyle opts) of
             SemiGC ->
               ("PUSHAR(AR("++).shows arityCAF.
               (", 0"++).((foldl (\x -> \y -> x ++ ", " ++ (y "")) "" nms)++).
               ("));"++).nl
             LibGC ->
               (nameGCAF m).("_AR("++).insCommIfMore nms.(");"++)).nl
       else id).
      ("}"++).nl.nl
      
-- | Generates the name of a module initializer.
genInitMod :: MName -> ShowS
genInitMod m = ("__initModule_"++).(m++)
  
-- | Generates C macros for accessing the variables of each LAR block.
argDefs :: LARStyle -> [BlockL] -> TEnv -> Stricts -> CBNVars -> ShowS
argDefs larStyle ds env stricts cbnVars =
  foldDot (\def -> (protoB larStyle def env stricts cbnVars)) ds

-- | Returns the LAR with the actuals of a function call.
makeActs :: QName -> [QName] -> TEnv -> ConfigLAR -> ShowS
makeActs f args env config =
  let fNesting  = findPMDepthSafe f (getPMDepths config)
      isVar = case args of { [] | fNesting==0 -> True ; _ -> False }
      allocHeap = optHeap (getOptions config) || (returnsThunk env f)
      larStyle  = optLARStyle (getOptions config)
      -- nullary functions don't create a new LAR but use the current one (T0)
      -- unless they have nesting > 0
      fLAR =
        if isVar then ("AR_TP(T0)"++)
        else
          let fArity = length args
          in  mkAllocAR larStyle allocHeap f fArity fNesting (map pprint args)
      simpleCall = pprint f.("("++).fLAR.(")"++)
  in  case larStyle of
        LAROPT  -> simpleCall
        _       -> ("RETVAL("++).pprint f.("(PUSHAR("++).fLAR.(")))"++)

-- | Finds the pattern-matching depth of the 'result' definition.
depthOfMainDef :: [BlockL] -> Int
depthOfMainDef blocks =
  let findRes (DefL v _ _) = (lName v)==mainDefName
      findRes (ActualL {}) = False
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
