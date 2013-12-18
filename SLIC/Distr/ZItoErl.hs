-- | Distributed back-end: prototype compiler from ZOIL to Erlang.
-- 
--   Compiles the intensional zero-order program (in ZOIL) to a distributed
--   dataflow program in Erlang. Execution follows the naive parallelism paradigm.
-- 
--   This compiler supports only first-order programs without data types.
-- 
--   There are two modes of parallelism: implicit parallelism that parallelizes
--   the application of every built-in strict operator, and explicit parallelism
--   that only parallelizes expressions using the 'par' combinator (and also
--   recongnizes 'pseq' as the sequential combinator).
-- 
--   Optimizations (implicit parallelism):
-- 
--   - Commutative parallel operators can receive out-of-order messages
--     (saves one message match failure).
-- 
--   - Constants in built-in operations are inlined.
-- 

module SLIC.Distr.ZItoErl (makeErl) where

import Data.Char (toUpper)
import Data.List
import Data.Map (elems)
import SLIC.AuxFun (foldDot, ierr)
import SLIC.Constants
import SLIC.ITrans.Syntax
import SLIC.State
import SLIC.SyntaxAux
import SLIC.Types

-- | The depth of nesting when compiling intensional code.
type EDep    = Int
-- | Erlang generators produce 'ShowS' functions.
type ErlCode = ShowS
-- | Variables are strings.
type QNameErl = String
-- | Constructors are strings.
type CstrNameErl = String
-- | The type of Erlang generators for built-in operations.
type Calc    = [QNameErl] -> ErlCode
-- | The suffix used in each scope by the unique names generator.
type Suffix  = String
-- | The configuration needed for compilation.
type ConfigErl = (TEnv, VUsage, Options, CIDs)

-- | A piece of code may have two kinds of parents, either it is a child of a
--   parallel operator, or it is a sequential inner code block inside an outer
--   Erlang code block. A variable is provided for the parallel
--   operator parent, which holds its PID.
data EParent = ParVar QNameErl   -- ^ the parent is a parallel operator
             | OuterScope     -- ^ the outer scope containg this code

-- | Produces the Erlang expression holding the PID of a parent node.
makePID :: EParent -> ErlCode
makePID (ParVar var) = (var++)
makePID (OuterScope) = ("self()"++)

-- | A piece of code may be a left or right child of a parallel operator parent,
--   a single piece of code, or inherited by the parent using the 'Branch' variable
--   of a process in the generated Erlang code.
data BranchT = LeftNode      -- ^ the left node of a parallel operator
             | RightNode     -- ^ the right node of a parallel operator
             | SingleNode    -- ^ the single child node of an operator (such as 'if')
             | BranchVar     -- ^ the 'Branch' variable (contains the enclosing node's branch)

-- | The Erlang message ids for each type of branching code.
instance Show BranchT where
  showsPrec _ LeftNode   = ("leftNode"++)
  showsPrec _ RightNode  = ("rightNode"++)
  showsPrec _ SingleNode = ("singleNode"++)
  showsPrec _ BranchVar  = ("Branch"++)
  
-- * Erlang translator functions

-- | The main compiler entry point, translates a ZOIL program into
--   Erlang code.
makeErl :: MName -> ProgZ -> ConfigErl -> ErlCode
makeErl m (Prog _ defs) conf@(_, _, opts, cids) =
    let defs' = map (prepareForErl opts cids) defs
        eDefs = foldDot (makeErlDef conf) defs'
        resultSig = pprint (mainDefQName m).("/6"++)
    in  -- error (pprintList defs' "").
        ("-module(main)."++).nl.
        ("-export([init/0, "++).resultSig.(", regval_wh/3])."++).nl.nl.
        ("% The number of the warehouses during execution."++).nl.
        ("-define(WAREHOUSES_NUM, "++).shows (optWhNum opts).(")."++).nl.nl.
        ("% How many contexts a warehouse can create before garbage collection."++).nl.
        ("-define(MAX_SZ_CTXTS, "++).shows (optMaxCtxts opts).(")."++).nl.nl.
        magic.
        regval_wh opts.
        init_func m opts.
        eDefs

-- | Prepares a ZOIL program for the Erlang back-end: illegal symbols are
--   translated (e.g. dollar signs in function names), capitals at the start 
--   of variables are fixed, and constructors are replaced by their numbers.
prepareForErl :: Options -> CIDs -> DefZ -> DefZ
prepareForErl opts cids def =
  let prepareForErlV v = procLName fixSymbols v
      prepareForErlE (XZ (V v)) = XZ (V (prepareForErlV v))
      prepareForErlE (XZ (BV _ _)) =
        error "pattern-bound variables are not supported in the Erlang back-end"
      prepareForErlE (ConZ c el) = ConZ c (map prepareForErlE el)
      prepareForErlE (FZ q v) = FZ q (prepareForErlV v)
      prepareForErlE (CaseZ d e pats) =
        CaseZ d (prepareForErlE e) (map prepareForErlP pats)
      prepareForErlE (ConstrZ c) =
        if optOptEnums opts then
          let (_, cId) = findArId c cids
          in  ConstrZ (mkQNCID cId)
        else
          error "constructors are only supported in -enum mode"
      prepareForErlP (PatZ c e b) =
        if optOptEnums opts then
          let (_, cId) = findArId c cids
          in  PatZ (mkQNCID cId) (prepareForErlE e) b
        else
          error "patterns are only supported in -enum mode"
      fixDollar '$' = '_'
      fixDollar c   = c
      fixSymbols [] = ierr "fixSymbols: empty symbol found"
      fixSymbols s@(c:_) =
        let s' = if toUpper c == c then "constr_"++s else s
        in  map fixDollar s'
  in  case def of
        DefZ v e      -> DefZ (procLName fixSymbols v) (prepareForErlE e)
        ActualsZ v m el ->
          ActualsZ (procLName fixSymbols v) m (map prepareForErlE el)

-- | Function signatures differ according to the parallelism model: when
--   implicitly parallel, all functions must know and thread their parents
--   and branching information; when explicit parallelism is used, this
--   information is only visible at the point of use of the 'par' combinator.
funcSig :: ErlCode
funcSig = ("(Parent, Branch, C, W, Wpend, AuxID) ->"++)
    
-- | Translates a ZOIL definition into Erlang code.
makeErlDef :: ConfigErl -> DefZ -> ErlCode
makeErlDef conf (DefZ v e) =
  pprint v.funcSig.nl.
  (makeErlE conf e "C" (ParVar "Parent") BranchVar 0 True "0").("."++).nl.nl
-- actuals() expressions become /case ... of/ clauses
makeErlDef conf@(_, _, opts, _) (ActualsZ v m el) =
    let aux (i, e) = (i, makeErlE conf e "C2" (ParVar "Parent") BranchVar 0 True (show i))
        inds       = [0..((length el)-1)]
        makeActs (i, e) = shows i.(" -> "++).nl.e
        eExprs     = map makeActs (map aux (zip inds el))
        eExprs'    = if optVerbose opts then eExprs++[eDefault] else eExprs
        eDefault   =
          ("ACTDEFAULT -> io:fwrite(\"ERROR: index ~p not in actuals() of "++).pprint v.("\\n\", [ACTDEFAULT])"++)
    in  pprint v.funcSig.nl.
        error ("TODO: look up the actuals of module "++m).
        (case eExprs of
            [] -> ("io:fwrite(\"Internal error: empty actuals() for "++).pprint v.("\")."++).nl.nl
            _  -> ("{C1, WHPID, C2Ind} = C,"++).nl.
                  -- "io:fwrite(\"getCtxt(~p) returned {~p, ~p, ~p}\\n\", [C, C1, WHPID, C2Ind]),"++).nl.
                  -- "{C1, WHPID, C2Ind} = C,"++).nl.
                  -- "io:fwrite(\"Sending {getCtxtTail, ~p, ~p} to ~p\\n\", [self(), C2Ind, WHPID]),"++).nl.
                  ("WHPID ! {getCtxtTail, self(), C2Ind},"++).nl.
                  ("receive {ctxtTail, C2} ->"++).nl.
                  -- "io:fwrite(\"getCtxtTail(~p)=~p\\n\", [C2Ind, C2]),"++).nl.
                  -- "io:fwrite(\"2. ------------------------\\n\"),"++).nl.
                  -- TODO: opt#1
                  ("case C1 of "++).nl.
                  ((foldDot id (intersperse (";\n"++) eExprs'))).nl.
                  ("end"++).nl.
                  ("end."++).nl.nl)

-- | Produces Erlang code for a ZOIL expression.
-- 
-- Arguments:
-- 
-- - ZOIL expression
-- 
-- - the context to use
-- 
-- - node parent (or inline)
-- 
-- - branch: leftNode or rightNode
-- 
-- - the depth of the expression (for generating unique local vars)
-- 
-- - whether the node is the result of an identifier (and therefore should be memoized)
-- 
-- - a suffix that helps in unique name generation
-- 
makeErlE :: ConfigErl -> ExprZ -> QNameErl -> EParent -> BranchT -> EDep -> Bool -> Suffix -> ErlCode
makeErlE _ (FZ (Call i) v) c parent branch dep _ suf =
    let spc   = tabs dep
        wUsed = ("Wused_"++).(suf++)
        c'    = "C_"++suf
    in  spc.("% Find the warehouse to use for the context of function "++).pprint v.(" in call "++).shows i.nl.
        -- spc.("warehouse:?DEB(\"Sending context demand for call("++).shows i.(", "++).pprint v.(") in context ~p\\n\", ["++).(c++).("]),"++).nl.
        -- spc.wIdx.(" = magic({"++).shows i.(", ok, "++).(c++).("}),"++).nl.
        -- spc.wIdx.(" = (AuxID,"++).nl.
        spc.wUsed.(" = lists:nth(AuxID, W),"++).nl.
        spc.("% Ask the warehouse to create a context descriptor"++).nl.
        spc.wUsed.(" ! {createCtxt, self(), "++).shows i.(", "++).(c++).("},"++).nl.
        spc.("% Call "++).pprint v.("() with the new context descriptor"++).nl.
        spc.("receive {ctxt, "++).(c'++).("} -> "++).
        pprint v.("("++).makePID parent.(", "++).shows branch.(", "++).(c'++).(", W, Wpend, (AuxID rem ?WAREHOUSES_NUM) + 1) end"++)

makeErlE _ e@(FZ _ _) _ _ _ _ _ _ =
  ierr $ "makeErlE: cannot process expression: "++(pprint e "")

makeErlE (_, _, opts, _) (ConZ val []) _ parent branch dep ident _ =
    let spc    = tabs dep
        aux v0 = (if ident then
                    spc.("% register value in the warehouse(s)"++).nl.
                    spc.("regval_wh(Wpend, self(), "++).(v0++).("),"++).nl
                 else id).
                 -- implicit parallelism, tell parent
                 spc.(deb opts $ "io:fwrite(\"Evaluation of ~p reached value ~p, telling parent ~p \\n\", [self(), "++v0++", "++(makePID parent "")++"]),").nl.
                 spc.("% tell the parent execution is over with a value"++).nl.
                 spc.makePID parent.(" ! {notify, "++).shows branch.(", "++).(v0++).("}"++)
    in  case val of
          CN CTrue   -> aux "1"
          CN CFalse  -> aux "0"
          LitInt iV  -> aux (show iV)
          _          -> ierr $ "Found something that is not a constant: "++
                               (show val)

makeErlE conf (ConZ (CN CIf) [cond, e1, e2]) c parent branch dep ident suf =
  makeErlCase conf "if" cond [("1", e1), ("0", e2)] c parent branch dep ident suf

makeErlE conf (CaseZ _ e pats) c parent branch dep ident suf =  
  let brs = map (\(PatZ cP eP _)->(qName cP, eP)) pats
  in  makeErlCase conf "caseZ" e brs c parent branch dep ident suf
      -- error "Pattern matching clauses are not supported yet in the Erlang compiler"

makeErlE conf@(_, _, opts, _) (ConZ (CN op) [e1, e2]) c parent branch dep ident suf =
  let strictBinOpPar opCode =
        debInfo.evalBinOpPar conf e1 e2 c dep (pprint op "") suf.opCode brs parent branch dep ident suf opts
      debInfo = tabs dep.(deb opts $ "io:fwrite(\""++(spcs dep "")++"Evaluating "++(pprint op "")++" operator.\\n\"),\n")
      brs = (not $ isConst e1, not $ isConst e2)
  in  -- implicit parallelism, spawn processes to evaluate each expression
      case op of
        CPlus   -> strictBinOpPar receivePlusMerge
        CMult   -> strictBinOpPar receiveMultMerge
        CMinus  -> strictBinOpPar receiveMinusMerge
        CLe     -> strictBinOpPar receiveLeMerge
        CEqu    -> strictBinOpPar receiveEquMerge
        CLt     -> strictBinOpPar receiveLtMerge
        CAnd    -> strictBinOpPar receiveAndMerge
        CNEq    -> strictBinOpPar receiveNEquMerge
        CMod    -> strictBinOpPar receiveModMerge
        CDiv    -> strictBinOpPar receiveDivMerge
        CPar    -> error "The 'par' combinator is not supported in the implicitly parallel mode."
        CPSeq   -> error "The 'pseq' combinator is not supported in the implicitly parallel mode."
        _     -> error $ "Erlang generator doesn't support binary op: " ++ (show op)

makeErlE _ e@(ConZ _ _) _ _ _ _ _ _ =
  error $ "Erlang generator doesn't support unknown op: " ++ (pprint e "")
  
makeErlE (_, (cbnVars, _), opts, _) (XZ (V v)) c parent branch dep _ suf =
    let spc   = tabs dep
        wIdx  = ("Widx_"++).(suf++)
        wUsed = ("Wused_"++).(suf++)
        wPend2= ("Wpend2_"++).(suf++)
        val   = ("Val_"++).(suf++)
        cUsed = ("C_"++).(suf++)
        sps   = spcs dep ""
        cbns  = concat $ Data.Map.elems cbnVars
    in  spc.("% Find the warehouse to use for identifier "++).pprint v.(" in context "++).(c++).nl.
        spc.cUsed.(" = "++).(c++).(","++).nl.
	spc.wIdx.(" = magic("++).cUsed.("),"++).nl.
	spc.wUsed.(" = lists:nth("++).wIdx.(", W),"++).nl.
        spc.(deb opts $ "io:fwrite(\""++sps++"Node ~p demanding (~p, ~p) from (WH#~p)\\n\", [self(), "++(qName v)++", "++(cUsed "")++", "++(wIdx "")++"]),\n").
        -- if the variable is a call-by-name one (according to the usage analysis), don't
        -- ask the warehouse; act as if the warehouse had let the process 'continue',
        -- this also means that the pending warehouses are not changed
        (if v `elem` cbns then
           ("begin"++).nl.
           spc.tab.wPend2.(" = Wpend,"++).nl
         else
           spc.("% Send a demand to the warehouse"++).nl.
           spc.wUsed.(" ! {demand, self(), "++).pprint v.(", "++).cUsed.("},"++).nl.
           spc.(deb opts $ "io:fwrite(\""++sps++"Node ~p waiting for warehouse reply...\\n\", [self()]),\n").
           spc.("receive"++).nl.
           spc.("{notify, _, "++).val.("} ->"++).nl.
           spc.tab.(deb opts $ "io:fwrite(\"Got value ~p from warehouse!\\n\", ["++(val "")++"]),").nl.
           spc.tab.("% Update the warehouse (unblocking other nodes waiting for the same value)"++).nl.
           spc.tab.("regval_wh(Wpend, self(), "++).val.("),"++).nl.
           -- implicit parallelism, send the value to the parent
           spc.tab.makePID parent.(" ! {notify, "++).shows branch.(", "++).val.("};"++).nl.
           spc.("{continue} ->"++).nl.
           spc.tab.(deb opts "io:fwrite(\"Got message to continue from warehouse!\\n\"),").nl.
           spc.tab.("% Add the warehouse to update to the pending warehouses"++).nl.
           spc.tab.wPend2.(" = ["++).wUsed.("|Wpend],"++).nl).
        -- implicit parallelism, call the variable with messaging information
        spc.tab.pprint v.("("++).makePID parent.(", "++).shows branch.(", "++).cUsed.(", W, "++).wPend2.(", AuxID)"++).nl.
	spc.("end"++)
makeErlE _ (ConstrZ _) _ _ _ _ _ _ =
  ierr "makeErlE: ConstrZ missing"
-- makeErlE conf (ConstrZ cstr) c parent branch dep ident suf =
  -- makeErlE conf (ConZ (CN qName cstr) []) c parent branch dep ident suf

makeErlE _ (XZ (BV _ _)) _ _ _ _ _ _ =
    error "Bound variables are not supported yet in the Erlang compiler"

makeErlCase :: ConfigErl -> String -> ExprZ -> [(CstrNameErl, ExprZ)] -> QNameErl ->
               EParent -> BranchT -> EDep -> Bool -> Suffix -> ErlCode
makeErlCase conf stmt cond brs c parent branch dep ident suf =
    let dep' = dep + 1
        self = "S_"++suf
        spc  = tabs dep
        val  = "CondVal_"++suf
        mkBr (cId, e) =
          makeErlBranch conf (stmt, spc, cId, e) c parent branch dep' ident suf
        branches = foldDot id $ intersperse ((";"++).nl) $ map mkBr brs
        body = ("case "++).(val++).(" of"++).nl.
               branches.nl.
               spc.("end"++)
               -- makeErlBranch conf (stmt, spc, "1", e1) c parent branch dep' ident suf.
               -- (";"++).nl.
               -- makeErlBranch conf (stmt, spc, "0", e2) c parent branch dep' ident suf.
    in  spc.(self++).(" = self(),"++).nl.nl.
        evalSingleNode val (makeErlE conf cond c (ParVar self) SingleNode dep' False (suf++"s")) body spc

makeErlBranch :: ConfigErl -> (String, ShowS, CstrNameErl, ExprZ) -> QNameErl ->
                 EParent -> BranchT -> EDep -> Bool -> Suffix -> ErlCode
makeErlBranch conf@(_, _, opts, _) (stmt, spc, cId, e) c parent branch dep ident suf =
  spc.(cId++).(" ->"++).nl.
  spc.tab.(deb opts $ "io:fwrite(\"("++stmt++") = "++cId++"\\n\"),\n").
  makeErlE conf e c parent branch dep ident (suf++cId)

-- | Takes a variable 'val' and two pieces of generated Erlang code 'e' and 'body'.
--   Produces the equivalent Erlang code for 'let val = e in body'.
--   The last parameter is the indentation spaces.
evalSingleNode :: QNameErl -> ErlCode -> ErlCode -> ErlCode -> ErlCode
evalSingleNode val e body spc =
  spc.("% Spawn a single node."++).nl.
  spc.("spawn(fun () -> "++).nl.e.(" end),\n"++).nl.
  spc.("% Receive the single node result."++).nl.
  spc.("(receive {notify, _, "++).(val++).("} -> "++).nl.
  spc.body.nl.("end)"++)

-- | Generate code that spawns subnodes for built-in operators.
evalBinOpPar :: ConfigErl -> ExprZ -> ExprZ -> QNameErl -> EDep -> String -> Suffix -> ErlCode
evalBinOpPar conf e1 e2 c dep bOp suf =
    let dep' = dep + 1
        self = "S"++(show dep)    -- use the depth to access parent PID
        spc  = tabs dep    
        v1  = nameReceive  LeftNode suf
        v2  = nameReceive RightNode suf
    in  case e1 of
        ConZ (CN c1) [] -> 
          case e2 of
            -- Operation: c1 + c2
            ConZ _ [] -> ierr "TODO: evalBinOpPar: evaluate constants"
            -- Operation: c1 + e2
            _ ->
              spc.(v1++).(" = "++).pprint c1.(","++).nl.
              spc.("% Spawn right subnode for the builtin operator '"++).(bOp++).("'."++).nl.
              spc.(self++).(" = self(),"++).nl.nl.
              spc.spawn_aux conf e2 RightNode (ParVar self) c dep' suf
        _ ->
          case e2 of
            -- Operation: e1 + c2
            ConZ (CN c2) [] ->
              spc.(v2++).(" = "++).pprint c2.(","++).nl.
              spc.("% Spawn left subnode for the builtin operator '"++).(bOp++).("'."++).nl.
              spc.(self++).(" = self(),"++).nl.nl.
              spc.spawn_aux conf e1 LeftNode (ParVar self) c dep' suf
            -- Operation: e1 + e2
            _ ->
              spc.("% Spawn two subnodes for the builtin operator '"++).(bOp++).("'."++).nl.
              spc.(self++).(" = self(),"++).nl.nl.
              spc.spawn_aux conf e1 LeftNode  (ParVar self) c dep' suf.
              spc.spawn_aux conf e2 RightNode (ParVar self) c dep' suf

{-
-- | Evaluates a built-in binary operator sequentially: first each argument, then
--   its result.
evalBinOpSeq :: ConfigErl -> CName -> ExprZ -> ExprZ -> Calc -> EDep -> Bool -> Suffix -> ErlCode
evalBinOpSeq conf c e1 e2 calc dep ident suf =
  let erl1 = makeErlE conf e1 c noParent LeftNode  (dep+1) ident suf
      erl2 = makeErlE conf e2 c noParent RightNode (dep+1) ident suf
      var1 = "VarL_"++suf
      var2 = "VarR_"++suf
      spc  = tabs dep
  in  spc.(var1++).(" = begin "++).nl.erl1.(" end, "++).nl.
      spc.(var2++).(" = begin "++).nl.erl2.(" end, "++).nl.
      spc.calc [var1, var2].nl
-}

{-
noParent :: EParent
noParent = ierr "The 'parent' argument should not be used in this case."
-}

-- | Generates a number of tabs.
tabs :: EDep -> ErlCode
tabs dep = foldDot (\_ -> tab) [0..dep]

-- | Generates a number of spaces.
spcs :: EDep -> ErlCode
spcs dep = foldDot (\_ -> space) [0..dep]

-- | Spawns a process to compute an expression. If a constant is sent here, it is
--   an internal error (since it should already be inlined)
spawn_aux :: ConfigErl -> ExprZ -> BranchT -> EParent -> QNameErl -> EDep -> Suffix -> ErlCode
spawn_aux conf e branch self c dep suf =
  if isConst e then
    ierr $ "constant "++(pprint e "")++" should not be sent as a message to self()"
  else 
    ("spawn(fun () -> "++).nl.(makeErlE conf e c self branch dep False suf).(" end),"++).nl.nl

-- * Merge rules and Erlang code for built-in operators

-- | Name generator for variables in receive clauses from left/right nodes.
nameReceive :: BranchT -> Suffix -> String
nameReceive  LeftNode suf = "V1_"++suf
nameReceive RightNode suf = "V2_"++suf
nameReceive _ _ = ierr "makeReceiveV: no left/right node"

-- | Represents left/right/both branches.
type Branches = (Bool, Bool)

-- | Main receive-merge helper function. Takes the operator and the expression 
--   body. The rest of the parameters are threaded. If the last parameter is 
--   set, the operator is assumed to be commutative and out-of-order value 
--   responses may be used.
receiveMerge :: Branches -> String -> (Calc) -> EParent -> BranchT -> EDep ->
                Bool -> Bool -> Suffix -> Options -> ErlCode
receiveMerge (brL, brR) op calc parent branch dep ident commut suf opts =
    let val = ("Val_"++).(suf++)        
        v1  = nameReceive  LeftNode suf
        v2  = nameReceive RightNode suf
        spc = tabs dep
        spc1= tabs (dep+1)
    in  spc.("% Wait for results of the '"++).(op++).("' operator"++).nl.
        spc.(deb opts $ "io:fwrite(\"Node ~p is waiting for ("++op++") operands...\\n\", [self()]),\n").
        -- both expressions spawn
        (if brL && brR then
           -- commutative operations do not care about the order of results
           (if commut then
              spc.("(receive {notify, _, "++).(v1++).("} ->"++).nl.
              spc.("(receive {notify, _, "++).(v2++).("} ->"++).nl
            -- non-commutative operations should check that the leftNode arrives first
            else
              spc.("(receive {notify, leftNode, "++).(v1++).("} ->"++).nl.
              spc.("(receive {notify, _, "++).(v2++).("} ->"++).nl)
              -- spc.("(receive {notify, rightNode, "++).(v2++).("} ->"++).nl)
           else if brL then
                  spc.("(receive {notify, _, "++).(v1++).("} ->"++).nl
                else if brR then
                       spc.("(receive {notify, _, "++).(v2++).("} ->"++).nl
                     else
                       ierr $ "implicitly parallel operation found no parallelizable nodes for "++op).
        spc1.val.(" = "++).calc [v1, v2].(","++).nl.
        spc1.(deb opts $ "io:fwrite(\"Operator: ~p "++op++" ~p = ~p\\n\", ["++v1++", "++v2++", "++(val "")++"]),\n").
        spc1.makePID parent.(" ! {notify, "++).shows branch.(", "++).val.("}"++).
        (if ident then
           (","++).nl.spc1.("regval_wh(Wpend, self(), "++).val.(")\n"++)
         else nl).
        spc.(if brR then ("end )"++) else id).(if brL then ("end )"++) else id)

-- | Merge rule for the plus operator.
receivePlusMerge :: Branches -> EParent -> BranchT -> EDep -> Bool -> Suffix -> Options -> ErlCode
receivePlusMerge brs parent branch dep ident suf opts =
  receiveMerge brs "+" calcPlus parent branch dep ident True suf opts

-- | The Erlang code that adds two variables.
calcPlus :: Calc
calcPlus vl = (vl!!0++).(" + "++).(vl!!1++)

-- | Merge rule for the minus operator.
receiveMinusMerge :: Branches -> EParent -> BranchT -> EDep -> Bool -> Suffix -> Options -> ErlCode
receiveMinusMerge brs parent branch dep ident suf opts =
  receiveMerge brs "-" calcMinus parent branch dep ident False suf opts

-- | The Erlang code that performs subtraction on two variables.
calcMinus :: Calc
calcMinus vl = (vl!!0++).(" - "++).(vl!!1++)

-- | Merge rule for the multiplication operator.
receiveMultMerge :: Branches -> EParent -> BranchT -> EDep -> Bool -> Suffix -> Options -> ErlCode
receiveMultMerge brs parent branch dep ident suf opts =
  receiveMerge brs "*" (\vl -> (vl!!0++).(" * "++).(vl!!1++)) parent branch dep ident True suf opts

-- | Merge rule for the less-or-equal operator.
receiveLeMerge :: Branches -> EParent -> BranchT -> EDep -> Bool -> Suffix -> Options -> ErlCode
receiveLeMerge brs parent branch dep ident suf opts =
  let calcLe vl = ("if "++).(vl!!0++).("=<"++).(vl!!1++).(" -> 1; true -> 0 end"++)
  in  receiveMerge brs "<=" calcLe parent branch dep ident False suf opts

-- | Merge rule for the less-than operator.
receiveLtMerge :: Branches -> EParent -> BranchT -> EDep -> Bool -> Suffix -> Options -> ErlCode
receiveLtMerge brs parent branch dep ident suf opts =
  receiveMerge brs "<" calcLt parent branch dep ident False suf opts

-- | The Erlang code that computes the less-than test.
calcLt :: Calc
calcLt vl = ("if "++).(vl!!0++).("<"++).(vl!!1++).(" -> 1; true -> 0 end"++)

-- | Merge rule for the equals operator.
receiveEquMerge :: Branches -> EParent -> BranchT -> EDep -> Bool -> Suffix -> Options -> ErlCode
receiveEquMerge brs parent branch dep ident suf opts =
  receiveMerge brs "==" calcEqu parent branch dep ident True suf opts

-- | The Erlang code that computes the equality test.
calcEqu :: Calc
calcEqu vl = ("if "++).(vl!!0++).("=="++).(vl!!1++).(" -> 1; true -> 0 end"++)

-- | Merge rule for the not-equals operator.
receiveNEquMerge :: Branches -> EParent -> BranchT -> EDep -> Bool -> Suffix -> Options -> ErlCode
receiveNEquMerge brs parent branch dep ident suf opts =
  receiveMerge brs "/=" calcNEqu parent branch dep ident True suf opts

calcNEqu :: Calc
calcNEqu vl = ("if "++).(vl!!0++).("=="++).(vl!!1++).(" -> 0; true -> 1 end"++)

-- | Merge rule for the mod operator.
receiveModMerge :: Branches -> EParent -> BranchT -> EDep -> Bool -> Suffix -> Options -> ErlCode
receiveModMerge brs parent branch dep ident suf opts =
  let calcMod vl = (vl!!0++).(" rem "++).(vl!!1++)
  in  receiveMerge brs "mod" calcMod parent branch dep ident False suf opts

-- | Merge rule for the div operator.
receiveDivMerge :: Branches -> EParent -> BranchT -> EDep -> Bool -> Suffix -> Options -> ErlCode
receiveDivMerge brs parent branch dep ident suf opts =
  receiveMerge brs "div" (\vl -> (vl!!0++).(" div "++).(vl!!1++)) parent branch dep ident False suf opts

-- | Merge rule for the and operator.
receiveAndMerge :: Branches -> EParent -> BranchT -> EDep -> Bool -> Suffix -> Options -> ErlCode
receiveAndMerge brs parent branch dep ident suf opts =
  let calcAnd vl = ("if "++).(vl!!0++).("==1 andalso "++).(vl!!1++).("==1 -> 1; true -> 0 end"++)
  in  receiveMerge brs "&&" calcAnd parent branch dep ident True suf opts

isConst :: ExprZ -> Bool
isConst (ConZ _ []) = True
isConst _ = False

-- * Auxiliary functions

-- | Send a message registering a value in a warehouse.
regval_wh :: Options -> ErlCode
regval_wh opts =
    ("% A notification from a process to the warehouses waiting"++).nl.
    ("% for its value"++).nl.
    ("regval_wh(Wpend, PID, Val) ->"++).nl.
    ("case Wpend of"++).nl.
    ("[] -> true;"++).nl.
    ("[W|WS] ->"++).nl.
    (deb opts "io:fwrite(\"Registering value (~p, ~p) with warehouse ~p\\n\", [self(), Val, W]),\n").
    tab.("W ! {regval, self(), Val},"++).nl.
    tab.("regval_wh(WS, PID, Val)"++).nl.
    ("end."++).nl.nl

-- | Program entry point.
init_func :: MName -> Options -> ErlCode
init_func m opts =
  let spc = tabs 1
      wh  = if optWhRedis opts then "warehouse_redis" else "warehouse"
      mainDef = pprint (mainDefQName m)
  in  ("% The main function that kickstarts evaluation."++).nl.
      ("init() ->"++).nl.
      spc.("{_, T1, S1} = erlang:now(),"++).nl.
      spc.("WPIDs = "++).(wh++).(":spawn_warehouses(?WAREHOUSES_NUM, ?MAX_SZ_CTXTS),"++).nl.
      spc.("GCPID = spawn(gc, gc_init, [WPIDs]),"++).nl.
      spc.("Wpend = [],"++).nl.
      spc.("% Create initial context in warehouse 1"++).nl.
      spc.("W_idx0 = 1,"++).nl.
      spc.("W_used0 = lists:nth(W_idx0, WPIDs),"++).nl.
      spc.("% Ask the warehouse to create a context descriptor under fake context -1"++).nl.
      spc.("W_used0 ! {createCtxt, self(), 0, {0, self(), -1}},"++).nl.
      spc.("% Call "++).mainDef.("() with the new context descriptor"++).nl.
      spc.("receive {ctxt, C0} ->"++).nl.    
      spc.spc.("% start evaluation"++).nl.      
      spc.spc.("spawn(main, "++).mainDef.(", [self(), leftNode, C0, WPIDs, Wpend, 1]),"++).nl.
      spc.spc.("% wait for result and show statistics"++).nl.
      spc.spc.("receive {notify, _, ValResult} ->"++).nl.
      spc.spc.spc.("{_, T2, S2} = erlang:now(),"++).nl.
      spc.spc.spc.("io:fwrite(\"result = ~p\\n\", [ValResult]),"++).nl.
      spc.spc.spc.("io:fwrite(\"time = ~p\\n\", [(T2-T1)+((S2-S1)/1000000)])"++).nl.
      spc.spc.("end"++).nl.
      spc.("end."++).nl.nl

-- | Generates debug messages when verbose mode is set.
deb :: Options -> String -> ErlCode
deb opts s = if optVerbose opts then (s++) else ("% "++).(s++)

-- | The "magic()" function that distributes memory.
magic :: ErlCode
magic =
  ("% Chooses the warehouse to hit, according to the context."++).nl.
  ("% The (+1) is due to the 1-based Erlang list indices."++).nl.
  ("magic(X) ->"++).nl.
  ("  case X of"++).nl.
  ("    {J, _, C} ->"++).nl.
  -- ("      io:fwrite(\"magic=~p\\n\", [((abs(C+J) rem ?WAREHOUSES_NUM) + 1)]),"++).nl.
  ("      (abs(C+J) rem ?WAREHOUSES_NUM) + 1;"++).nl.
  ("    _ -> io:fwrite(\"magic(~p)=?\\n\", [X])"++).nl.
  ("end."++).nl.nl

