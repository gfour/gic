-- | The language of the Tagged-Token Dataflow back-end.
-- 
--   The TTD back-end compiles the intensional program to a dataflow graph.
--   The execution model follows closely the ideas of tagged-token dataflow
--   architectures (where contexts are tags), aided by a distributed warehouse 
--   to do value memoization.
-- 
--   Each variable becomes a dataflow variable node with a local warehouse and 
--   connections to the nodes of the variables it uses.
-- 
--   A new first-order intermediate language (TTD, tagged-token dataflow) is 
--   used, with two new features:
-- 
--   * 'Ports', numbered connections between the dataflow nodes (connecting 
--     each v or call(v) to the corresponding node).
-- 
--   * 'Nodes', which represent pieces of the graph that depend on the values 
--     of other nodes and must be activated to compute values.
-- 

module SLIC.TTD.SyntaxTTD where

import Data.List (nub)
import SLIC.AuxFun
import SLIC.Constants
import SLIC.ITrans.Syntax
import SLIC.SyntaxAux
import SLIC.Types

-- * The TTD language

-- | A numbered edge in the dataflow graph. It connects two 'ports' 
--   of nodes in the dataflow graph. Since edges are uniquely enumerated, ports
--   are assumed to be numbered in the same way (i.e. edge 8 between 'v1' and 'v2'
--   connects port 8 of node 'v1' to port 8 of node 'v2').
type Edge = Maybe Int

pprintEdge :: Edge -> ShowS
pprintEdge (Just eIdx) = shows eIdx
pprintEdge Nothing = ("?"++)

-- | A link to a node that is either a simple variable read, or an
--   intensional 'call' operator with some index.
data NodeLink = VarEdge QName Edge                 -- ^ variable
              | VarCall QName Edge (Maybe IIndex)  -- ^ call(i) intensional operator
                deriving (Ord, Eq)
                         
-- | A ''node'' encapsulates an expression with input ports (the single
--   value port is omitted).
data NodeT = OpT Const [NodeLink]           -- ^ strict operator
           | IfT NodeT NodeT NodeT          -- ^ conditional

-- | Variable definitions.
data DefT = DefT QName NodeT               -- ^ function/CAF definition
          | ActualsT QName MName [NodeT]   -- ^ actuals(...) definitions

-- | A merged operator is a piece of code that only depends on other node links.
data MOp = MOp Const [MOp]            -- ^ merged operator
         | MNL NodeLink               -- ^ a node link
         deriving (Ord, Eq)

-- | A merged operator definition.
type MOpDef = (QName, MOp)

-- | The dataflow program is a collection of definitions and merged operators.
data ProgT = ProgT [DefT] [MOpDef]

instance PPrint NodeLink where
  pprint (VarEdge v e)   = pprint v.("~"++).pprintEdge e
  pprint (VarCall v e i) =
    let iS = case i of Just iidx -> pprintIdx iidx ; Nothing -> ("?"++)
    in  ("call_"++).iS.lparen.pprint v.rparen.("~"++).pprintEdge e

instance PPrint NodeT where
  pprint (OpT c nls)   = ("[["++).pprint c.("| "++).pprintList (", "++) nls.("]]"++)
  pprint (IfT b b1 b2) = ("if "++).pprint b.(" then "++).pprint b1.(" else "++).pprint b2

instance PPrint DefT where
  pprint (DefT v b)      =
    pprint v.(" = "++).pprint b
  pprint (ActualsT v m bl) = 
    pprint v.("{"++).(m++).("}"++).(" = actuals["++).pprintList (", "++) bl.("]"++)

instance PPrint MOp where
  pprint (MOp c mops) = prettyConst 0 c mops -- (c++).lparen.pprintList (", "++) mops.rparen
  pprint (MNL nlink)  = pprint nlink
  
-- | Pretty printer for merged operator definitions.
pprintMOpDef :: MOpDef -> ShowS
pprintMOpDef (v, mop) = ("Operator "++).pprint v.(": "++).pprint mop

instance PPrint ProgT where
  pprint (ProgT ds ops) =
    foldl (\f -> \d -> f . pprint d . nl) id ds.
    foldl (\f -> \d -> f . pprintMOpDef d . nl) id ops
  
-- * The translator from ZOIL to TTD

-- | The identity merged operator.
cMOpId :: COp
cMOpId = CMOp "$id$"

-- | The translation from ZOIL to the tagged-token dataflow language.
fromZItoTTD :: ProgZ -> ProgT
fromZItoTTD (Prog _ defs) =
  let fromZItoTTDd :: DefZ -> DefT
      -- TODO: thread the counters
      fromZItoTTDd (DefZ v e)        = DefT v (fromZItoTTDe_node e)
      fromZItoTTDd (ActualsZ v m el)   = ActualsT v m (map fromZItoTTDe_node el)
      -- Converts the ZOIL expression to a node.
      fromZItoTTDe_node :: ExprZ -> NodeT
      fromZItoTTDe_node e@(XZ (V _))      = OpT (CN cMOpId) [fromZItoTTDe_nlink e]
      fromZItoTTDe_node e@(FZ (Call _) _) = OpT (CN cMOpId) [fromZItoTTDe_nlink e]
      fromZItoTTDe_node (ConZ (CN CIf) [e0, e1, e2])   =
        let b0 = fromZItoTTDe_node e0
            b1 = fromZItoTTDe_node e1
            b2 = fromZItoTTDe_node e2
        in  IfT b0 b1 b2
      -- eliminate the parameters of merged operators
      fromZItoTTDe_node (ConZ cn el)               = 
        case cn of
          CN (CMOp _) -> OpT cn []
          _           -> OpT cn (map fromZItoTTDe_nlink el)
      fromZItoTTDe_node e                          = error $ "ZOIL->TTD, unsupported expression: "++(pprint e "")
  in  ProgT (map fromZItoTTDd defs) []

-- | Represents an expression (that is a variable or a 'call') as a node link.
fromZItoTTDe_nlink :: ExprZ -> NodeLink
fromZItoTTDe_nlink (XZ (V v)) = VarEdge v Nothing
fromZItoTTDe_nlink (FZ (Call i) v) = VarCall v Nothing (Just i)
fromZItoTTDe_nlink e = error $ "cannot convert to edge, arg of non-merged op? "++(pprint e "")

-- | Merges the operators in a ZOIL program to form merged operator blocks that only depend on variables and function calls.
mergeOpsZI :: ProgZ -> (ProgZ, [MOpDef])
mergeOpsZI (Prog ds defs) =
  let makeMOp :: ExprZ -> MOp
      makeMOp (ConZ cn@(LitInt _) []) = MOp cn []
      makeMOp (ConZ (CN c) el) = 
        if c==CIf then 
          error "makeMOp does not support nested ifs"
        else
          MOp (CN c) (map makeMOp el)
      makeMOp e@(XZ (V _))      = MNL (fromZItoTTDe_nlink e)
      makeMOp e@(FZ (Call _) _) = MNL (fromZItoTTDe_nlink e)
      makeMOp e = error $ "makeMOp: unsupported expression found when merging op: "++(pprint e "")
      mergeOpsD :: Int -> [MOpDef] -> [DefZ] -> ([DefZ], [MOpDef])
      mergeOpsD _ mops [] = ([], mops)
      mergeOpsD i mops ((DefZ v e):rs) =
        let (e', i', mops') = mergeOpsE i mops e
            (rs', mops'')   = mergeOpsD i' mops' rs
        in  (((DefZ v e') : rs'), mops'')
      mergeOpsD i mops ((ActualsZ v m el):rs) =
        let (el', i', mops') = mergeOpsE_el i mops el
            (rs', mops'')    = mergeOpsD i' mops' rs            
        in  (((ActualsZ v m el') : rs'), mops'')
      -- the main function that merges built-in operators
      mergeOpsE :: Int -> [MOpDef] -> ExprZ -> (ExprZ, Int, [MOpDef])
      mergeOpsE i ops e@(XZ (V _))      = (e, i, ops)
      mergeOpsE i ops e@(FZ (Call _) _) = (e, i, ops)
      mergeOpsE i ops (ConZ (CN CIf) [e, e1, e2])    =
        let (e' , i' , ops')  = mergeOpsE i   ops   e
            (e1', i1', ops1') = mergeOpsE i'  ops'  e1
            (e2', i2', ops2') = mergeOpsE i1' ops1' e2
        in  (ConZ (CN CIf) [e', e1', e2'], i2', ops2')
      mergeOpsE i ops e@(ConZ _ el) =
        -- TODO: uncomment this to have simple built-ins not as mops
        -- However, this is not convenient for the graph, no edges are then made.
        -- if the operation is already in merged form, leave it unchanged
        --   if (and (map isV el)) then        
        -- Currently, only constants are left unchanged.
        if el == [] then
          (e, i, ops)
        else
          let newOpName = QN Nothing (mopPre++(show i))
              gatherVars :: ExprZ -> [ExprZ]
              gatherVars e1@(XZ (V _))      = [e1]
              gatherVars e1@(FZ (Call _) _) = [e1]
              gatherVars (ConZ _ el1) = concatMap gatherVars el1
              gatherVars e1 = error $ "gatherVars: cannot process expression: "++(pprint e1 "")
              newOp = (newOpName, makeMOp e)
              vArgs = nub (concatMap gatherVars el)
          in  (ConZ (CN $ CMOp $ lName newOpName) vArgs, i+1, newOp:ops)
      mergeOpsE _ _ e = error $ "TODO: mergeOpsE for expression "++(pprint e "")
      mergeOpsE_el :: Int -> [MOpDef] -> [ExprZ] -> ([ExprZ], Int, [MOpDef])
      mergeOpsE_el i ops [] = ([], i, ops)
      mergeOpsE_el i ops (e:el) =
        let (e' , i' , ops' ) = mergeOpsE    i  ops  e
            (el', i'', ops'') = mergeOpsE_el i' ops' el
        in  (e':el', i'', ops'')
      result = mergeOpsD 0 [] defs
  in  (Prog ds (fst result), snd result)

isV :: ExprZ -> Bool
isV (XZ (V _)) = True
isV (FZ (Call _) _) = True
isV _ = False

-- | Enumerates all ports that point to variables (this includes function calls).
enumPorts :: ProgT -> [MOpDef] -> ProgT
enumPorts (ProgT defs []) mops =
  let enumPortsD :: Int -> DefT -> (DefT, Int)
      enumPortsD i (DefT v node) =
        let (node', i') = enumPortsN i node
        in  (DefT v node', i')
      enumPortsD i (ActualsT v m bl) =
        let (bl', i') = threadfunc_l i bl enumPortsN
        in  (ActualsT v m bl', i')
      enumPortsN :: Int -> NodeT -> (NodeT, Int)
      enumPortsN i (OpT c nls) =
        let (nls', i') = threadfunc_l i nls enumPortsNL
        in  (OpT c nls', i')
      enumPortsN i (IfT b0 b1 b2) =
        let (b0', i0') = enumPortsN i   b0
            (b1', i1') = enumPortsN i0' b1
            (b2', i2') = enumPortsN i1' b2
        in  (IfT b0' b1' b2', i2')
      enumPortsNL :: Int -> NodeLink -> (NodeLink, Int)
      enumPortsNL j ve@(VarEdge v edge) =
        if edge /= Nothing then error $ "VarEdge already enumerated: "++(pprint ve "")
        else (VarEdge v (Just j), j+1)
      enumPortsNL j ve@(VarCall v edge index) =
        if edge /= Nothing then error $ "VarEdge already enumerated: "++(pprint ve "")
        else (VarCall v (Just j) index, j+1)
      enumPortsM :: Int -> MOp -> (MOp, Int)
      enumPortsM i (MOp c ops) =
        let (ops', i') = threadfunc_l i ops enumPortsM
        in  (MOp c ops', i')
      enumPortsM i (MNL nlink) =
        let (nlink', i') = enumPortsNL i nlink
        in  (MNL nlink', i')
      enumPortsMD :: Int -> MOpDef -> (MOpDef, Int)
      enumPortsMD i (mopN, mop) =
        let (mop', i') = enumPortsM i mop
        in  ((mopN, mop'), i')
      (defs2, idefs2) = threadfunc_l 0 defs enumPortsD
      (mops2, _) = threadfunc_l idefs2 mops enumPortsMD
  in  ProgT defs2 mops2
-- if the 2nd arg of ProgT is not [], what to do with these extra mops?      
enumPorts (ProgT _ _) _ = error "enumPorts: program alrady suffixed by merged ops"

{--
TODO: is this still needed? Do we have to put it in a separate "interpreter" module?

type Demand = (QName, Ctxt, Port)
type Response = (QName, Ctxt, Port, Value)

-- | An eval() to simulate the dataflow graph.
evalTTD :: ProgT -> Value
evalTTD (ProgT _ defs) =
  let lookup :: QName -> [DefT] -> IIndex -> (ExprT, Ctxt)
      lookup v ((DefT v' e) : ds)    ctxt = if v==v' then (e, ctxt) else lookup v ds i
      lookup v ((ActualsT v' el):ds) ctxt = if v==v' then (el!!(hd ctxt), tl ctxt) else lookup v ds ctxt
      eval (XT v port) ctxt = sendDemand v ctxt port
      sendDemand :: Demand -> () -- ???
      sendDemand (v, ctxt, port) = 
-}
