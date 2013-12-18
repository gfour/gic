-- | The TTD prototype that compiles TTD graphs to Erlang code.
-- 

{-
-- | Finds all the code fragments between ports in a TTD definition. These code fragments
--   can be used for code generation for receiving data in each port. Special port 0 is
--   the function entry ("variable demand" in intensional terms).
fragmentsD :: DefT -> [(Port, ExprT)]
fragmentsD def =
  let -- finds the next code fragment until the next port, then returns it,
      -- together with the rest of the expression
      getNextFragment :: ExprT -> ((Port, ExprT), Maybe ExprT)
      getNextFragment eN@(XT _ port) = ((port, eN), Nothing)
      getNextFragment (FT q e) =
        let ((port', e'), rest') = getNextFragment e
        in  ((port', FT q e'), rest')
      -- TODO: how to treat if?
      getNextFragment (ConT "if" [e0, e1, e2]) =
        let ((port', e0'), rest') = getNextFragment e0
        in  ((port', ConT "if" [e0', e1, e2]), rest')
      -- getNextFragment eN = ((0, XT "A" 0), Nothing) -- 
      getNextFragment eN = error $ "??? "++(show eN)
      -- gets all the fragments of an expression
      aux e = case getNextFragment e of
                (e', Just r') -> e' : (aux r')
                (e', Nothing) -> [e']
  in  case def of
        DefT _ (_, e) -> aux e
        ActualsT _ el -> concatMap aux (map snd el)
        
fragments :: ProgT -> ShowS
fragments (ProgT _ defs) =
  shows (map fragmentsD defs)

-- | Translate a TTD program to Erlang code.
makeTTD :: ProgT -> TTDGraph -> ShowS
makeTTD (ProgT [] defs) =
  error "makeTTD: TODO"
  -- makeTTDPreamble defs . foldDot makeTTDDef defs
makeTTD (ProgT (_:_) _) =
  error "The TTD back-end does not support data type declarations."
      
-- | Construct the dataflow graph from a TTD program with already numbered ports.
analyzeTTD :: [DefT] -> TTDGraph
analyzeTTD defs =
  let analyzeTTD_aux :: [DefT] -> TTDGraph -> TTDGraph
      analyzeTTD_aux [] graph = graph
      analyzeTTD_aux (def':defs') graph =
        case def' of
          DefT v (_, e) -> analyzeTTD_aux defs' ((v, (findNodes e)) : graph)
          ActualsT v bl -> analyzeTTD_aux defs' ((v, (concatMap findNodes (map snd bl))) : graph)
      findNodes :: ExprT -> [VPort]
      findNodes (XT v port)     = [(v, port)]
      findNodes (ConT _ el)     = concatMap findNodes el
      findNodes (FT (Call _) e) = findNodes e
      findNodes (IfT (_, e0) (_, e1) (_, e2)) = findNodes e0 ++ findNodes e1 ++ findNodes e2
      findNodes e = ierr $ "findNodes analysis found construct: "++(show e)
  in  analyzeTTD_aux defs []

-- | Generates process names from definition variable names.
nodeName :: VName -> ShowS
nodeName v = ("node_"++).(v++)

-- | Translate a ZOIL definition to Erlang code.
makeTTDDef :: DefD -> ShowS
makeTTDDef def =
  let defHeader :: VName -> ShowS
      defHeader v = nodeName v.("() ->"++).nl
      whLookup :: ShowS
      whLookup = ("io:fwrite(\"TODO: warehouse lookup\"\\n),"++).nl
  in  case def of 
        DefD v e ->
          defHeader v.
          tab.("receive"++).nl.
          tab.tab.("{demand, Pid, Ctxt} ->"++).nl.
          tab.tab.tab.whLookup.
          tab.tab.tab.makeTTDExpr e.nl.
          tab.("end."++).nl.nl
        -- ignore empty actuals, since Erlang is dynamic, no compilation error will occur
        ActualsD _ [] -> id  
        ActualsD v el ->
          let makeActual (i, e) = tab.tab.tab.shows i.(" -> "++).makeTTDExpr e.semi.nl
          in  defHeader v.
              tab.("receive"++).nl.
              tab.tab.("{demand, Pid, [I|Ctxt]} ->"++).nl.
              tab.tab.tab.whLookup.
              tab.tab.tab.("case I of"++).nl.
              foldDot makeActual (zip [0 .. (length el - 1)] el).
              tab.tab.tab.("end."++).nl.
              tab.("end."++).nl.nl
  
-- | The preamble of the Erlang prototype code.
makeTTDPreamble :: [DefD] -> ShowS
makeTTDPreamble defs =
  let caseNode :: (Integer, DefD) -> ShowS
      caseNode (i, def) = tab.shows i.(" -> spawn(fun() -> "++).nodeName (defVarD def).("(WH) end);"++).nl
  in  ("%% For each function 0..N of the program, spawn a process and return its PID."++).nl.    
      ("spawnNode(I, WH) -> "++).nl.
      tab.("case I of "++).nl.
      foldDot caseNode (zip [0..] defs).
      tab.("Default -> io:fwrite(\"Process ~p does not exist.\", [I])"++).
      tab.("end."++).nl.
      nl.
      ("init() ->"++).nl.
      tab.("WH = io:fwrite(\"Spawning warehouse...\"), spawn(whouse),"++).nl.
      tab.("io:fwrite(\"Spawning "++).shows (length defs).(" nodes...\"), "++).
      ("map((fun(I) -> spawnNode(I, WH) end), array:new("++).shows (length defs).("))"++).nl.      
      ("end."++).nl.nl
            
-- | Translate a ZOIL definition to Erlang code.
makeTTDExpr :: ExprD -> ShowS
makeTTDExpr (XD v port) =
  (v++).(" ! {demand, Ctxt, "++).shows port.(", self()}"++)
makeTTDExpr (ConD n []) = ("Pid ! {value, Ctxt, "++).(n++).("}"++)
makeTTDExpr (ConD c _) = ierr $ "makeTTDExpr: unknown constant op "++c
makeTTDExpr (FD q (XD f port)) =
  case q of
    NOp -> ierr "makeTTDExpr: intensional operator == NOp, is this normal?"
    Call i -> (f++).(" ! {demand, ["++).(shows i).("|Ctxt], "++).shows port.(", self()}"++)
makeTTDExpr e = ierr $ "Cannot translate to dataflow: " ++ (show e)

-- | Combine two parallelism constructs in a new Para construct.
sfCombinePara :: ExprT -> ExprT -> ExprT
sfCombinePara a@(Sequ _) b@(Sequ _) = Para [a, b]
sfCombinePara (Para a) (Para b)     = Para (a++b)
sfCombinePara a@(Sequ _) (Para b)   = Para ([a]++b)
sfCombinePara (Para a) b@(Sequ _)   = Para (a++[b])
sfCombinePara _ _ = error "sfCombinePara works only for Para and Sequ constructs"

-- | Combine two parallelism constructs in a new Sequ construct.
sfCombineSequ :: ExprT -> ExprT -> ExprT
sfCombineSequ (Sequ a) (Sequ b)     = Sequ (a++b)
sfCombineSequ a@(Para _) b@(Para _) = Sequ [a, b]
sfCombineSequ a@(Para _) (Sequ b)   = Sequ ([a]++b)
sfCombineSequ (Sequ a) b@(Para _)   = Sequ (a++[b])
sfCombineSequ _ _ = error "sfCombineSequ works only for Para and Sequ constructs"

-- | The sequencer is a transformation that makes evaluation order explicit and sets
--   up the layout for the ports handling.
-- TODO: do port-enumeration
sequencer :: ExprT -> ExprT
sequencer e@(XT _ _) = e
sequencer e@(FT _ (XT _ _)) = e
sequencer e@(FT _ _) = error $ "Found intensional operator applied to expression: "++(show e)
-- The sequencer assumes that all binary built-in operations are strict.
sequencer e@(ConT c el) =
  let depends :: ExprT -> Maybe ExprT
      depends (ConT _ []) = Nothing
      depends (ConT _ [e1, e2]) = case (depends e1) of 
                                    Nothing      -> depends e2
                                    d1@(Just j1) -> case (depends e2) of
                                                      Nothing -> d1
                                                      Just j2 -> Just $ sfCombinePara j1 j2
      depends (ConT "if" [e0, _, _]) = depends e0
      depends eN@(ConT _ _) = error $ "depends() found unknown built-in: "++(show eN)
      depends eN@(XT _ _) = Just $ Sequ [eN]
      depends eN@(FT _ _) = Just $ Sequ [eN]
      depends eN = error $ "depends() doesn't process expressions like "++(show eN)
      strictBinOpRes e1 e2 se =
        case depends e1 of
          Nothing ->
            case depends e2 of
              Nothing -> e
              Just j2 -> Sequ [j2, e]
          Just j1 ->
            case depends e2 of
              Nothing -> Sequ [j1, se]
              Just j2 -> Sequ [sfCombinePara j1 j2, se]
  in  case c of
        "+"  -> let [e1, e2] = el in  strictBinOpRes e1 e2 e
        "-"  -> let [e1, e2] = el in  strictBinOpRes e1 e2 e
        "<"  -> let [e1, e2] = el in  strictBinOpRes e1 e2 e
        "div"-> let [e1, e2] = el in  strictBinOpRes e1 e2 e
        "if" -> let aux e' = case depends e' of
                               Nothing  -> e'
                               Just dep -> sfCombineSequ dep (Sequ [e'])
                in  ConT "if" (map aux el)
        _    -> case el of
                     [] -> e
                     _  -> error $ "The sequencer found unknown built-in op: "++c
sequencer e@(Para _)    = error $ "The sequencer found already introduced Para[]: "++(show e)
sequencer e@(Sequ _)    = error $ "The sequencer found already introduced Sequ[]: "++(show e)

-- | Compacts "Sequ"/"Para" constructs with too few arguments.
compact :: ProgT -> ProgT
compact (ProgT ds defs) =
  let compactD (DefT v (inps, e)) = DefT v (inps, compactE e)
      compactD (ActualsT v el) = ActualsT v (map (\(inps, e)->(inps, compactE e)) el)
      compactE e@(XT _ _) = e
      compactE (FT q f) = FT q (compactE f)
      compactE (ConT c el) = ConT c (map compactE el)
      compactE (Sequ [e]) = compactE e
      compactE (Sequ el) = Sequ (map compactE el)
      compactE (Para [e]) = compactE e
      compactE (Para el) = Para (map compactE el)
  in  ProgT ds (map compactD defs)

-- | Sequences the evaluation order to make explicit the locations where ports 
--   should be created for interconnection.
sequenceProg :: ProgT -> ProgT
sequenceProg (ProgT ds defs) =
  let fromTTDtoSd (DefT v e) = DefT v (sequencer e)
      fromTTDtoSd (ActualsT v el) = ActualsT v (map sequencer el)
  in  ProgT ds (map fromTTDtoSd defs)

-- | Annotates all boxes with their input ports.
updateBoxPorts :: ProgT -> ProgT
updateBoxPorts (ProgT ds defs) =
  let updD :: DefT -> DefT
      updD (DefT v ([], e)) = DefT v (updBoxOf e)
      updD (ActualsT v bl) = ActualsT v (map (\(_, be)->updBoxOf be) bl)
      updD def = error $ "updD: definition seems to have box ports, "++(show def)
      updBoxOf e = (gatherPorts e, updE e)
      updE e@(XT _ _) = e
      updE (ConT c el) = ConT c (map updE el)
      updE (FT q e) = FT q (updE e)
      updE (IfT ([], e) ([], e1) ([], e2)) = IfT (updBoxOf e) (updBoxOf e1) (updBoxOf e2)
      updE _ = error "updE: unsupported expression, are the boxes already updated?"
  in  ProgT ds (map updD defs)
-}
