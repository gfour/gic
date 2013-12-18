module SLIC.Front.LLifter.Lifter (lambdaLiftMod) where

import SLIC.AuxFun (ierr, lkUpSure)
import SLIC.Front.LLifter.Equations as Eqn
import SLIC.State
import SLIC.SyntaxAux
import SLIC.SyntaxFL
import SLIC.Types

import Data.List as List (concat, foldl, map, unzip, zip)
import Data.Set as Set (Set, delete, difference, elems, empty, fromList,
                        intersection, map, null, singleton, union)
import Data.Map as Map (Map, adjust, delete, empty, findWithDefault,
                        foldWithKey, fromList, insert, insertWith, keys,
                        map, null)

type Renamings = Map QName QName
type VarGrh = Map QName (Set QName)

-- | Same as Map.insert but raises an error when given key
--   is already present in the map so that silent updates
--   are avoided.
insertUnique :: Ord k => k -> a -> Map k a -> Map k a
insertUnique k a m = 
  Map.insertWith (\_ _ -> ierr $ "key already present in map") k a m

-- | Take a map from varnames to varname sets, a defined name and
--   an FL expression and add the free vars of the expression
--   to the set associated with the given defined name
fvExprF :: VarGrh -> QName -> ExprF -> VarGrh
fvExprF vg vn exprF =
  let augmentWith s              = Map.insertWith Set.union vn s vg
      removeDefinedNames defFL s = Set.difference s $ Set.fromList $
                                                List.map defVarName defFL
  in  case exprF of
        XF v        -> augmentWith $ Set.singleton $ nameOfV v
        ConF _ exprFL -> List.foldl (\vg' -> fvExprF vg' vn) vg exprFL
        FF v exprFL -> List.foldl (\vg' -> fvExprF vg' vn) 
                                  (augmentWith $ Set.singleton $ nameOfV v)
                                  exprFL
        ConstrF _ exprFL -> List.foldl (\vg' -> fvExprF vg' vn) vg exprFL
        CaseF _ exprF' vname patFL -> Map.adjust (Set.delete vname) vn $
                                        List.foldl (\vg' -> fvPatF vg' vn) 
                                              (fvExprF vg vn exprF') patFL
        LamF _ vname exprF' -> Map.adjust (Set.delete vname) vn $
                                          fvExprF vg vn exprF'
        LetF _ defFL exprF' -> Map.adjust (removeDefinedNames defFL) vn $
                                  fvExprF
                                      (List.foldl (\vg' -> fvDefF vg' vn) 
                                                    vg defFL)
                                      vn exprF'

-- | Take a map from varnames to varname sets, a defined name and
--   an FL pattern and add the free vars of the pattern
--   to the set associated with the given defined name
fvPatF :: VarGrh -> QName -> PatF -> VarGrh
fvPatF vg vn (PatF (SPat _ vnameL) exprF) =
  Map.adjust (\s1 -> Set.difference s1 $ Set.fromList vnameL) 
              vn $ fvExprF vg vn exprF

-- | Take a map from varnames to varname sets and a definition
--   and augment the map so that it associates the defined name
--   with the set of the names of the free variables appearing in its body
fvDefF :: VarGrh -> QName -> DefF -> VarGrh
fvDefF vg vn (DefF vname frmL exprF) =
  let vg' = Map.adjust 
              (\s1 -> Set.difference s1 $ Set.fromList $ vname:(frmsToNames frmL))
              vname $ fvExprF (insertUnique vname Set.empty vg) vname exprF
  in Map.insertWith Set.union vn 
                    (lkUpSure vname vg') vg'

-- | Take an FL program and return a map that associates the defined
--   names with the free variables appearing in their definitions
fvProgF :: ProgF -> VarGrh
fvProgF (Prog _ defFL) = 
  Map.delete pFV $
  Map.adjust 
    (\s1 -> Set.difference s1 $ Set.fromList $ List.map defVarName defFL)
    pFV $ List.foldl (\vg' -> fvDefF vg' pFV) Map.empty defFL

pFV :: QName
pFV = QN Nothing "*" -- special name representing the set of the program's free vars

-- | a-rename an FL expression using the given map associating
--   names with names. Constructor and constant names are preserved
aRenameExprF :: Renamings -> ExprF -> ExprF
aRenameExprF ren exprF =
  case exprF of
    XF v -> XF . V $ Map.findWithDefault (nameOfV v) (nameOfV v) ren
    ConF cName exprFL -> 
        ConF cName $ List.map (aRenameExprF ren) exprFL
    FF v exprFL -> 
        FF (V $ Map.findWithDefault (nameOfV v) (nameOfV v) ren) $
           List.map (aRenameExprF ren) exprFL
    ConstrF cstrName exprFL -> 
        ConstrF cstrName $ List.map (aRenameExprF ren) exprFL
    CaseF depth exprF' vname patFL -> 
      let vname' = Map.findWithDefault vname vname ren in
        CaseF depth (aRenameExprF ren exprF') vname' $ 
              List.map (aRenamePatF ren) patFL
    LamF depth vname exprF' ->
      let vname' = Map.findWithDefault vname vname ren in
        LamF depth vname' $ aRenameExprF ren exprF'
    LetF depth defFL exprF' -> 
      let defFL' = List.map (aRenameDefF ren) defFL
      in  LetF depth defFL' $ aRenameExprF ren exprF'

-- | a-rename an FL pattern using the given map associating
--   names with names. Constructor and constant names are preserved
aRenamePatF :: Renamings -> PatF -> PatF
aRenamePatF ren (PatF (SPat cstrName vnameL) exprF) = 
  PatF (SPat cstrName (List.map (\vn -> Map.findWithDefault vn vn ren) vnameL)) $
       aRenameExprF ren exprF

-- | a-rename an FL definition using the given map associating
--   names with names. Constructor and constant names are preserved
aRenameDefF :: Renamings -> DefF -> DefF
aRenameDefF ren (DefF vname frmL exprF) =
  DefF (Map.findWithDefault vname vname ren)
       (List.map 
          (\(Frm vn stc) -> Frm (Map.findWithDefault vn vn ren) stc)
          frmL) $
        aRenameExprF ren exprF

-- | Take an expression and a map associating function names with
--   the extra parameters to be applied on them first and amend all
--   function applications
--   ** all names are supposed to be unique at this point **
preApplyExprF :: VarGrh -> ExprF -> ExprF
preApplyExprF vg exprF =
  case exprF of
    XF v -> 
      let extVarsS = Map.findWithDefault Set.empty (nameOfV v) vg
      in  if Set.null extVarsS then exprF 
          else FF v $ List.map (XF . V) $ Set.elems extVarsS
    ConF cName exprFL -> 
        ConF cName $ List.map (preApplyExprF vg) exprFL
    FF v exprFL -> 
      let exprFL'  = List.map (preApplyExprF vg) exprFL
          extVarsS = Map.findWithDefault Set.empty (nameOfV v) vg
      in  if Set.null extVarsS then FF v exprFL'
          else FF v $ (List.map (XF . V) $ Set.elems extVarsS) ++ exprFL'
    ConstrF cstrName exprFL -> 
        ConstrF cstrName $ List.map (preApplyExprF vg) exprFL
    CaseF depth exprF' vname patFL -> 
        CaseF depth (preApplyExprF vg exprF') vname $ 
          List.map (\(PatF sPat exprF'') -> 
                        PatF sPat $ preApplyExprF vg exprF'')
                   patFL
    LamF depth vname exprF' ->
        LamF depth vname $ preApplyExprF vg exprF'
    LetF depth defFL exprF' -> 
      let defFL' = List.map (preApplyDefF vg) defFL
      in  LetF depth defFL' $ preApplyExprF vg exprF'

-- | Take a definition and a map associating function names with
--   the extra parameters to be applied on them first and amend all
--   function applications
--   ** all names are supposed to be unique at this point **
preApplyDefF :: VarGrh -> DefF -> DefF
preApplyDefF vg (DefF vname frmL exprF) = 
  DefF vname frmL $ preApplyExprF vg exprF

-- | Take a definition and a map associating function names with
--   the extra parameters to be applied on them first and amend all
--   function applications
--   ** all names are supposed to be unique at this point **
preApplyProgF :: VarGrh -> ProgF -> ProgF
preApplyProgF vg (Prog dataL defFL) = 
  Prog dataL $ List.map (preApplyDefF vg) defFL


-- | Take a definition and a list of extra formals, add the
--   extra formals to the definition and a-rename (the formals
--   and the bound variables in the definition body
abstractDefWithFormals :: DefF -> [Frm] -> DefF
abstractDefWithFormals (DefF vname frmL exprF) frmL' =
  let frmNameL'       = frmsToNames frmL'
      renFrmNameL'    = List.map (\x -> procLName (\ln->ln ++ "_" ++ qName vname ++ "_Lifted") x)
                                 frmNameL'
      ren             = Map.fromList $ List.zip frmNameL' renFrmNameL'
  in aRenameDefF ren $ DefF vname (frmL'++frmL) exprF 

-- | Add extra formals to FL definitions recursively
abstractDefsDefF :: VarGrh -> DefF  -> DefF
abstractDefsDefF vg (DefF vname frmL exprF) = 
  let frmL' = List.map (\vn -> Frm vn False) $ Set.elems $
                                  lkUpSure vname vg
  in  abstractDefWithFormals 
        (DefF vname frmL $ abstractDefsExprF vg exprF) frmL'

-- | Add extra formals to FL definitions recursively
abstractDefsExprF :: VarGrh -> ExprF -> ExprF
abstractDefsExprF vg exprF =
  case exprF of
    XF _ -> exprF
    ConF cName exprFL -> 
      ConF cName $ List.map (abstractDefsExprF vg) exprFL
    ConstrF cstrName exprFL -> 
      ConstrF cstrName $ List.map (abstractDefsExprF vg) exprFL
    FF v exprFL -> FF v $ List.map (abstractDefsExprF vg) exprFL
    CaseF depth exprF' vname patFL ->
      let abstractDefsPatF (PatF sPat exprF'') =
              PatF sPat $ abstractDefsExprF vg exprF''
      in CaseF depth (abstractDefsExprF vg exprF') vname $
            List.map abstractDefsPatF patFL
    LamF depth vname exprF' -> LamF depth vname $ 
                                    abstractDefsExprF vg exprF'
    LetF depth defFL exprF' -> 
      let defFL' = List.map (abstractDefsDefF vg) defFL
      in LetF depth defFL' $ abstractDefsExprF vg exprF'
  
-- | Add extra formals to FL definitions recursively
abstractDefsProgF :: VarGrh -> ProgF -> ProgF
abstractDefsProgF vg (Prog dataL defFL) =
  Prog dataL $ List.map (abstractDefsDefF vg) defFL

-- | Take an expression and return the a tuple containing 1) all the
--   definitions that are nested in the original expression and 2) an
--   expression which is the original one with all nested definitions
--   removed
liftDefsExprF :: ExprF -> ([DefF], ExprF)
liftDefsExprF exprF = 
  case exprF of
    XF _ -> ([], exprF)
    ConF cName exprFL -> 
      let (defFLL, exprFL') = List.unzip $ List.map liftDefsExprF exprFL
      in (List.concat defFLL, ConF cName exprFL')
    ConstrF cstrName exprFL  -> 
      let (defFLL, exprFL') = List.unzip $ List.map liftDefsExprF exprFL
      in (List.concat defFLL, ConstrF cstrName exprFL')
    FF v exprFL ->
      let (defFLL, exprFL') = List.unzip $ List.map liftDefsExprF exprFL
      in (List.concat defFLL, FF v exprFL')
    CaseF depth exprF' vname patFL -> 
      let liftDefsPatF (PatF sPat exprF'') = 
            let (defFL', exprF''') = liftDefsExprF exprF''
            in (defFL', PatF sPat exprF''')
          (defFLL, patFL') = List.unzip $ List.map liftDefsPatF patFL
          (defFL, exprF'''') = liftDefsExprF exprF'
      in (defFL ++ (List.concat defFLL), CaseF depth exprF'''' vname patFL')
    LamF depth vname exprF' -> 
      let (defFL, exprF'') = liftDefsExprF exprF'
      in (defFL, LamF depth vname exprF'')
    LetF _ defFL exprF' -> 
      let (defFL', exprF'') = liftDefsExprF exprF'
          defFL'' = List.concat $ List.map liftDefsDefF defFL
      in (defFL'' ++ defFL', exprF'')

-- | Take a definition and return the a list of definitions which
--   contains all nested definition and the original one with
--   all nested definitions removed
liftDefsDefF :: DefF -> [DefF]
liftDefsDefF (DefF vname frmL exprF) =
  let (defFL, exprF') = liftDefsExprF exprF
  in  (DefF vname frmL exprF'):defFL

-- | Take an FL program and lift all definitions at top level
--   ** all definitions are supposed to be combinators 
--      i.e. they should contain no free variables at this point **
liftDefsProgF :: ProgF -> ProgF
liftDefsProgF (Prog dataL defFL) =
  --let liftDefsDefF (DefF vname frmL exprF) =
  --      let (defFL', exprF') = liftDefsExprF exprF
  --      in  (DefF vname frmL exprF'):defFL'
  --in  
  Prog dataL $ List.concat $ List.map liftDefsDefF defFL

-- | Take a map associating defined names with free variable names
--   and a set of defined names and return an equation system
--   between defined names and variables to abstracted out of
--   definition bodies (as described by Johnsson)
--
--   * note that normally: defnameS == Set.fromList $ Map.keys vg
mkEquationsJ :: VarGrh -> Set QName -> EqnSys QName (EqE QName QName)
mkEquationsJ vg defnameS =
  let --knowns vn knownS = Set.map EqV $ Set.intersection knownS $ Map.
      f vn fvS eqnSys = Map.insert vn
                          (Set.union 
                                (Set.map (\vn' -> EqU vn') $
                                        Set.intersection fvS defnameS) $
                                Set.map (\vn' -> EqV vn') $ 
                                        Set.difference fvS defnameS)
                          eqnSys
  in Map.foldWithKey f Map.empty vg

-- | Take an FL program and returned a corresponding lambda-lifted
--   FL program containing no abstractions and, more generally,
--   no local definitions. Lambda-lifting is performed in accordance
--   with Johnsson
lambdaLiftProgF :: ProgF -> [QName] -> ProgF
lambdaLiftProgF p llExcluded =
  let defFV = Map.map 
                (\s -> Set.difference s $ 
                   Set.fromList llExcluded)
                $ fvProgF p
      eqnSys = mkEquationsJ defFV $ Set.fromList $ Map.keys defFV
      solvedEqnSys = Eqn.solveEqs eqnSys
      solvedEqnSysV = Map.map (Set.map $ \(EqV x) -> x) solvedEqnSys
  in liftDefsProgF $ 
        abstractDefsProgF solvedEqnSysV $ 
          preApplyProgF solvedEqnSysV p

-- | This is the entry point of the lambda-lifter. It takes an FL module and
--   eliminates all let-bindings, introducing new top-level definitions.
lambdaLiftModF :: ModF -> ModF
lambdaLiftModF (Mod modName exports imports progF an ts) =
  let importedNames = mergeImportFuns imports
      vfnL          = Map.keys importedNames
      llExcluded    = vfnL ++ cBuiltinFuncs
  in  Mod modName exports imports (lambdaLiftProgF progF llExcluded) an ts

-- | Runs the lambda lifter on a list of FL modules. Checks that the lifter
--   can be used with the type checking scheme selected.
lambdaLiftMod :: Options -> ModF -> ModF
lambdaLiftMod opts modF =
  let usesLifter = hasLs $ modProg modF
      hasTSigs   = Map.null $ modTAnnot modF
      lliftedMod = lambdaLiftModF modF
      lliftedModIfNotL =
        if usesLifter && hasTSigs then
          error "lambda-lifting doesn't support type signatures, use -gic-tc-nsig"
        else
          lliftedMod
  in  case optTC opts of
        GHCTypeInf       -> lliftedModIfNotL
        GICTypeInf True  -> lliftedModIfNotL
        GICTypeInf False -> lliftedMod
