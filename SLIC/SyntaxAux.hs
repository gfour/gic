-- | Common things for the syntax of all intermediate languages.
-- 

module SLIC.SyntaxAux where

import Data.List (intersperse)
import Data.Map (Map, filterWithKey, fromList,
                                keys, member, unions)
import qualified Data.Map as M (filter, lookup, null, toList)
import Data.Maybe (isJust)
import qualified Data.Set as S (Set, empty, toList)
import Data.Tree (Tree(..), drawForest, drawTree)
import SLIC.AuxFun (ierr, foldDot, insCommIfMore, showStrings, spaces)
import SLIC.Constants (mControlParallelN, tcMod, lparen, rparen, nl, semi)
import SLIC.Types

-- * User-defined data types

-- | Data type names in constructor declarations.
data DT =
    DT Type EvOrder (Maybe QName) -- ^ data type name with an evaluation
                                  -- order annotation and an optional selector
    deriving (Eq, Read, Show)

instance PPrint DT where
  pprintPrec _ (DT dt s sel) =
    pprint s.
    (case sel of Nothing -> id ; Just qn -> pprint qn.(" :: "++)).
    pprint dt

-- | Data constructor declaration. Contains a constructor name, the constructor
--   components and the return type (if it is a GADT, this may be different
--   for constructors of the same type, for regular data types it's Nothing).
data DConstr = 
    DConstr CstrName [DT] (Maybe Type)
    deriving (Eq, Read, Show)

instance PPrint DConstr where
  pprintPrec _ (DConstr c cs t) = 
    let hasNamedFields = containSelectors cs
        delimDT = if hasNamedFields then (" ; "++) else (" "++)
    in  pprint c.spaces 1.
        (if hasNamedFields then ("{ "++) else id).
        showTyps cs delimDT.
        (if hasNamedFields then (" }"++) else id).
        (case t of Nothing -> id ; Just t' -> (" -> "++).pprint t')

-- | Shows a list of data type names.
showTyps :: [DT] -> ShowS -> ShowS
showTyps [] _ = id
showTyps ds delimDT = foldDot id (intersperse delimDT (map pprint ds))

-- | Check if a constructor is a record (i.e. if it contains a named component).
isRecord :: DConstr -> Bool
isRecord (DConstr _ dts _) = containSelectors dts
  
-- | Check if a list of data type components contains a selector.
containSelectors :: [DT] -> Bool
containSelectors dts = any (\(DT _ _ sel)->isJust sel) dts

-- | Data type declarations. Contain the data type name, a list of binder
--   variables, and the list of constructors.
data Data =
    Data DTName [SName] [DConstr]
    deriving (Eq, Read, Show)

instance PPrint Data where
   pprintPrec _ (Data d as cs) =
       let pprintConstrs dcs =
             foldDot id $ map (\x->nl.spaces 2.(" | "++).pprint x) dcs
       in  ("data "++).pprint d.(" "++).showStrings " " as.
           (" = "++).pprintConstrs cs.("\n"++)

-- | The projection of a constructor name.
dcName :: DConstr -> CstrName
dcName (DConstr c _ _) = c

-- | Calculates the numerical IDs that can be assigned to constructor names.
calcCIDs :: [Data] -> CIDs
calcCIDs ds =
  let gatherDT (Data _ _ dtcs) = map (\(DConstr c cs _) -> (c, length cs)) dtcs
      constrs = concatMap gatherDT ds
      aux ((c, ar), cid) = (c, (ar, cid))
  in  fromList $ map aux $ zip constrs [1..(length constrs)]

-- * Pattern annotation

-- | A flag that shows if a costructor binds variables in an expression.
type BindsVars = Bool

-- | Information stored about a pattern.
data PatInfo = PatInfo BindsVars
             deriving (Eq, Read, Show)

instance PPrint PatInfo where
  pprint (PatInfo b) = if b then ("#"++) else spaces 1

-- | A pattern branch bundles a pattern with accompanying information and an
--   expression.
data PatB p e = PatB (p, PatInfo) e
             deriving (Eq, Read, Show)

instance (PPrint p, PPrint e) => PPrint (PatB p e) where
  pprint (PatB (p, b) e) = spaces 1.pprint p.spaces 1.pprint b.("-> "++).pprint e

-- * Programs

-- | A program with data definitions and function definitions (parameterized).
data Prog a = Prog { progData :: [Data], progDefs :: [a] }
            deriving (Eq, Read, Show)

instance (PPrint a) => PPrint (Prog a) where
   pprintPrec p (Prog cs ds) =
      foldl (\f d -> f.pprint d) id cs .
      pprintDefs p ds id

-- | Pretty printer for definitions. Takes the depth of indentation, the
--   list of elements to print, and the spacer.
pprintDefs :: (PPrint a) => Int -> [a] -> ShowS -> ShowS
pprintDefs _ [] _ = id
pprintDefs _ [def] _ = pprint def.nl
pprintDefs d (def:ds) spc =
  pprint def.nl.
  spaces d.foldl (\f0 d0 -> spc.f0.pprint d0.nl) id ds

-- | Concatenates many programs to a single one (concatenates data declarations
--   and top-level definitions).
concatProgs :: [Prog a] -> Prog a
concatProgs progs =
  let dts'  = concatMap (\(Prog dts    _)-> dts) progs
      defs' = concatMap (\(Prog   _ defs)->defs) progs
  in  Prog dts' defs'

-- | Given a list of modules, concatenates their program parts and returns the
--   resulting program. Used when feeding the back-ends with code without module
--   boundaries.
concatCode :: [Mod (Prog a)] -> (Prog a)
concatCode mods = concatProgs (map modProg mods)

-- | Pretty printer that takes a \"location\" arg to insert whitespace.
pprint_tab :: (PPrint a) => PMDepth -> a -> ShowS
pprint_tab d pat = spaces (2*d).pprint pat

-- | Pretty printer of semicolon-terminated tabbed lists. Used for patterns.
pprint_tab_l :: (PPrint a) => PMDepth -> [a] -> ShowS
pprint_tab_l _ []         = ("{- nothing -}"++).nl
pprint_tab_l d [pat]      = pprint_tab d pat.nl
pprint_tab_l d (pat : ps) = pprint_tab d pat.semi.nl.pprint_tab_l d ps

-- | Shows a constructor name.
pprintTH :: CstrName -> ShowS
pprintTH c = pprint c

-- * Import declarations

-- | Prints a Maybe value that should not be missing.
pprintMaybe :: (PPrint a) => Maybe a -> ShowS
pprintMaybe (Nothing) = ("<missing>"++)
pprintMaybe (Just t)  = pprint t

-- | An imported name can be a function, a constructor, or a data type name.
data NInfo = NFunc | NConstr | NDType deriving (Eq, Show)

-- | Information for an imported name: type, arity, CAF position, nesting.
--   For functions that do not belong to a type class, the type/arity/CAF/depth
--   components are initialized to Nothing during parsing; they get their
--   real value later, when reading the DFI of the imported module.
data IInfo =
  IInfo { impT :: Maybe Type         -- ^ type
        , impA :: Maybe Arity        -- ^ arity
        , impC :: NInfo              -- ^ what kind of name it is
        , impCAF :: Maybe CAFId      -- ^ global CAF index if it is a CAF
        , impD :: Depth              -- ^ pattern matching depth
        , impStricts :: Maybe StrictInds -- ^ strict parameter positions
        }
  deriving (Eq, Show)

-- | A mapping from imported names to information for compilation.
type ImportedNames = Map QName IInfo

-- | Pretty printer for compilation information of imported names.
pprintINames :: ImportedNames -> ShowS
pprintINames impNames =
  let pprintIN (v, IInfo {}) =
        pprint v -- .("::"++).pprintMaybe it
      --   pprint v.("::"++).pprintMaybe it.showsCAF caf.showsNesting nesting
      -- showsCAF Nothing      = id
      -- showsCAF (Just idx)   = ("[CAF #"++).shows idx.("]"++)
      -- showsNesting Nothing  = id
      -- showsNesting (Just n) = ("[nesting="++).shows n.("]"++)
  in  foldDot id $ intersperse (", "++) $ map pprintIN $ M.toList impNames

-- | An import declaration.
data IDecl =
  IDecl { ideclMName  :: MName         -- ^ imported module name
        , ideclINames :: ImportedNames -- ^ import names (typed symbol table)
        , ideclInfo   :: Maybe (FuncSigs, CIDs) -- ^ signatures/CIDs table
        }

-- | Used to filter out built-in modules, leaving only user-defined module names.
filterRealMods :: [IDecl] -> [MName]
filterRealMods idecls = map ideclMName $ 
  filter (\(IDecl mn _ _)->not $ member mn builtinModules) idecls

instance PPrint IDecl where
  pprintPrec _ (IDecl mn impNames info) =
    ("import "++).(mn++).(" ("++).pprintINames impNames.(")"++).
    (case info of
        Just (fsigs, _) -> (" ["++).pprFSigs fsigs.("]"++)
        Nothing         -> id
    ).nl

-- | Returns all imported names declared in a list of \'import\' declarations.
mergeINames :: [IDecl] -> ImportedNames
mergeINames idms = unions $ map (\(IDecl _ ins _)->ins) idms

-- * Built-in \'import\' declarations

-- | The \'import\' for the built-in Control.Parallel module.
importControlParallel :: IDecl
importControlParallel = genBuiltinIDecl mControlParallelN

-- | Given a built-in module name, generate its import declaration.
genBuiltinIDecl :: MName -> IDecl
genBuiltinIDecl mn =
  let filtModNames bf _ = (lName bf)==mn
      modSigs    = filterWithKey filtModNames builtinFuncSigs
      modTEnv    = filterWithKey filtModNames builtinTEnv
      modCIDs    = filterWithKey filtModNames builtinCIDs
      modConstrs = keys modCIDs
      modFuncs   = filter (\qn->qn `notElem` modConstrs) $ keys modSigs
      names      = (map (mkImp NFunc) modConstrs)++
                   (map (mkImp NConstr) modFuncs)
      mkImp ni n =
        let (nT, Just nAr) = findInfo n modTEnv
            cid = case M.lookup n builtinCIDs of
                    Just (_, cId) -> Just cId
                    Nothing       -> Nothing
            pmDepth = M.lookup n builtinPmDepths
        in  (n, IInfo (Just nT) (Just nAr) ni cid pmDepth (Just S.empty))
  in  IDecl mn (fromList names) (Just (modSigs, modCIDs))

-- | Built-in (pseudo-)modules.
builtinModules :: Map MName IDecl
builtinModules = fromList [ (mControlParallelN, importControlParallel) 
                          , (tcMod, error "!")]

-- * Modules

-- | The name and file path of a module.
type MNameF = (MName, FPath)

-- | The signatures of exported functions.
type Exports = FuncSigs

-- | Pretty printer for export declarations.
pprintExports :: Exports -> ShowS
pprintExports exports = if M.null exports then id else lparen.showStrings ", " (map qName $ keys exports).rparen
--   if null exports then id else lparen.showStrings ", " (map (\(f, fs)->pprint f (show fs)) $ M.toList exports).rparen

-- | Explicit type annotations given by the user (or inserted by the compiler
--   for automatically generated functions).
type TAnnot = TEnv

-- | A module parameterized by the language of its contained program.
data Mod a =
  Mod { modNameF :: MNameF        -- ^ the full name and path
      , modExports :: Exports     -- ^ exported symbol table
      , modImports :: [IDecl]     -- ^ import declarations
      , modProg :: a              -- ^ the program code inside a module
      , modTAnnot :: TAnnot       -- ^ explicit type annotations
      , modTCs :: TcInfo          -- ^ type class information
      }

-- | Pretty printer for modules.
instance (PPrint a) => PPrint (Mod a) where
  pprint (Mod (m, _) exports imports p env tcs) =
    ("module "++).(m++).pprintExports exports.(" where"++).nl.
    foldDot pprint imports.nl.
    pprint p.nl.
    pprint tcs.nl.
    (if M.null env then id else ("-- Type signatures:"++).nl.pprintE env)

-- | Applies a function to the program part of a module (using the
--   module name).
procModSource :: (MName -> a -> a) -> Mod a -> Mod a
procModSource f (Mod fm@(m, _) es is p env tcs) = Mod fm es is (f m p) env tcs

-- | Applies a function to the program part of a list of modules.
procModSources :: (MName -> a -> a) -> [Mod a] -> [Mod a]
procModSources f ms = map (procModSource f) ms

-- * Constants

-- | Built-in operators.
data COp = CPlus     | CMinus    | CMult    | CDivide  | CMod     | CDiv
         | CAnd      | COr       | CEqu     | CNEq
         | CLt       | CGt       | CLe      | CGe      | CMulI    | CIf
         | CNeg      | CTrue     | CFalse   
         deriving (Eq, Ord, Read, Show)

-- | The mapping between the built-in operators and their string representation.
cOps :: Map COp String
cOps = fromList
       [ (CPlus, "+"), (CMinus, "-"), (CMult, "*"), (CDivide, "/")
       , (CMod, "mod"), (CDiv, "div")
       , (CAnd, "&&"), (COr, "||"), (CEqu, "=="), (CNEq, "/=")
       , (CLt, "<"), (CGt, ">"), (CLe, "<="), (CGe, ">="), (CMulI, "mulI")
       , (CIf, "if"), (CNeg, "-"), (CTrue, "True"), (CFalse, "CFalse")
       ]

-- | The built-in boolean operators.
cOpsBool :: [COp]
cOpsBool = [CEqu, CNEq, CGt, CLt, CLe, CGe, CAnd, COr]
              
instance PPrint COp where
  pprintPrec _ c =
    case M.lookup c cOps of
      Just s  -> (s++)
      Nothing ->
        ierr $ "String representation of constant "++(show c)++" not found."

-- | Given a string representation, returns the corresponding 'COp'. If more
--   than one operators correspond to the same representation (such as '-'),
--   the first one found is returned.
cOpForStr :: String -> COp
cOpForStr s =
  case M.toList (M.filter (\s'->s==s') cOps) of
    []       -> ierr $ "No built-in operator for "++s
    (c, _):_ -> c

-- | Built-in constants.
data Const = CN COp            -- ^ named built-in constant
           | LitInt Int        -- ^ integer literal
           deriving (Eq, Ord, Read, Show)

instance PPrint Const where
  pprint (CN cn)    = pprint cn
  pprint (LitInt i) = shows i

-- | Pretty printing for constant operators and literals,
--   used by all languages.
prettyConst :: (PPrint a) => Int -> Const -> [a] -> ShowS
prettyConst p (CN cn) el =
  case cn of
    CIf ->
      showParen (p > 1) (
        ("if " ++)   .pprintPrec 1 (el !! 0).
        (" then " ++).pprintPrec 1 (el !! 1).
        (" else " ++).pprintPrec 1 (el !! 2))
    c | c == CPlus || c == CMinus ->
      showParen (p > 5) (
        pprintPrec 6 (el !! 0) .
        ((" " ++ (pprint c "") ++ " ") ++).pprintPrec 5 (el !! 1))
    CMult ->
      showParen (p > 6) (
        pprintPrec 7 (el !! 0) .
        (" * " ++).pprintPrec 6 (el !! 1))
    c | (c `elem` [CMod, CDiv]) ->
      showParen (p > 6) (
        pprintPrec 7 (el !! 0) .
        ((" `" ++ (pprint c "") ++ "` ") ++).pprintPrec 6 (el !! 1))
    c | (c `elem` [CEqu, CNEq, CLt, CGt, CLe, CGe]) ->
      showParen (p > 4) (
        pprintPrec 5 (el !! 0) .
        ((" " ++ (pprint c "") ++ " ") ++).pprintPrec 5 (el !! 1))
    CAnd ->
      showParen (p > 3) (
        pprintPrec 4 (el !! 0) .
        (" && " ++).pprintPrec 3 (el !! 1))
    COr ->
      showParen (p > 2) (
        pprintPrec 3 (el !! 0) .
        (" || " ++).pprintPrec 2 (el !! 1))
    CNeg ->
      showParen (p > 3) (("-"++).pprintPrec 4 (el !! 0))
    CTrue   -> ("True"++)
    CFalse  -> ("False"++)
    _    ->
      ierr $ "prettyConst: Unhandled built-in constant: "++(pprint cn "")
prettyConst _ (LitInt n) [] = shows n
prettyConst _ c _ =
  ierr $ "prettyConst: Unhandled built-in constant or literal with arity: "++
         (show c)

-- * Type classes

-- | Type classes live in a global namespace.
type TcName = SName

-- | Type class method names.
type MethodName = SName

-- | Type class method signatures.
type MSig = (MethodName, [SName])

-- | Type class method information (signature & type).
type TcMethodInfo = (MSig, Type)

-- | Type class declaration.
data TcDecl = TcDecl TcName TVar [TcMethodInfo]
            deriving (Read, Show)

instance PPrint TcDecl where
  pprint (TcDecl tcn tv methods) =
    let aux ((f, _), t) = spaces 2.(f++).(" :: "++).pprint t.nl
    in  ("class "++).(tcn++).(" "++).(tv++).(" where "++).nl.foldDot aux methods

-- | The signature of a type class instance: the name of the type class, the
--   type for the type variable, and the module where it is defined.
type TcInstSig = ((TcName, Type), MName)

-- | Type class information, per module.
data TcInfo = TcInfo { tcIDecls :: [TcDecl]        -- ^ declarations
                     , tcISigs  :: [TcInstSig]     -- ^ instances
                     }
            deriving (Read, Show)

instance PPrint TcInfo where
  pprint (TcInfo decls insts) =
    foldDot (\x->pprint x.nl) decls.
    foldDot (\((n, t), _)->
              ("declares instance "++).(n++).spaces 1.pprint t.nl) insts

-- | Merge type class information.
mergeTcInfos :: [TcInfo] -> TcInfo
mergeTcInfos tci = TcInfo (concatMap tcIDecls tci) (concatMap tcISigs tci)

-- * Tail calls

-- | A windmill is a list of cycles (axles) and a list of trees rooted in
--   the cycles (blades); it is used by tail calls to implement the required
--   parallel assignment, in order to reuse formals between the old and
--   the new LAR. For more information, read:
-- 
--   L. Rideau, B. P. Serpette, and X. Leroy. ''Tilting at windmills with
--   Coq: formal verification of a compilation algorithm for parallel moves''.
--   Journal of Automated Reasoning, 40(4):307–326, May 2008.
type Windmill = ([Axle], [Blade])

-- | Windmill pretty printer.
pprWindmill :: Windmill -> ShowS
pprWindmill (axles, blades) =
  let pprBlades bls = ((drawForest (map toStrTree bls))++)
  in  ("axles:["++).insCommIfMore (map pprAxle axles).("]"++).nl.
      ("blades:["++).nl.pprBlades blades.("]"++)

-- | A cycle in the windmill, written as a non-empty list.
data Axle = IdA SlotIdx | ConsA SlotIdx Axle
     deriving (Eq, Read, Show)

-- | Axle pretty printer.
pprAxle :: Axle -> ShowS
pprAxle axle =
  let pprAxle_aux (IdA i) = shows i
      pprAxle_aux (ConsA i a) = shows i.(", "++).pprAxle_aux a
  in  ("{"++).pprAxle_aux axle.("}"++)

-- | Converts a list of slot indices to an axle.
listToAxle :: [SlotIdx] -> Axle
listToAxle [] = ierr "listToAxle: cannot construct empty cycle"
listToAxle [i] = IdA i
listToAxle (i : a) = ConsA i (listToAxle a)

-- | Converts an axle to a list of slot indices.
axleToList :: Axle -> [SlotIdx]
axleToList (IdA i) = [i]
axleToList (ConsA i a) = i : (axleToList a)

-- | A blade is a tree. Its root node may also belong to an axle.
type Blade = Tree SlotIdx

-- | Convert a tree to a string tree.
toStrTree :: Show a => Tree a -> Tree String
toStrTree (Node i ns) = Node (show i) (map toStrTree ns)

-- | Blade pretty printer.
pprBlade :: Blade -> ShowS
pprBlade blade = ((drawTree $ toStrTree blade)++)

-- | The positions of arguments whose value doesn't need access
--   to the current LAR. See 'closedExpr' in 'SLIC.Front.TailCalls'.
type ClosedInds = S.Set SlotIdx

-- | High-level description for LAR mutations. A LAR mutation is:
--   (a) a parallel assignment to reuse (the thunks of) formals,
--   (b) a number of new slots to assign to closed arguments, and
--   (c) a number of strict slots to evaluate on the spot.
type Mutation = (Windmill, ClosedInds, StrictInds)

-- | Pretty printer for LAR mutations.
pprintMut :: Mutation -> ShowS
pprintMut (windmill, closed, stricts) =
  (" windmill:"++).pprWindmill windmill.
  (" closed:["++).insCommIfMore (map shows $ S.toList closed).("] |"++).
  (" strict:["++).insCommIfMore (map shows $ S.toList stricts).("]"++)

-- | Tail-call information. LAR mutations keep the intensional index
--   to uniquely identify the call (set by the intensional transformation).
data CI = NoCI | Mut Mutation (Maybe IIndex)
     deriving (Eq, Read, Show)

instance PPrint CI where
  pprint NoCI = ("{}"++)
  pprint (Mut mut iidx) =
    ("{mut:"++).pprintMut mut.(" |"++).
    (" call:"++).(case iidx of Just i -> pprintIdx i ; Nothing -> id).
    ("}"++)
