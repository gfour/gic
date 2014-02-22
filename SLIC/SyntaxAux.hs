-- | Common things for the syntax of all intermediate languages.
-- 

module SLIC.SyntaxAux where

import Prelude hiding (lookup, null)
import Data.List (intersperse)
import qualified Data.Map as Map (Map, filter, filterWithKey, fromList,
                                  keys, lookup, member, null, toList, unions)
import Data.Maybe (isJust)
import SLIC.AuxFun (ierr, foldDot, showStrings, spaces)
import SLIC.Constants (mControlParallelN, tcMod, lparen, rparen, nl, semi)
import SLIC.Types

-- * User-defined data types

-- | Data type names in constructor declarations.
data DT =
    DT Type Strictness (Maybe QName) -- ^ data type name with a strictness 
                                     --   annotation and an optional selector
    deriving (Eq, Read, Show)

instance PPrint DT where
  pprintPrec _ (DT dt s sel) =
    (if s then ("!"++) else id).
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
containSelectors dts = or $ map (\(DT _ _ sel)->isJust sel) dts

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
  in  Map.fromList $ map aux $ zip constrs [1..(length constrs)]

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
pprint_tab :: (PPrint a) => Loc -> a -> ShowS
pprint_tab l pat = (case l of Just (_, d) -> spaces (2*d); _ -> id).pprint pat

-- | Pretty printer of semicolon-terminated tabbed lists. Used for patterns.
pprint_tab_l :: (PPrint a) => Loc -> [a] -> ShowS
pprint_tab_l _ []         = ("{- nothing -}"++).nl
pprint_tab_l l [pat]      = pprint_tab l pat.nl
pprint_tab_l l (pat : ps) = pprint_tab l pat.semi.nl.pprint_tab_l l ps

-- | Shows a bound variable name.
pprintBV :: QName -> ShowS
pprintBV v = ("@"++).pprint v

-- | Shows a constructor name.
pprintTH :: CstrName -> ShowS
pprintTH c = pprint c

-- * Import declarations

-- | The type of an imported name. During parsing, it's Nothing; it gets its
--   value after reading the DFI of the imported module.
type IType = Maybe Type

-- | Converts the type of an import to a string.
pprintIType :: IType -> ShowS
pprintIType (Nothing) = ("<missing type>"++)
pprintIType (Just t)  = shows t

-- | An imported name can be a function, a constructor, or a data type name.
data NInfo = NFunc | NConstr | NDType deriving (Eq, Show)

-- | Information for an imported name: type, arity, CAF position, nesting.
data IInfo =
  IInfo { impT :: IType              -- ^ type
        , impA :: Maybe Arity        -- ^ arity
        , impC :: NInfo              -- ^ what kind of name it is
        , impCAF :: Maybe CAFId      -- ^ global CAF index if it is a CAF
        , impD :: Depth              -- ^ pattern matching depth
        }
  deriving (Eq, Show)

-- | A mapping from imported names to information for compilation.
type ImportedNames = Map.Map QName IInfo

-- | Pretty printer for compilation information of imported names.
pprintINames :: ImportedNames -> ShowS
pprintINames impNames =
  let pprintIN (v, (IInfo _ _ _ _ _)) =
        pprint v -- .("::"++).pprintIType it
      --   pprint v.("::"++).pprintIType it.showsCAF caf.showsNesting nesting
      -- showsCAF Nothing      = id
      -- showsCAF (Just idx)   = ("[CAF #"++).shows idx.("]"++)
      -- showsNesting Nothing  = id
      -- showsNesting (Just n) = ("[nesting="++).shows n.("]"++)
  in  foldDot id $ intersperse (", "++) $ map pprintIN $ Map.toList impNames

-- | An import declaration.
data IDecl =
  IDecl { ideclMName  :: MName         -- ^ imported module name
        , ideclINames :: ImportedNames -- ^ import names (typed symbol table)
        , ideclInfo   :: Maybe (FuncSigs, CIDs) -- ^ signatures/CIDs table
        }

-- | Used to filter out built-in modules, leaving only user-defined module names.
filterRealMods :: [IDecl] -> [MName]
filterRealMods idecls = map ideclMName $ 
  filter (\(IDecl mn _ _)->not $ Map.member mn builtinModules) idecls

instance PPrint IDecl where
  pprintPrec _ (IDecl mn impNames info) =
    ("import "++).(mn++).(" ("++).pprintINames impNames.(")"++).
    (case info of
        Just (fsigs, _) -> (" ["++).pprFSigs fsigs.("]"++)
        Nothing         -> id
    ).nl

-- | Returns all imported names declared in a list of \'import\' declarations.
mergeINames :: [IDecl] -> ImportedNames
mergeINames idms = Map.unions $ map (\(IDecl _ ins _)->ins) idms

-- * Built-in \'import\' declarations

-- | The \'import\' for the built-in Control.Parallel module.
importControlParallel :: IDecl
importControlParallel = genBuiltinIDecl mControlParallelN

-- | Given a built-in module name, generate its import declaration.
genBuiltinIDecl :: MName -> IDecl
genBuiltinIDecl mn =
  let filtModNames bf _ = (lName bf)==mn
      modSigs    = Map.filterWithKey filtModNames builtinFuncSigs
      modTEnv    = Map.filterWithKey filtModNames builtinTEnv
      modCIDs    = Map.filterWithKey filtModNames builtinCIDs
      modConstrs = Map.keys modCIDs
      modFuncs   = filter (\qn->not (qn `elem` modConstrs)) $ Map.keys modSigs
      names      = (map (mkImp NFunc) modConstrs)++
                   (map (mkImp NConstr) modFuncs)
      mkImp ni n =
        let (nT, Just nAr) = findInfo n modTEnv
            cid = case Map.lookup n builtinCIDs of
                    Just (_, cId) -> Just cId
                    Nothing       -> Nothing
            pmDepth = Map.lookup n builtinPmDepths
        in  (n, IInfo (Just nT) (Just nAr) ni cid pmDepth)
  in  IDecl mn (Map.fromList names) (Just (modSigs, modCIDs))

-- | Built-in (pseudo-)modules.
builtinModules :: Map.Map MName IDecl
builtinModules = Map.fromList [ (mControlParallelN, importControlParallel) 
                              , (tcMod, error "!")]

-- * Modules

-- | The name and file path of a module.
type MNameF = (MName, FPath)

-- | The signatures of exported functions.
type Exports = FuncSigs

-- | Pretty printer for export declarations.
pprintExports :: Exports -> ShowS
pprintExports exports = if Map.null exports then id else lparen.showStrings ", " (map qName $ Map.keys exports).rparen
--   if null exports then id else lparen.showStrings ", " (map (\(f, fs)->pprint f (show fs)) $ Map.toList exports).rparen

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
    (if Map.null env then id else ("-- Type signatures:"++).nl.pprintE env)

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
cOps :: Map.Map COp String
cOps = Map.fromList $
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
    case Map.lookup c cOps of
      Just s  -> (s++)
      Nothing ->
        ierr $ "String representation of constant "++(show c)++" not found."

-- | Given a string representation, returns the corresponding 'COp'. If more
--   than one operators correspond to the same representation (such as '-'),
--   the first one found is returned.
cOpForStr :: String -> COp
cOpForStr s =
  case Map.toList (Map.filter (\s'->s==s') cOps) of
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
    c | c == CEqu || c == CNEq || c == CLt || c == CGt ||
        c == CLe || c == CGe ->
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
