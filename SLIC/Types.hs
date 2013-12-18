-- | The types used in the compiler.
-- 

module SLIC.Types where

import Prelude hiding (lookup, null)
import Data.List (intersperse)
import qualified Data.Map as Map (Map, filterWithKey, fromList, lookup, 
                                  map, null, toList)
import qualified Data.Sequence as Sequence (elemIndexR, fromList)
import SLIC.AuxFun (ierr, foldDot, showStrings, spaces, toLowerFirst)
import SLIC.Constants (bMod, delim, dfMod, lparen, rparen, nl)

-- * Names

-- | Module names.
type MName = String

-- | Simple names.
type SName = String

-- | Variable names, maybe qualified.
data QName = QN (Maybe MName) SName deriving (Eq, Ord, Read, Show)
instance PPrint QName where
  pprintPrec _ (QN (Just m) v) = (m++).([delim]++).(v++)
  pprintPrec _ (QN  Nothing v) = (v++)

-- | Converts a qualified variable name to a string using the pretty printer.
qName :: QName -> String
qName n = pprint n ""

-- | The local name of a name (without the module prefix).
lName :: QName -> SName
lName (QN _ v) = v

-- | The module prefix of a name.
mName :: QName -> Maybe MName
mName (QN m _) = m

-- | Applies a function to the local part of a qualified name.
procLName :: (SName -> SName) -> QName -> QName
procLName f (QN m x) = QN m (f x)

instance (Show a) => PPrint (Maybe a) where
  pprintPrec _ (Just m) = shows m
  pprintPrec _ (Nothing) = id

-- | The "const" name used during the intensional transformation.
constV :: QName
constV = QN Nothing "const"

-- | The name of the obligatory "result" definition in every program.
mainDefName :: SName
mainDefName = "result"

-- | The \"main\" variable of a module. If the module is compiled as the main
--   module of the program, evaluation of this variable starts program execution.
mainDefQName :: MName -> QName
mainDefQName m = QN (Just m) mainDefName

-- | The underscore pattern binder.
underscoreVar :: QName
underscoreVar = QN Nothing "_"

-- | Makes a pseudo constructor from a CID. It doesn't belong to any module.
mkQNCID :: CID -> QName
mkQNCID cid = QN Nothing (show cid)

-- | Given a constructor name and an arg index, returns the name of 
--   the corresponding formal of the function wrapper.
ithFrmOfCstr :: Int -> QName -> QName
ithFrmOfCstr i (QN m c) = QN m ((toLowerFirst c)++"_"++(show i))

-- | Placeholder for missing enclosing function information.
noEFunc :: QName
noEFunc = QN Nothing "<missing enclosing function>"

-- | Returns the module part of a fuly qualified string name (the part until
--   the final dot).
modOf :: String -> Maybe MName
modOf name =
  case Sequence.elemIndexR '.' (Sequence.fromList name) of
    Nothing  -> Nothing
    Just idx -> Just (take idx name)

-- | Returns the local name part of a fuly qualified string name (the part
--   after the final dot, or the full name if no dot exists).
lNameOf :: String -> SName
lNameOf name =
  case Sequence.elemIndexR '.' (Sequence.fromList name) of
    Nothing  -> name
    Just idx -> drop (idx+1) name

-- | Generates a qualified name from a simple name.
stringToQName :: String -> QName
stringToQName name = QN (modOf name) (lNameOf name)

-- * Auxiliary types (I)

-- | Constructor names.
type CstrName = QName

-- | Arity of a variable (>0 if a function with parameters).
type Arity = Int

-- | A table from variables to arities.
type Arities = Map.Map QName Arity

-- | Data type names.
type DTName = QName

-- | Strictness annotations ("!").
type Strictness = Bool

-- | The index used by the intensional operators
--   (modular intensional transformation).
type IIndex = (MName, Int)

-- | Pretty printer for intensional indexes.
pprintIdx :: IIndex -> ShowS
pprintIdx (m, i) = ("("++).(m++).(":"++).shows i.(")"++)

-- | The nesting depth of a bound variable in pattern matching.
type Depth = Maybe Int

-- | Spacing function (for depths).
spacing :: Depth -> ShowS
spacing (Nothing) = id 
spacing (Just d)  = spaces d

-- | The indexes of strict formals for a function signature.
type StrictInds = [Int]

-- | The list of strict formals of every function.
type Stricts = Map.Map QName StrictInds

-- | Prints the extra 'strict formals' field of a function definition.
pprintStrictFrms :: Stricts -> ShowS
pprintStrictFrms stricts =
  let aux (f, is) = pprint f.(" {"++).showStrings ", " (map show is).("}"++).nl
  in  foldDot aux $ Map.toList stricts

-- | A map containing the call-by-name formals of each function.
type CBNVars = Map.Map QName [QName]

-- | The variable usage information contains the variables that can be evaluated
--   using call-by-name and those that can be evaluated using call-by-value.
type VUsage = (CBNVars, Stricts)

-- * Pattern matching depths

-- | Pattern matching depth.
type PMDepth = Int

-- | The mapping of variable definitions to their maximum pattern
--   matching depth.
type PMDepths = Map.Map QName PMDepth

-- | Pretty printer for pattern matching depth tables.
pprintPD :: PMDepths -> ShowS
pprintPD pds =
  let pprPDep (qn, i) = pprint qn.(" ## "++).shows i.nl
  in  foldDot pprPDep (Map.toList pds)

-- | Returns the pattern matching depth of a function. Fails, if no depth
--   is found in the PMDepths table.
findPMDepth :: QName -> PMDepths -> PMDepth
findPMDepth f pmds =
  case Map.lookup f pmds of
    Just pd -> pd
    Nothing -> ierr $ (qName f)++" depth not found in:\n"++(pprintPD pmds "")

-- | Returns the pattern matching depth of a function. Returns 0 if no depth
--   is found in the PMDepths table.
findPMDepthSafe :: QName -> PMDepths -> PMDepth
findPMDepthSafe f pmds =
  case Map.lookup f pmds of
    Just pd -> pd
    Nothing -> 0

-- * Program information

-- | Information passed to the back-ends.
type ProgInfo = (FuncSigs, VUsage, PMDepths)

-- | The assigned numerical IDs to constructor during compilation.
type CID = Int

-- | The table of the IDs assigned to constructors during compilation.
type CIDs = Map.Map CstrName (Arity, CID)

-- | Pretty printer for compiled constructor tables.
pprintCIDs :: CIDs -> ShowS
pprintCIDs cids =
  let aux (c, (ar, cid)) = (pprint c).("/"++).shows ar.(" = "++).shows cid.nl
  in  foldDot aux (Map.toList cids)

-- | Filenames.
type FileName = String

-- | File paths.
type FPath = String

-- | A function signature.
type FSig = (QName, [QName])
  
-- | Function signatures: function (formals...)
type FuncSigs = Map.Map QName [QName]

-- | Returns the formals of a name. Fails if no signature is found.
frmsOf :: QName -> FuncSigs -> [QName]
frmsOf qn fsigs =
  case Map.lookup qn fsigs of
    Just frms -> frms
    Nothing -> ierr $ "No signature found for name "++(qName qn)++" in:\n"++(pprFSigs fsigs "")

-- | Pretty printer for function signature tables.
pprFSigs :: FuncSigs -> ShowS
pprFSigs fsigs =
  let aux [] = id
      aux ((f, vs):fs) =
        let strL ls = showStrings ", " (map qName ls)
        in  pprint f.("("++).strL vs.(")"++).nl.aux fs
  in  aux $ Map.toList fsigs
        
-- | Pretty printer for function signature tables (for import declarations).
pprintImportSigs :: FuncSigs -> ShowS
pprintImportSigs fsigs =
  let pprintISig (f, frms) = ("import "++).pprint f.lparen.showStrings ", " (map qName frms).rparen.nl
  in  if Map.null fsigs then id else foldDot pprintISig (Map.toList fsigs)

-- | Fins the parameters of a function name in a signatures table.
paramsOf :: QName -> FuncSigs -> [QName]
paramsOf qn fsigs =
  case Map.lookup qn fsigs of
    Just ps -> ps
    Nothing -> ierr $ "No parameters found in signatures table for "++(qName qn)

-- | A \"case ... of\" location: inside a case expression of some
--   depth and inside an enclosing function.
type CaseLoc = (Depth, QName)

-- | The default value for code where no enumeration has taken place yet.
noCaseLoc :: CaseLoc 
noCaseLoc = (Nothing, noEFunc)

-- | Program variables.
data V = V QName           -- ^ normal (free or local) variable
       | BV QName CaseLoc    -- ^ pattern-bound variable
       deriving (Eq, Ord, Read, Show)
             
instance PPrint V where
  pprintPrec _ (V v) = pprint v
  pprintPrec _ (BV v (d, f)) = pprint v.("{"++).pprint f.(":"++).shows d.("}"++)
  
-- | Modifies the name of a variable.
modifyV :: (QName -> QName) -> V -> V
modifyV f (V v) = V (f v)
modifyV f (BV bv d) = BV (f bv) d

-- | Extracts the name of a variable.
nameOfV :: V -> QName
nameOfV (V v) = v
nameOfV (BV v _) = v

data Ground = T   DTName         -- ^ a normal data type
            | TDF DTName Type    -- ^ a defunctionalized data type
                                 --   (annotated by the original)
            deriving (Eq, Ord, Read, Show)

-- | The main data type inside a ground type.
dtOfGround :: Ground -> DTName
dtOfGround (T dt)     = dt
dtOfGround (TDF dt _) = dt

instance PPrint Ground where
   pprintPrec _ (T g) = pprint g
   pprintPrec _ (TDF _ t) = ("Closure{"++).pprint t.("}"++)

-- * Types

-- | Type variables.
type TVar = String

-- | A data type component type, either a simple type, or 
--   a function type.
data Type =
      Tg Ground      -- ^ base type, such as Int
    | Tv TVar        -- ^ type variable
    | Tf Type Type   -- ^ function type, such as Int->Int
    | Ta Type Type   -- ^ type application
    deriving (Eq, Ord, Read, Show)

instance PPrint Type where
  pprint (Tg g)   = pprint g
  pprint (Tv a)   = (a++)
  pprint (Tf a b) = ("("++).pprint a.(" -> "++).pprint b.(")"++)
  pprint (Ta a b) = ("("++).pprint a.(" "++).pprint b.(")"++)

-- | Takes the residual type of a closure formed after binding a function
--   of some type to a number of arguments.
residualT :: Arity -> Type -> Type
residualT ar t = 
  let paramsT = takeParams t
  in  if ar > length paramsT then ierr "residualT: too big arity"
      else constrT (drop ar paramsT) (takeRes t)

-- | Returns all the types appearing in a type.
types :: Type -> [Type]
types t@(Tg _)   = [t]
types t@(Tv _)   = [t]
types t@(Ta _ _) = [t]
types   (Tf a b) = a:(types b)

-- | Drops the first n arguments types from a type (t_1 -> ... -> t_n -> t).
dropT :: Arity -> Type -> Type
dropT 0 t        = t
dropT n (Tf _ t) = dropT (n-1) t
dropT _ (Tg _)   = ierr "cannot drop args from ground type"
dropT _ (Tv _)   = ierr "cannot drop args from type variable"
dropT _ (Ta _ _) = ierr "cannot drop args from abstract type"

-- * Built-in data types, constructors and type information

dtInt     :: DTName ; dtInt     = QN bMod "Int"
dtInteger :: DTName ; dtInteger = QN bMod "Integer"
dtBool    :: DTName ; dtBool    = QN bMod "Bool"
dtUnit    :: DTName ; dtUnit    = QN bMod "Unit"
dtList    :: DTName ; dtList    = QN bMod "$List"
-- a special data type that represents the absence of information
-- TODO: use typeclasses for the auto-derived pretty printers
dtMagic   :: DTName ; dtMagic   = QN bMod "$Magic"
-- built-in tuples
dtTuple   :: Int -> DTName ; dtTuple i = QN bMod ("$Tuple"++(show i))

maxTupleSize :: Int ; maxTupleSize = 15   -- "6.1.4 Tuples", Haskell 98 Report

gInt     :: Ground        ; gInt     = T dtInt
gInteger :: Ground        ; gInteger = T dtInteger
gBool    :: Ground        ; gBool    = T dtBool
gUnit    :: Ground        ; gUnit    = T dtUnit
gList    :: Ground        ; gList    = T dtList
gMagic   :: Ground        ; gMagic   = T dtMagic
gTuple   :: Int -> Ground ; gTuple i = T (dtTuple i)

tInt      :: Type         ; tInt     = Tg gInt
tChar     :: Type         ; tChar    = tInt              -- Char == Int
tInteger  :: Type         ; tInteger = Tg gInteger
tBool     :: Type         ; tBool    = Tg gBool
tUnit     :: Type         ; tUnit    = Tg gUnit
tList     :: Type         ; tList    = Tg gList
tMagic    :: Type         ; tMagic   = Tg gMagic
tTuple    :: Int -> Type  ; tTuple i = Tg (gTuple i)
tString   :: Type         ; tString  = Ta tList tChar    -- String == [Char]

-- | The list of all built-in data types.
builtinDTypes :: [DTName]
builtinDTypes = [dtInt, dtInteger, dtBool, dtUnit, dtList, dtMagic] ++
                (map dtTuple b_tupleSizes)

-- | The list of all tuple sizes with built-in support.
b_tupleSizes :: [Arity]
b_tupleSizes = [2..maxTupleSize]

-- * Auxiliary types (II)

-- | Variable information records type and arity (if it's a top-level function).
type EInfo  = (Type, Maybe Arity)

-- | A typing environment parameterized by key (e.g. variables, names).
type Env a = Map.Map a EInfo

-- | Pretty printer for environments.
pprintE :: (PPrint a) => Env a -> ShowS
pprintE ve =
  let pprintE0 []      = nl
      pprintE0 (vb:rs) = pprintE0_aux vb.nl.pprintE0 rs
      pprintE0_aux (v, (t, ar)) = pprint v.s ar.(" :: "++).pprint t
      s (Just ar)       = ("() / "++).shows ar
      s Nothing         = id
  in  pprintE0 $ Map.toList ve

-- | A typing environment (Yaghi-style transformation).
type TEnv = Env QName

-- | One entry in a typing environment.
type TEnvI = (QName, EInfo)

-- | A typing environment represented as a list.
type TEnvL = [TEnvI]

-- | Returns the arity of a type.
typeArity :: Type -> Arity
typeArity t = length (types t) - 1

-- | Finds the type/arity information of a name in an environment.
findInfo :: (Ord a, PPrint a) => a -> Env a -> EInfo
findInfo qn env =
  case Map.lookup qn env of
    Just info  -> info
    Nothing ->
      ierr ("name not found: " ++ (pprint qn "") ++ " in " ++ (pprintE env ""))
  
-- | Finds the type of a name in an environment.
findType :: (Ord a, PPrint a) => a -> Env a -> Type
findType qn env =
  let (t, _) = findInfo qn env
  in  t
  
-- | Finds the arity of a name.
arity :: (Ord a, PPrint a) => a -> Env a -> Arity
arity qn env = 
  case findInfo qn env of
    (_, Just ar) -> ar
    (_, Nothing) -> ierr $ "arity of "++(pprint qn "")++" not found"

-- | The order of a type, i.e. the number of arguments that can be gievn until
--   a ground type is reached.  
order :: Type -> Arity
order (Tg _) = 0
order (Tv _) = 0
order (Ta _ _) = 0
order (Tf _ b) = 1 + (order b)
       
-- | Tests if a variable will return lazy data on evaluation.
returnsThunk :: TEnv -> QName -> Bool
returnsThunk ve vn =
  -- hack: if it is a closure constructor/dispatcher, assume it returns thunk
  case mName vn of
    Just m | m==dfMod -> True
    _ -> 
      case Map.lookup vn ve of
        Just (_, Nothing) ->
          ierr $ "returnsThunk: called for "++(pprint vn "")++", which has no arity"
        Just (t, Just ar) ->
          let resType = drop ar $ types t
          in  case resType of
                [rT] ->              -- defined arity agrees with type
                  case rT of
                    Tg gt  -> not $ isNullaryGT gt
                    Tv _   -> True   -- abstract types are assumed to use thunks
                    Ta _ _ -> True   -- abstract types with arguments should contain
                                     -- non-nullary constructors (unless degenerate)
                    Tf _ _ -> ierr "returnsThunk: last type found is function"
                (_:_:_) -> True       -- defined arity < type arity, returns closure
                [] -> ierr $ "arity of "++(pprint vn "")++
                             " is bigger than type "++(pprint t "")
        Nothing ->      
          ierr $ "returnsThunk: could not find name "++(qName vn)++" in: "++(pprintE ve "")

-- | Tests if a ground type is one of the known nullary constructor data types.
--   Closures are not considered nullary data types (use -enum to convert them to
--   the integer data type for some programs).
isNullaryGT :: Ground -> Bool
isNullaryGT (T dt)    = dt == dtInt || dt==dtInteger || dt==dtBool || dt==dtUnit
isNullaryGT (TDF _ _) = False

-- | Returns the types of all parameters of a type (until but not including
--   the ground result).
takeParams :: Type -> [Type]
takeParams t = init $ types t

-- | Returns the type of the result of a type.
takeRes :: Type -> Type
takeRes t = last $ types t

-- | Takes a list of types (t_0, ..., t_n) and a type t and returns the
--   type (t_0 -> ... -> t_n -> t).
constrT :: [Type] -> Type -> Type
constrT []     r = r
constrT (t:tl) r = Tf t (constrT tl r)

-- | Returns the arity of a constructor and its compiled ID.
findArId :: CstrName -> CIDs -> (Arity, CID)
findArId c cids =
  case Map.lookup c cids of
    Just (ar, cid) -> (ar, cid)
    Nothing -> ierr ("findIDAR: constructor "++(qName c)++" not found in enum: "++(show cids))

-- | First projection of findIDAR.
findID :: CstrName -> CIDs -> CID
findID c cids = let (_, cId) = findArId c cids in cId

-- | Second projection of findIDAR.
findArity :: CstrName -> CIDs -> Arity
findArity c cids = let (cAr, _) = findArId c cids in cAr

-- * The pretty printing class (PPrint) and its helper functions

-- | The typeclass of things that can be printed on screen.
--   Mimics the Show typeclass.
class PPrint a where
  pprint :: a -> ShowS
  pprint x = pprintPrec 0 x
  pprintPrec :: Int -> a -> ShowS
  pprintPrec _ x = pprint x

instance (PPrint a, PPrint b) => PPrint (a, b) where
  pprint (a, b) = ("("++).pprint a.(", "++).pprint b.(")"++)

-- | Helper function to print a list of PPrint-ables.
pprintList :: (PPrint a) => ShowS -> [a] -> ShowS
pprintList pDelim as = foldDot id $ intersperse pDelim $ map pprint as

instance (PPrint a) => PPrint [a] where
  pprint as = pprintList (" "++) as

-- | Prints a printable thing in the IO monad.
printLn :: (PPrint a) => a -> IO (Maybe b)
printLn a =
  do putStrLn (pprint a "")
     return Nothing

-- | A safer version of 'take'.
take' :: (PPrint a) => Int -> [a] -> [a]
take' n list =
  let takeSafe 0 _ buf = buf
      takeSafe _ [] _  = ierr $ "Cannot take "++(show n)++" elements from "++(pprint list "")++", which has length "++(show (length list))
      takeSafe i (x:xs) buf = takeSafe (i-1) xs (x:buf)
  in  takeSafe n list []

-- * Built-in functions: signatures and type information

-- built-in functions

bf_toInteger       :: QName          ; bf_toInteger       = QN bMod "toInteger"
bf_toInteger_arg0  :: QName          ; bf_toInteger_arg0  = QN bMod "toInteger_x"

bf_printIntIO      :: QName          ; bf_printIntIO      = QN bMod "printIntIO"
bf_printIntIO_arg0 :: QName          ; bf_printIntIO_arg0 = QN bMod "printIntIOi"

bf_readIntIO       :: QName          ; bf_readIntIO       = QN bMod "readIntIO"

bf_putStr          :: QName          ; bf_putStr          = QN bMod "putStr"
bf_putStr_arg0     :: QName          ; bf_putStr_arg0     = QN bMod "putStr_arg"
bf_putStrLn        :: QName          ; bf_putStrLn        = QN bMod "putStrLn"
bf_putStrLn_arg0   :: QName          ; bf_putStrLn_arg0   = QN bMod "putStrLn_arg"
bf_error           :: QName          ; bf_error           = QN bMod "error"
bf_error_arg0      :: QName          ; bf_error_arg0      = QN bMod "error_arg"

bf_runMainIO       :: QName          ; bf_runMainIO       = QN bMod "runMainIO"
bf_runMainIO_arg0  :: QName          ; bf_runMainIO_arg0  = QN bMod "runMainIO_a"

bf_show            :: QName          ; bf_show            = QN bMod "show"
bf_show_arg0       :: QName          ; bf_show_arg0       = QN bMod "showArgument"

-- built-in constructors ('$' is inserted to avoid clashing with user-defined
-- constructors)

bf_Cons   :: QName                   ; bf_Cons   = QN bMod "Cons$"
bf_cons_0 :: QName                   ; bf_cons_0 = QN bMod "cons$_0"
bf_cons_1 :: QName                   ; bf_cons_1 = QN bMod "cons$_1"
bf_Nil    :: QName                   ; bf_Nil    = QN bMod "Nil$"
bf_Unit   :: QName                   ; bf_Unit   = QN bMod "Unit$$"
bf_Tuple  :: Int -> QName            ; bf_Tuple i= QN bMod ("TupleC$"++(show i))

-- | The IDs of the built-in constructors.
builtinCIDs :: CIDs
builtinCIDs = Map.fromList $
              [ (bf_Cons, (2, 0)), (bf_Nil, (0, 1))
              , (bf_Unit, (0, 0)) ]
              ++ (map (\i->(bf_Tuple i, (i, 0))) b_tupleSizes)

-- | Generator of the pretty printer function corresponding to a data type.
pprDT :: DTName -> String -> QName
pprDT (QN m dtName) sufx = QN m ("_pretty_printDT_"++dtName++sufx)

-- The type variables used for type checking built-in functions.
v_a :: SName  ; v_a = "a"       ;   tv_a :: Type  ; tv_a = Tv v_a
v_b :: SName  ; v_b = "b"       ;   tv_b :: Type  ; tv_b = Tv v_b
v_m :: SName  ; v_m = "m"       ;   tv_m :: Type  ; tv_m = Tv v_m
-- | Type variable with index.
tv_a_ :: Int -> Type            ;   tv_a_ i = Tv (v_a++(show i))

-- | Types of built-in functions.
builtinTEnv :: TEnv
builtinTEnv = 
  Map.fromList $
  [ (bf_toInteger, (Tf tInt tInteger, Just 1))
  , (bf_toInteger_arg0, (tInt, Nothing))
  , (bf_putStrLn, (Tf (Ta tList tv_a) tUnit, Just 1))
  , (bf_putStrLn_arg0, (Ta tList tv_a, Nothing))
  , (bf_putStr, (Tf (Ta tList tv_a) tUnit, Just 1))
  , (bf_putStr_arg0, (Ta tList tv_a, Nothing))
  , (bf_error, (Tf (Ta tList tInt) tv_a, Just 1))
  , (bf_error_arg0, (Ta tList tv_a, Nothing))
  , (bf_show, (Tf tv_a (Ta tList tInt), Just 1))
  , (bf_show_arg0, (tInt, Nothing))
  , (bf_runMainIO, (Tf (Ta tList tv_a) tUnit, Just 1))
  , (bf_runMainIO_arg0, (Ta tList tv_a, Nothing))
  , (bf_readIntIO, (tInt, Just 0))
  , (bf_printIntIO, (Tf tInt tInt, Just 1))
  , (bf_Cons, (Tf tInt (Tf (Ta tList tv_a) (Ta tList tv_a)), Just 2))
  , (bf_cons_0, (tInt, Nothing))
  , (bf_cons_1, (Ta tList tv_a, Nothing))
  , (bf_Nil, (Ta tList tv_a, Just 0))
  , (bf_Unit, (tUnit, Just 0))
  ] ++ (concatMap mkTupleTEnv b_tupleSizes)

-- | For some arity, generate the types of the tuple constructor of that arity,
--   together with the types of its formals.
mkTupleTEnv :: Arity -> [TEnvI]
mkTupleTEnv n =
  let (f, fs) = bf_TupleSig n
      tvs = map tv_a_ [0..(n-1)]
      info_c = (f, (abstr tvs, Just n))
      info_frms = zip fs (zip tvs (repeat Nothing))
      abstr [] = mkTupleT tvs
      abstr (t:ts) = Tf t (abstr ts)
  in  info_c : info_frms

-- | Generates the type of tuple type, takes the list of types.      
mkTupleT :: [Type] -> Type      
mkTupleT tts =
  let aux [] = ierr "transl_type: tuple type with no parameters"
      aux [t] = Ta (tTuple (length tts)) t
      aux (t:ts) = Ta (aux ts) t
  in  aux $ reverse tts

-- | A table containing only the bulit-in constructors.
builtinConstrs :: Arities
builtinConstrs =
  let constrs = [bf_Cons, bf_Nil, bf_Unit]
  in  Map.filterWithKey (\c _-> c `elem` constrs) builtinArities

-- | Function signatures of built-in functions (needed for the intensional 
--   transformation).
builtinFuncSigs :: FuncSigs
builtinFuncSigs = Map.fromList $
                  [ (bf_toInteger, [bf_toInteger_arg0])     -- big integers
                  , (bf_printIntIO, [bf_printIntIO_arg0])
                  , (bf_readIntIO, [])
                  , (bf_putStr, [bf_putStr_arg0])
                  , (bf_putStrLn, [bf_putStrLn_arg0])
                  , (bf_show, [bf_show_arg0])
                  , (bf_error, [bf_error_arg0])
                  , (bf_runMainIO, [bf_runMainIO_arg0])
                  , (bf_Cons, [bf_cons_0, bf_cons_1])
                  , (bf_Nil, [])
                  , (bf_Unit, [])
                  ] ++ (map bf_TupleSig b_tupleSizes)
                  
-- | Generate the signature of the built-in tuple constructor for a given arity.
bf_TupleSig :: Arity -> FSig
bf_TupleSig n = 
  let frms =
        map (\i->QN bMod (toLowerFirst $ lName (bf_Tuple n)++"_"++(show i))) 
        [0..(n-1)]
  in  (bf_Tuple n, frms)

-- | Arity information for all built-in functions.
builtinArities :: Arities                  
builtinArities = Map.map (\args->length args) builtinFuncSigs
                  
-- | The pattern depths of the built-in functions.
builtinPmDepths :: PMDepths
builtinPmDepths = Map.fromList $
                  [ (bf_toInteger, 0), (bf_printIntIO, 0)
                  , (bf_readIntIO, 0)
                  , (bf_putStr, 0), (bf_putStrLn, 0), (bf_show, 0)
                  , (bf_error, 0), (bf_runMainIO, 0)
                  , (bf_Cons, 0), (bf_Nil, 0), (bf_Unit, 0)
                  ] ++ (map (\i->(bf_Tuple i, 0)) b_tupleSizes)

-- * Interpreter values

-- | Execution values. Lazy constructors are parameterized by the type
--   they embed.
data Value a = VI !Int                -- ^ integer
             | VB !Bool               -- ^ boolean
             | VT (CstrName, a)       -- ^ suspended constructor
             deriving (Eq, Read, Show)

instance PPrint (Value a) where
   pprintPrec _ (VI int)    = shows int
   pprintPrec _ (VB bool)   = shows bool
   pprintPrec _ (VT (c, _)) = pprint c

-- | Gets an integer value.
intFrom :: (Value a) -> Int
intFrom (VI i) = i
intFrom v = error $ "not an integer: " ++ (pprint v "")

-- | Gets a boolean value.
boolFrom :: (Value a) -> Bool
boolFrom (VB b) = b
boolFrom v = error $ "not a boolean: " ++ (pprint v "")

-- * CAFs

-- | Ids for CAFs serving as indices in a global LAR.
type CAFId = Int

-- | A dictionary from (CAF) names to indices in a CAF-LAR.
type CAFDct = [(QName, Int)]

-- | Pretty printer for CAF index tables.
pprintCAFs :: CAFDct -> ShowS
pprintCAFs cafDct=
  let pprCAF (qn, i) = pprint qn.(": #"++).shows i.nl
  in  foldDot pprCAF cafDct
