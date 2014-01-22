-- | Constants used in the implementation.

module SLIC.Constants where

-- * Pretty printing

-- | Enable comments when pretty printing.
comments :: Bool ;  comments = False

nl :: ShowS ;       nl = ("\n"++)
space :: ShowS ;    space = (" "++)
tab :: ShowS ;      tab = ("  "++) ;     -- ^ Tabs are two spaces.
dquot :: ShowS ;    dquot = ("\""++)
comma :: ShowS ;    comma = (","++)
semi :: ShowS ;     semi = (";"++)
colon :: ShowS ;    colon = (":"++)
lbracket :: ShowS ; lbracket = ("{"++)
rbracket :: ShowS ; rbracket = ("}"++)
lparen :: ShowS ;   lparen = ("("++)
rparen :: ShowS ;   rparen = (")"++)
lcurl :: ShowS ;    lcurl = ("{"++)
rcurl :: ShowS ;    rcurl = ("}"++)
langle :: ShowS ;   langle = ("<"++)
rangle :: ShowS ;   rangle = (">"++)
hyph :: ShowS ;     hyph = ("-"++)
plus :: ShowS ;     plus = ("+"++)
times :: ShowS ;    times = ("*"++)
uscore :: ShowS ;   uscore = ("_"++)
lcomment :: ShowS ; lcomment = lcurl.hyph
rcomment :: ShowS ; rcomment = hyph.rcurl

-- * Defunctionalization

-- | The delimiter character used for unique names during defunctionalization.
delim :: Char
delim = '_'

-- | The delimiter between the types of a constructor's fields. Used
--   by defunctionalization, for the serialization of closure types.
dfTypDelim :: Char
dfTypDelim = '$'

-- | The name of the mini-interpreter.
dfApply :: String
dfApply = "apply"

-- | The prefix of the /apply()/ interpreter functions of defunctionalization.
--   Should not appear as a prefix of functions in the input program.
applPref :: String
applPref = dfApply++[delim]

-- | The suffix of the defunctionalization interface files.
dfiSuffix :: String
dfiSuffix = ".dfi"

-- | The suffix of the intensional interface files.
iiSuffix :: String
iiSuffix = ".ii"

-- | The prefix of the closure formal variable in the defunctionalization dispatchers.
dfClosurePre :: String
dfClosurePre = "cl"++[delim]

-- | The name of the pseudo-module, where defunctionalization places its
--   generated code.
dfMod :: String
dfMod = "LibDef"

-- * TTD back-end

-- | The prefix of merged operators in the TTD back-end.
mopPre :: String
mopPre = "op_"

-- * LAR/C constants

-- | The default maximum memory size (in bytes).
defaultMemSize :: Int
defaultMemSize = 120*1000*1000

-- | Names that clash with the C reserved words in the LAR back-end.
cReservedWords :: [String]
cReservedWords =
  [ "and", "and_eq", "asm", "auto", "bitand", "bitor", "bool", "break", "case"
  , "catch", "char", "class", "compl", "const", "const_cast", "continue"
  , "default", "delete", "do", "double", "dynamic_cast", "else", "enum"
  , "explicit", "export", "extern", "false", "float", "for", "friend", "goto"
  , "if", "inline", "int", "long", "mutable", "namespace", "new", "not", "not_eq"
  , "operator", "or", "or_eq", "private", "protected", "public", "register"
  , "reinterpret_cast", "return", "short", "signed", "sizeof", "static"
  , "static_cast", "struct", "switch", "template", "this", "throw", "true", "try"
  , "typedef", "typeid", "typename", "union", "unsigned", "using", "virtual"
  , "void", "volatile", "wchar_t", "whie", "xor", "xor_eq" ]
  
-- | The number of the maximum LARs that can be nested under another LAR.
--   The value corresponds to the 'unsigned char' of the implementation.
maxNestedLARs :: Int
maxNestedLARs = 255

-- | Maximum LAR arity. The value corresponds to the 'unsigned char' of the 
--   implementation.
maxLARArity :: Int
maxLARArity = 255

-- * Distributed eduction

-- | The default number of warehouse to use for distributed eduction.
defaultWhs :: Int
defaultWhs = 10

-- | The default maximum number of contexts per warehouse in distributed eduction.
defaultMaxCtxts :: Int
defaultMaxCtxts = 500000

-- * Misc. constants

-- | The name of the default module, if no name is given.
defaultMod :: String
defaultMod = "Main"

-- | The name of the built-in pseudo-module.
bModN :: String
bModN = "GIC"

-- | The name of the built-in pseudo-module ('Maybe' version).
bMod :: Maybe String
bMod = Just bModN

-- | The name of the built-in \"Control.Parallel\" module.
mControlParallelN :: String
mControlParallelN = "Control.Parallel"

-- | The name of the built-in \"Control.Parallel\" module ('Maybe' version).
mControlParallel :: Maybe String
mControlParallel = Just mControlParallelN

-- | The type classes pseudo-module that contains method dispatchers.
tcMod :: String
tcMod = "$Typeclasses$"

-- | The default working path.
defaultPath :: String
defaultPath = "."

-- | The directories separator. Depends on the OS.
dirSeparator :: Char
dirSeparator = '/'

-- | The \"path\" for dynamically generated modules.
modNoPath :: String
modNoPath = "<dynamically generated>"

-- | The \"module\" that all code belongs to when compiled in whole-program mode.
wpMod :: String
wpMod = ""

-- | The default maximum warehouse size.
defaultMaxWHSize :: Int
defaultMaxWHSize = 20000
