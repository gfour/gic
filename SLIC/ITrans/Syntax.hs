-- | The syntax of the intermediate languages used by the intensional 
--   transformation.
-- 
-- - HIL: the intermediate hybrid intensional language used in the transformation.
-- 
-- - ZOIL: the target 0-order intensional language.
--

module SLIC.ITrans.Syntax where

import SLIC.AuxFun (ierr, showStrings, spaces)
import SLIC.Constants (nl, space)
import SLIC.SyntaxAux
import SLIC.SyntaxFL
import SLIC.Types

-- * The HIL language

-- | A HIL expression.
data ExprH =
    XH V                     -- ^ variable
  | ConH Const [ExprH]       -- ^ built-in constant application
  | FH QOp QName [ExprH]     -- ^ function call (with intensional operators)
  | CaseH CaseLoc ExprH [PatH] -- ^ pattern matching expression
  | ConstrH CstrName         -- ^ constructor call
  deriving (Eq,Read)

-- | An intensional call(i) operator.
data QOp = NOp 
         | Call IIndex deriving (Eq,Read)

-- | A HIL definition.
data DefH = 
    DefH QName [QName] ExprH        -- ^ v (v0, ..., vN) = e
  | ActualsH QName MName [ExprH]    -- ^ v = actuals(i0:e0, ..., iN:eN)
  deriving (Eq,Read)

-- | A HIL program.
type ProgH = Prog DefH

-- | A flag that shows if a costructor binds variables in an expression.
type BindsVars = Bool

-- | Prints the \"binds\" flag of patterns.
pprintBinds :: BindsVars -> ShowS
pprintBinds b = if b then ("   {binds}"++) else id

-- | A HIL pattern pairs a constructor name with a HIL expression.
data PatH = PatH CstrName ExprH BindsVars deriving (Eq, Read)

-- | The signatures of imported functions.
type ImportsI = FuncSigs

-- | A HIL module.
type ModH = Mod ProgH

instance PPrint QOp where
   pprintPrec _ (Call i) = ("call[" ++).pprintIdx i.("]"++)
   pprintPrec _ NOp      = id
      
instance PPrint ExprH where
   pprintPrec _ (XH vn)          = pprintVar vn
   pprintPrec p (ConH cn el)     = prettyConst p cn el
   pprintPrec _ (ConstrH c)      = pprintTH c
   pprintPrec _ (FH NOp vn ps)   = pprint vn.pprintList space ps
   pprintPrec p (FH qOp vn ps)   =
       pprintPrec p qOp.(" (" ++).pprint vn.(")" ++).pprintList space ps
   pprintPrec p (CaseH (d, _) e pats) =
      let pprintPats []        = id
          pprintPats (p0 : pl) = pprintPat p0 . pprintPats pl
          pprintPat (PatH c0 e0 b0) =
            ("\n  "++).spacing d.("| "++).pprint c0.(" -> "++).pprintPrec p e0.
            pprintBinds b0
      in  ("case "++).pprintPrec p e.(" of{"++).shows d.("}"++).
          pprintPats pats
                
instance PPrint DefH where
    pprintPrec p (DefH vn ps e) =
        pprint vn.spaces 1.showStrings " " (map qName ps).(" = " ++).pprintPrec p e
    pprintPrec _ (ActualsH vn m es) =
        pprint vn.("{"++).(m++).("}"++).
        (" = actuals["++).(pprintList (", "++) es).("]" ++)

-- * The ZOIL language

-- | A 0-order intensional expression.
data ExprZ = XZ V                         -- ^ variable
	   | ConZ Const [ExprZ]           -- ^ built-in constant application
	   | FZ QOp QName                 -- ^ intensional call operator
           | CaseZ CaseLoc ExprZ [PatZ]   -- ^ pattern matching
           | ConstrZ CstrName             -- ^ intensional constructor
           deriving (Eq, Read)

-- | A 0-order variable definition.
data DefZ = DefZ QName ExprZ              -- ^ v = e
          | ActualsZ QName MName [ExprZ]  -- ^ v = actuals(e0, ..., eN)
          deriving (Eq, Read)

-- | A 0-order intensional program.
type ProgZ = Prog DefZ

-- | A 0-order pattern is a constructor name and an expression.
data PatZ = PatZ CstrName ExprZ BindsVars deriving (Eq, Read)

-- | A 0-order module.
type ModZ = Mod ProgZ

instance PPrint ExprZ where
   pprintPrec _ (XZ vn) = pprintVar vn
   pprintPrec p (ConZ cn el) = prettyConst p cn el
   pprintPrec p (FZ NOp e) = pprintPrec p e
   pprintPrec p (FZ qOp e) =
      pprintPrec p qOp . (" (" ++) . pprint e . (")" ++)
   pprintPrec p (CaseZ (d, _) e pats) =
      let pprintPats []        = id
          pprintPats (p0 : pl) = pprintPat p0 . pprintPats pl
          pprintPat (PatZ c0 e0 b0) =
            nl.(" "++).spacing d.(" | "++).pprint c0.(" -> "++).
            pprintPrec p e0.pprintBinds b0
      in  ("case "++).pprintPrec p e.(" of{"++).shows d.("}"++).
          pprintPats pats
   pprintPrec _ (ConstrZ c) = pprintTH c

instance PPrint DefZ where
   pprintPrec p (DefZ vn e) =
     pprint vn.(" = " ++).pprintPrec p e
   pprintPrec _ (ActualsZ vn m es) =
     pprint vn.("{"++).(m++).("}"++).
     (" = actuals["++).pprintList (", "++) es.("]" ++)

instance PPrint PatZ where
   pprintPrec p (PatZ c e b) =
      (" | "++).pprint c.(" -> "++).pprintPrec p e.
      pprintBinds b

-- | Searches for a function definition in ZOIL.
searchDefZ :: QName -> ProgZ -> Maybe DefZ
searchDefZ _  (Prog _ []) = Nothing
searchDefZ vn1 (Prog cs (def@(DefZ vn2 _):restdefs)) 
   | vn1 == vn2		= Just def
   | otherwise 		= searchDefZ vn1 (Prog cs restdefs)
searchDefZ vn1  (Prog cs (_:restdefs)) = searchDefZ vn1 (Prog cs restdefs)

-- | Searches for an /actuals()/ declaration in ZOIL.
searchDefCaseZ :: QName -> ProgZ -> Maybe DefZ
searchDefCaseZ _  (Prog _ []) = Nothing
searchDefCaseZ vn1 (Prog cs (def@(ActualsZ vn2 _ _):restdefs)) 
   | vn1 == vn2		= Just def
   | otherwise 		= searchDefCaseZ vn1 (Prog cs restdefs)
searchDefCaseZ vn1  (Prog cs (_:restdefs)) = 
  searchDefCaseZ vn1 (Prog cs restdefs)

-- | Searches for a variable declaration in the ZOIL program.
searchZD :: QName -> ProgZ -> Maybe DefZ
searchZD vn prog = 
   if (sDef == Nothing) then 
      if (sDefCase == Nothing) then Nothing
       else sDefCase
    else sDef
      where
      sDef	= searchDefZ vn prog
      sDefCase	= searchDefCaseZ vn prog

-- | Searches a 0-order program for the actuals of a variable.
searchActuals :: QName -> ProgZ -> [DefZ]
searchActuals vn (Prog _ defs) =
  let aux (DefZ _ _) = False
      aux (ActualsZ vn' _ _) = vn==vn'
  in  filter aux defs

-- | Looks up an index in an actuals list.
lookupAct :: (PPrint a) => Int -> [a] -> a
lookupAct i acts =
  if i<length acts then acts !! i else ierr$"index "++(show i)++" too big for actuals(): "++(pprint acts "")

-- | Gets the variable name of a definition.
defVarZ :: DefZ -> QName
defVarZ (DefZ v _) = v
defVarZ (ActualsZ v _ _) = v

-- | Filter for actuals definitions.
isActualsZ :: DefZ -> Bool
isActualsZ (DefZ _ _)       = False
isActualsZ (ActualsZ _ _ _) = True

-- | Filter for function definitions.
isDefZ :: DefZ -> Bool
isDefZ (DefZ _ _)       = True
isDefZ (ActualsZ _ _ _) = False
