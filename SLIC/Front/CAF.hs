-- | A simple analysis that finds constant applicative forms (CAFs) and
--   assigns them indices in a special LAR to be used for memoization.
-- 
--   For more information about CAFs, check 
--   <http://www.haskell.org/haskellwiki/Constant_applicative_form>.
-- 

module SLIC.Front.CAF (nmsids2nms, getCAFid, getCAFDcts) where

import SLIC.SyntaxAux
import SLIC.SyntaxFL
import SLIC.Types

-- | Gets the name of a defined function, if it is a CAF.
--   Constructor functions are not considered CAFs (even if nullary).
nmifCAF :: DefF -> Maybe QName
nmifCAF (DefF  _ [] (ConstrF _ _)) = Nothing
nmifCAF (DefF vn []             _) = Just vn
nmifCAF _                          = Nothing

-- | Returns a dictionary of the CAFs found in an FL module.
getCAFDcts :: ModF -> CAFDct
getCAFDcts m =
  let Prog _ defs = modProg m
      addSomething (Just x) l = x:l
      addSomething Nothing  l = l
  in  nms2nmsids $ foldr addSomething [] (map nmifCAF defs)

-- | Takes a list of names (names of CAFs) and returns a
--   list of name-id pairs. Ids are unique numbers that
--   are used as indices of CAFs in a global LAR.
nms2nmsids :: [QName] -> CAFDct
nms2nmsids vnl = 
  let f x l = if x == 0 then x:l else f (x-1) (x:l)
  in  zip vnl (f ((length vnl)-1) [])

-- | Takes a CAF name and returns its id.
getCAFid :: QName -> CAFDct -> Maybe CAFId
getCAFid vn dct = lookup vn dct

-- | Takes a list of name-id pairs and returns the list of names.
nmsids2nms :: CAFDct -> [QName]
nmsids2nms dct = map fst dct
