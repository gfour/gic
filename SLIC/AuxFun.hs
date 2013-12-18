-- | Auxiliay functions that do not fit anywhere else.

module SLIC.AuxFun (comment, foldDot, errM, ierr, insCommIfMore, lkUpSure,
                    mergeM, nameOf, pathOf, showNum, showStrings,
                    spaces, toLowerFirst, threadfunc_l, trace2) where

import Debug.Trace (trace)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Map (Map, lookup, unionWithKey)
import Data.Sequence (elemIndexR, fromList)
import SLIC.Constants

-- | Debugging trace.
trace2 :: String -> a -> a
trace2 = Debug.Trace.trace

-- | A 'foldl (.)'.
foldDot :: (a1 -> a -> a) -> [a1] -> a -> a
foldDot f l = foldl (.) id (map f l)

-- | Looks up an entry in a map (by key), or shows an error.
lkUpSure :: (Ord a, Show a, Show b) => a -> Map a b -> b 
lkUpSure x l =
  case Data.Map.lookup x l of
    Nothing -> ierr ("lkUpSure: "++show x++" must be existing key in "++show l)
    Just v  -> v

-- | Show positive big numbers in a nice way.
showNum :: Int -> String
showNum num =
  let showTripl n
        | n < 0     = ierr "showTripl only works for positive numbers"
        | n < 10    = "00" ++ show n
        | n < 100   = "0" ++ show n
        | n < 1000  = show n
        | otherwise = ierr "showTripl can't handle numbers => 1000"
      ndiv = num `div` 1000
      nmod = num `mod` 1000
  in  if ndiv == 0 then show nmod
      else showNum ndiv ++ "," ++ showTripl nmod

-- | Iterate a function over lists with a threaded counter.
threadfunc_l :: Int -> [a] -> (Int -> a -> (a, Int)) -> ([a], Int)
threadfunc_l i [] _ = ([], i)
threadfunc_l i (x:xs) f =
  let (x', i')   = f i x
      (xs', i'') = threadfunc_l i' xs f
  in  (x' : xs', i'')

-- | Merges two maps, aborts with an error when duplicate keys
--   are found.
mergeM :: (Ord k, Show k, Show a) => Map k a -> Map k a -> Map k a
mergeM = Data.Map.unionWithKey
         (\ k a1 a2 -> ierr $ "duplicate renaming for key "++(show k)++" and values: "++(show [a1, a2])++". Possible cause: local name inside let bindings is the same as local name inside function")

-- | Make the first letter of a string lowercase.
toLowerFirst :: String -> String
toLowerFirst [] = error "toLowerFirst: string is empty"
toLowerFirst (s:r) = (toLower s):r

-- * Error reporting

-- | Produce an 'internal error' mesage.
ierr :: String -> a
ierr msg = error $ "internal error: "++msg

-- | Show an error about something (2nd arg) happening in a module (1st arg).
errM :: (String, String) -> String -> a
errM (m, _) msg = error $ "[GIC] Module "++m++": "++msg

-- * Pretty printing

-- | Shows a list of strings, separated by a given delimiter.
showStrings :: String -> [String] -> ShowS
showStrings delim0 ps = ((intercalate delim0 ps)++)

-- | Create a comment containing some text.
comment :: ShowS -> ShowS
comment s = if comments then spaces 1.lcomment.spaces 1.s.spaces 1.rcomment.spaces 1 else id

-- | Spacing function.
spaces :: Int -> ShowS
spaces n = ((replicate n ' ')++)

-- | Prints a list, separating the elements with commas.
insCommIfMore :: [ShowS] -> ShowS
insCommIfMore [] = id
insCommIfMore [l] = l
insCommIfMore (l:ls) = l.(", "++).insCommIfMore ls

-- * Name handling

-- | Returns the path of a file path (the part until the directory separator).
pathOf :: String -> String
pathOf fName =
  case elemIndexR dirSeparator (Data.Sequence.fromList fName) of
    Nothing  -> defaultPath
    Just idx -> take idx fName

-- | Returns the filename of a file path (the part after the directory separator).
nameOf :: String -> String
nameOf fName =
  case elemIndexR dirSeparator (Data.Sequence.fromList fName) of
    Nothing  -> fName
    Just idx -> drop (idx+1) fName
