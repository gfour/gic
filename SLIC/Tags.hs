-- | Tags for value annotations.
-- 

module SLIC.Tags (builtinTags, intTag, listTag, findTagOfDT, uTag) where

import Data.Map (Map, fromList, lookup, toList)
import SLIC.AuxFun (foldDot, ierr)
import SLIC.State (Options, optTag)
import SLIC.Types

-- | Unknown tags, placeholder.
uTag :: Options -> ShowS
uTag opts = if optTag opts then ("0 /* unknown tag */, "++) else id      
    
-- | The tag for the built-in Int data type.
builtinTag :: Options -> DTName -> ShowS
builtinTag opts dt =
  if optTag opts then shows (findTagOfDT dt builtinTags).(", "++) else id

-- | The tag for the Int type.
intTag :: Options -> ShowS
intTag opts = builtinTag opts dtInt

-- | The tag for the list type.
listTag :: Options -> ShowS
listTag opts = builtinTag opts dtList

-- | A tag is a nuber.
type Tag = Int

-- | An assignment from data types to tags.
type Tags = Map DTName Tag                                                  

pprintTags :: Tags -> ShowS
pprintTags tags =
  let aux (dt, tag) = pprint dt.(" : "++).shows tag
  in  foldDot aux $ toList tags

-- | The tags assigned to built-in data types.
builtinTags :: Tags
builtinTags = fromList $ zip builtinDTypes [0..]

-- | Finds the tag of a data type in the tags table. Fails if no tag is found.
findTagOfDT :: DTName -> Tags -> Tag
findTagOfDT dt tags =
  case Data.Map.lookup dt tags of
    Just tId -> tId
    Nothing  -> ierr $ (qName dt)++" is not in tags: "++(pprintTags tags "")

