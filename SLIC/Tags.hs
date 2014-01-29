-- | Tags for value annotations.
-- 

module SLIC.Tags (builtinTags, intTag, integerTag, listTag, findTagOfDT, uTag,
                  mBoolTag, mIntTag) where

import Data.Map (Map, fromList, lookup, toList)
import SLIC.AuxFun (foldDot, ierr)
import SLIC.LAR.LARAux (ConfigLAR(getOptions))
import SLIC.State (Options(optTag))
import SLIC.Types

-- | Unknown tags, placeholder.
uTag :: ShowS
uTag = ("0"++)
    
-- | The tag for the built-in Int data type.
builtinTag :: DTName -> ShowS
builtinTag dt = shows (findTagOfDT dt builtinTags)

-- | The tag for the Int type.
intTag :: ShowS
intTag = builtinTag dtInt

-- | The tag for the Bool type.
boolTag :: ShowS
boolTag = builtinTag dtBool

-- | The tag for the Integer type.
integerTag :: ShowS
integerTag = builtinTag dtInteger

-- | The tag for the list type.
listTag :: ShowS
listTag = builtinTag dtList

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

mTag :: ShowS -> ConfigLAR -> ShowS
mTag tag config = if (optTag $ getOptions config) then (", "++).tag else id

mIntTag     :: ConfigLAR -> ShowS ; mIntTag     = mTag intTag
mBoolTag    :: ConfigLAR -> ShowS ; mBoolTag    = mTag boolTag
