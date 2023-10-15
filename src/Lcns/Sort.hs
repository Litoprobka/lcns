module Lcns.Sort (invertSort, defSort, cmpWith) where

import Lcns.FileInfo (isDir, nameOf)
import Lcns.Path (fromRel)
import Lcns.Prelude

import Data.ByteString.Char8 qualified as BS (map)
import Data.Char (toLower)

invertSort :: SortFunction -> SortFunction
invertSort sortf = sortf{reversed = not sortf.reversed}

defSort :: FileInfo -> FileInfo -> Ordering
defSort = compare `on` \file -> (Down $ isDir file, BS.map toLower $ fromRel $ nameOf file)

cmpWith :: SortFunction -> FileInfo -> FileInfo -> Ordering
cmpWith sortf fi1 fi2 = flip' $ cmp fi1 fi2
 where
  flip' = applyWhen sortf.reversed flipOrder
  cmp = fromMaybe defSort sortf.func
