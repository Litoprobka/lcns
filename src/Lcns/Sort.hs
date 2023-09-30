module Lcns.Sort (invertSort, defSort, cmpWith) where

import Lcns.FileInfo (isDir)
import Lcns.Prelude

import Data.ByteString.Char8 qualified as BS (map)
import Data.Char (toLower)

invertSort :: SortFunction -> SortFunction
invertSort sortf = sortf{reversed = not sortf.reversed}

defSort :: FileInfo -> FileInfo -> Ordering
defSort = compare `on` \file -> (Down $ isDir file, BS.map toLower file.name)

cmpWith :: SortFunction -> FileInfo -> FileInfo -> Ordering
cmpWith sortf fi1 fi2 = flip' $ cmp fi1 fi2
 where
  flip' = applyWhen sortf.reversed flipOrder
  cmp = fromMaybe defSort sortf.func
