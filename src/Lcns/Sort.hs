module Lcns.Sort(invertSort, defSort, cmpWith) where

import           Relude

import           Data.Char     (toLower)

import           Lcns.FileInfo (isDir)
import           Lcns.Types
import           Lcns.Utils

invertSort :: SortFunction -> SortFunction
invertSort sortf = sortf { reversed = not sortf.reversed }

defSort :: FileInfo -> FileInfo -> Ordering
defSort = compare `on` \file -> (Down $ isDir file, toLower <$> file.name)

cmpWith :: SortFunction -> FileInfo -> FileInfo -> Ordering
cmpWith sortf fi1 fi2 = flip' $ cmp fi1 fi2
    where flip' = applyWhen sortf.reversed flipOrder
          cmp = fromMaybe defSort sortf.func





