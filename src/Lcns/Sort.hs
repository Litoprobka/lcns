module Lcns.Sort (invertSort, defSort, cmpWith) where

import Lcns.FileInfo (isDir, nameOf)
import Lcns.Path (fromRel)
import Lcns.FileMap ()
import Lcns.Prelude

import Data.ByteString.Char8 qualified as BS (map)
import Data.Char (toLower)

invertSort :: SortFunction -> SortFunction
invertSort = #reversed %~ not

defSort :: FileMap -> FileId -> FileId -> Ordering
defSort fileMap = compare `on` f where
  f fid = let
    lowercasedName = BS.map toLower <. fromRel <. nameOf <$> fileMap ^? ix fid -- should I use `justified-containers`?
    in (Down $ isDir fileMap fid, lowercasedName)

cmpWith :: FileMap -> SortFunction -> FileId -> FileId -> Ordering
cmpWith fileMap sortf id1 id2 = flip' $ cmp id1 id2 where
    flip' = applyWhen sortf.reversed flipOrder
    cmp = fromMaybe defSort sortf.func fileMap

