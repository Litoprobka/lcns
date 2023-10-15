module Lcns.ListUtils (lookup, select, delete, insert, update, selectMaybe, empty) where

-- some helpers to use Brick.GenericList as an associative array

import Brick.Widgets.List (
  list,
  listElements,
  listInsert,
  listMoveTo,
  listRemove,
 )
import Data.Sequence qualified as Seq
import Lcns.FileInfo (nameOf)
import Lcns.Prelude hiding (empty)
import Lcns.Sort

lookup :: Path Rel -> FileSeq -> Maybe Int
lookup path =
  listElements
    .> Seq.findIndexL (nameOf .> (== path))

select :: Path Rel -> FileSeq -> FileSeq
select path =
  applyJust (lookup path) listMoveTo

selectMaybe :: Maybe (Path Rel) -> FileSeq -> FileSeq
selectMaybe mpath files = maybe files (`select` files) mpath

delete :: Path Rel -> FileSeq -> FileSeq
delete path =
  applyJust (lookup path) listRemove

insert :: SortFunction -> FileInfo -> FileSeq -> FileSeq
insert sortf file seq' =
  seq'
    & listElements
      .> Seq.takeWhileL (cmpWith sortf file .> (/= LT))
      .> Seq.length
      .> \i -> listInsert i file seq'

update :: SortFunction -> FileInfo -> FileSeq -> FileSeq
update sortf file =
  delete (nameOf file)
    .> insert sortf file

empty :: Text -> FileSeq
empty name = list name Seq.empty 0
