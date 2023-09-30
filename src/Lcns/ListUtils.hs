module Lcns.ListUtils (lookup, select, delete, insert, update) where

-- some helpers to use Brick.GenericList as an associative array

import Brick.Widgets.List (
  listElements,
  listInsert,
  listMoveTo,
  listRemove,
 )
import Data.Sequence qualified as Seq
import Lcns.Prelude
import Lcns.Sort

lookup :: RawFilePath -> FileSeq -> Maybe Int
lookup path =
  listElements
    .> Seq.findIndexL ((.name) .> (== path))

select :: RawFilePath -> FileSeq -> FileSeq
select path =
  applyJust (lookup path) listMoveTo

delete :: RawFilePath -> FileSeq -> FileSeq
delete path =
  applyJust (lookup path) listRemove

insert :: SortFunction -> FileInfo -> FileSeq -> FileSeq
insert sortf fi seq' =
  seq'
    & listElements
    .> Seq.takeWhileL (cmpWith sortf fi .> (/= LT))
    .> Seq.length
    .> (\i -> listInsert i fi seq')

update :: SortFunction -> FileInfo -> FileSeq -> FileSeq
update sortf fi =
  delete fi.name
    .> insert sortf fi
