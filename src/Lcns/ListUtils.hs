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
import Lcns.Prelude hiding (empty)
import Lcns.Sort

lookup :: Path Rel -> FileSeq -> Maybe Int
lookup path =
  listElements
    .> Seq.findIndexL ((.name) .> (== path))

select :: Path Rel -> FileSeq -> FileSeq
select path =
  applyJust (lookup path) listMoveTo

selectMaybe :: Maybe (Path Rel) -> FileSeq -> FileSeq
selectMaybe mpath files = maybe files (`select` files) mpath

delete :: Path Rel -> FileSeq -> FileSeq
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

empty :: Text -> FileSeq
empty name = list name Seq.empty 0
