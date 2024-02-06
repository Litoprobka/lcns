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

lookup :: FileId -> FileSeq -> Maybe Int
lookup fid =
  listElements
    .> Seq.findIndexL (== fid)

select :: FileId -> FileSeq -> FileSeq
select fid =
  applyJust (lookup fid) listMoveTo

selectMaybe :: Maybe FileId -> FileSeq -> FileSeq
selectMaybe mbId files = mbId & maybe files (`select` files)

delete :: FileId -> FileSeq -> FileSeq
delete path =
  applyJust (lookup path) listRemove

insert :: FileMap -> SortFunction -> FileId -> FileSeq -> FileSeq
insert fileMap sortf file seq' =
  seq'
    & listElements
      .> Seq.takeWhileL (cmpWith fileMap sortf file .> (/= LT))
      .> Seq.length
      .> \i -> listInsert i file seq'

update :: FileMap -> SortFunction -> FileId -> FileSeq -> FileSeq
update fileMap sortf fid =
  delete fid .> insert fileMap sortf fid

empty :: Text -> FileSeq
empty name = list name Seq.empty 0
