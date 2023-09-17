module Lcns.ListUtils (lookup, select, delete, insert, update) where

-- some helpers to use Brick.GenericList as an associative array

import           Relude

import           Lcns.Types
import           Lcns.Utils
import           Lcns.Sort

import           Brick.Widgets.List
    (listElements, listRemove, listMoveTo, listInsert)
import qualified Data.Sequence as Seq

lookup :: FilePath -> FileSeq -> Maybe Int
lookup path = 
    listElements
    .> Seq.findIndexL ((.name) .> (==path))

select :: FilePath -> FileSeq -> FileSeq
select path =
    applyJust (lookup path) listMoveTo

delete :: FilePath -> FileSeq -> FileSeq
delete path =
    applyJust (lookup path) listRemove

insert :: SortFunction -> FileInfo -> FileSeq -> FileSeq
insert sortf fi seq' =
    seq'
    & listElements
    .> Seq.takeWhileL (cmpWith sortf fi .> (/=LT))
    .> Seq.length
    .> (\i -> listInsert i fi seq')

update :: SortFunction -> FileInfo -> FileSeq -> FileSeq
update sortf fi =
    delete fi.name
    .> insert sortf fi