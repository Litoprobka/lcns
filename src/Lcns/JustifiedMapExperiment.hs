{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
module Lcns.JustifiedMapExperiment where

import Lcns.Prelude
import Control.Exception (PatternMatchFail(..))

newtype FileId' proof = FileId' Int deriving (Eq, Show)
type role FileId' phantom

data FileInfo' proof = FIPlaceholder
type role FileInfo' phantom

type DirHistory' proof = NonEmpty (FileId' proof)

data FileMap' proof = FileMap'
  { nextId :: FileId
  , fileIds :: HashMap (Path Abs) (FileId' proof)
  , files :: IntMap (FileInfo' proof)
  } deriving Generic
type role FileMap' phantom

data AppState' = forall proof. AppState'
  { dir :: DirHistory' proof
  , fileMap :: FileMap' proof
  , sortFunction :: SortFunction
  , watchers :: INotifyState
  }

type instance Index (FileMap' proof) = FileId' proof
type instance IxValue (FileMap' proof) = FileInfo' proof

instance Ixed (FileMap' proof) where
  type IxKind (FileMap' proof) = A_Lens
  ix :: FileId' proof -> Lens' (FileMap' proof) (FileInfo' proof)
  ix (FileId' id') = #files % lens getFile setFile where
    getFile files = case files ^? ix id' of
      Nothing -> bug $ PatternMatchFail "fileMap does not contain a verified key"
      Just file -> file
    setFile files file = files & ix id' .~ file

verify :: FileId -> FileMap' proof -> Maybe (FileId' proof)
verify (FileId mbId) fileMap = FileId' mbId <$ fileMap ^? #files % ix mbId

-- this shouldn't be possible
lie :: FileMap' proof -> FileMap' cake
lie FileMap'{..} = undefined -- FileMap'{nextId, fileIds = coerce fileIds, files = coerce files}

-- what to do with file deletion? It *may* result in broken links, and we don't want to
-- traverse the whole map on each deletion (or do we?)
--
-- what if... we just don't delete FileInfo-s, ever?
-- we only need to delete their id from a DirNode and, perhaps, invalidate links... well, that might be a problem
-- that being said, lcns doesn't yet handle link invalidation *at all*
-- the memory cost of, eh, "dangling" FileInfo-s would be negligible compared to the rest of the FileMap
--
-- todo: separate proof for DirNodes