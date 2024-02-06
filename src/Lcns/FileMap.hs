{-# LANGUAGE TypeFamilies #-}

module Lcns.FileMap (fileIn, empty, addFile, addFileOverride, lookupId, addRawFile, getIdOrCrash) where

import Lcns.FileInfo (pathOf)
import Lcns.Prelude hiding (empty)
import Control.Exception (PatternMatchFail(PatternMatchFail))

empty :: FileMap
empty = FileMap{nextId = FileId 0, fileIds = Empty, files = Empty}

-- | Adds a new file to the FileMap, overriding the old one if it exists
addFileOverride :: FileInfo -> FileMap -> FileMap
addFileOverride file fileMap =
  fileMap
    & #nextId .~ nextId
    & #fileIds % at path ?~ idOfNewFile
    & at idOfNewFile ?~ file
 where
  path = pathOf file
  inc (FileId id') = FileId $ id' + 1
  
  (nextId, idOfNewFile) = case fileMap.fileIds ^? ix path of
    -- if the file already is in the map, it gets replaced
    Just existingId -> (fileMap.nextId, existingId)
    Nothing -> (inc nextId, nextId)

-- | Adds a new file to the FileMap. If the file already exists, does nothing
addFile :: FileInfo -> FileMap -> FileMap
addFile file fileMap = fromMaybe fileMap do
  fid <- fileMap.fileIds ^? ix (pathOf file)
  guard $ fileMap & hasn't (at fid)
  pure $ addFileOverride file fileMap

-- | Adds a RawFileInfo to the FileMap. If it's a RawLink, it recursively adds the linked files
addRawFile :: RawFileInfo -> FileMap -> FileMap
addRawFile rawFile fileMap = fileMap & case rawFile of
  RawFile{..} -> addFileOverride File{path, name, status, contents = Nothing}
  RawDir{..} -> addFile Dir{..} -- account for `SavedDir`s
  RawLink{..} -> case link of
    Nothing -> addFileOverride Link{path, name, status, link = Nothing}
    Just nested -> addRawFile nested .> \updatedMap ->
      updatedMap
      & addFileOverride Link{path, name, status, link = lookupId updatedMap nested.path}

lookupId :: FileMap -> Path Abs -> Maybe FileId
lookupId FileMap{fileIds} path = fileIds ^? ix path

getIdOrCrash :: FileMap -> Path Abs -> FileId
getIdOrCrash fileMap path = case lookupId fileMap path of
  Nothing -> bug $ PatternMatchFail "no such file"
  Just fileId -> fileId

-- | Pretty much `at` with arguments flipped. Since the FileMap is constant here, it cannot be used for updating
fileIn :: FileMap -> AffineFold FileId FileInfo
fileIn files = afolding \fid -> files ^. at fid
