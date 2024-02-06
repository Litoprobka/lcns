{-# LANGUAGE MultiWayIf #-}

module Lcns.FileInfo (getFileInfo, isDir, isRealDir, isLink, nameOf, savedDir, symlinked, pathOf) where

import Lcns.Path
import Lcns.Prelude

import Data.HashSet qualified as Set
import System.Posix.PosixString (FileStatus, isDirectory, isSymbolicLink)

getFileInfo :: MonadIO m => Path Abs -> m RawFileInfo
getFileInfo path = tryGetFileInfo Set.empty path <&> fromMaybe RawFile{path, name = takeFileName path, status = Nothing}

-- tryGetFile info is not indended to be called directly
tryGetFileInfo :: MonadIO m => HashSet (Path Abs) -> Path Abs -> m (Maybe RawFileInfo)
tryGetFileInfo visited path = do
  tryJust (getSymbolicLinkStatus path)
    >>= traverse \status ->
      if
        | isDirectory status -> getDirInfo
        | isSymbolicLink status -> getSymlinkInfo visited status
        | otherwise -> do
            pure RawFile{path, name, status = Just status}
 where
  name = takeFileName path

  getSymlinkInfo :: MonadIO m => HashSet (Path Abs) -> FileStatus -> m RawFileInfo
  getSymlinkInfo visited' status = fmap (fromMaybe emptyLink) $ tryJust $ do
    -- "/" is never a symlink, so `takeDirectory` should be fine here
    linkedPath <- withPath id (takeDirectory path </>) <$> readSymbolicLink path
    -- if we've already encountered this path, then the link is cyclic and thus invalid
    guard $ not $ linkedPath `Set.member` visited'

    nestedInfo <- tryGetFileInfo (visited' & Set.insert linkedPath) linkedPath
    pure $ emptyLink & #link .~ nestedInfo
   where
    emptyLink = RawLink{path, name, status = Just status, link = Nothing}

  getDirInfo = do
    itemCount <- length <<$>> tryJust (listDirectory path)
    pure $ RawDir{path, name, itemCount}

isRealDir :: FileInfo -> Bool
isRealDir Dir{} = True
isRealDir SavedDir{} = True
isRealDir _ = False

isDir :: FileMap -> FileId -> Bool
isDir fileMap fid = case fileMap ^? ix fid of
  Just Dir{} -> True
  Just SavedDir{} -> True
  Just Link{link = Just linkId} -> isDir fileMap linkId
  _ -> False

isLink :: FileInfo -> Bool
isLink = has #_Link

nameOf :: FileInfo -> Path Rel
nameOf File{name} = name
nameOf Dir{name} = name
nameOf Link{name} = name
nameOf SavedDir{dir = DirNode{path}} = takeFileName path

pathOf :: FileInfo -> Path Abs
pathOf File{path} = path
pathOf Dir{path} = path
pathOf Link{path} = path
pathOf SavedDir{dir = DirNode{path}} = path

{- | Focuses an arbitrarily nested `SavedDir`.
Mostly reduntant, since it's usually called with `child` that already removes link nesting via `symlinked`,
but better be safe than sorry
-}
savedDir :: FileId -> AffineTraversal' FileMap DirNode
savedDir fid = symlinked fid % #_SavedDir

{- | Focuses the target of a (potentially nested) symlink

>>> import Lcns.Path
>>> path <- getCurrentDirectory
>>> cycle = Link{path, name = takeFileName path, link = Nothing, status = Nothing}
>>> cycle & #link ?~ cycle & has symlinked
False
-}
symlinked :: FileId -> AffineTraversal' FileMap FileInfo
symlinked fid = atraversal tryGet update where
  tryGet fileMap = case fileMap ^? ix fid of
    Nothing -> Left fileMap
    Just Link{link = Nothing} -> Left fileMap
    Just Link{link = Just nestedId} -> fileMap ^? symlinked nestedId & maybe (Left fileMap) Right
    Just nonLink -> Right nonLink

  update fileMap fileInfo = fileMap & ix fid .~ fileInfo