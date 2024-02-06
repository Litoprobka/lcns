{-# LANGUAGE MultiWayIf #-}

module Lcns.FileInfo (getFileInfo, isDir, isRealDir, isLink, nameOf, savedDir, symlinked, pathOf) where

import Lcns.Path
import Lcns.Prelude

import Data.HashSet qualified as Set
import System.Posix.PosixString (FileStatus, isDirectory, isSymbolicLink)

getFileInfo :: MonadIO m => Path Abs -> m FileInfo
getFileInfo path = tryGetFileInfo Set.empty path <&> fromMaybe File{path, name = takeFileName path, status = Nothing, contents = Nothing}

-- tryGetFile info is not indended to be called directly
tryGetFileInfo :: MonadIO m => HashSet (Path Abs) -> Path Abs -> m (Maybe FileInfo)
tryGetFileInfo visited path = do
  tryJust (getSymbolicLinkStatus path)
    >>= traverse \status ->
      if
        | isDirectory status -> getDirInfo
        | isSymbolicLink status -> getSymlinkInfo visited status
        | otherwise -> do
            pure File{path, name, status = Just status, contents = Nothing}
 where
  name = takeFileName path

  getSymlinkInfo :: MonadIO m => HashSet (Path Abs) -> FileStatus -> m FileInfo
  getSymlinkInfo visited' status = fmap (fromMaybe emptyLink) $ tryJust $ do
    -- "/" is never a symlink, so `takeDirectory` should be fine here
    linkedPath <- withPath id (takeDirectory path </>) <$> readSymbolicLink path
    -- if we've already encountered this path, then the link is cyclic and thus invalid
    guard $ not $ linkedPath `Set.member` visited'

    nestedInfo <- tryGetFileInfo (visited' & Set.insert linkedPath) linkedPath
    pure $ emptyLink & #link .~ nestedInfo
   where
    emptyLink = Link{path, name, status = Just status, link = Nothing}

  getDirInfo = do
    itemCount <- length <<$>> tryJust (listDirectory path)
    pure $ Dir{path, name, itemCount}

isRealDir :: FileInfo -> Bool
isRealDir Dir{} = True
isRealDir SavedDir{} = True
isRealDir _ = False

isDir :: FileInfo -> Bool
isDir Dir{} = True
isDir SavedDir{} = True
isDir Link{link = Just l} = isDir l
isDir _ = False

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
savedDir :: AffineTraversal' FileInfo DirNode
savedDir = symlinked % #_SavedDir

{- | Focuses the target of a (potentially nested) symlink

>>> import Lcns.Path
>>> path <- getCurrentDirectory
>>> cycle = Link{path, name = takeFileName path, link = Nothing, status = Nothing}
>>> cycle & #link ?~ cycle & has symlinked
False
-}
symlinked :: AffineTraversal' FileInfo FileInfo
symlinked = atraversal tryGet update
 where
  tryGet l@Link{link = Nothing} = Left l
  tryGet l@Link{link = Just nested} = tryGet nested & _Left %~ \other -> l & #link ?~ other
  tryGet nonLink = Right nonLink

  update l@Link{link = Nothing} _ = l
  update l@Link{link = Just nested} new = l & #link ?~ update nested new
  update _ new = new
