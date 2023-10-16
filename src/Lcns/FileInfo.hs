{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

module Lcns.FileInfo (getFileInfo, isDir, isRealDir, isLink, nameOf, savedDir, symlinked) where

import Lcns.Prelude

import Lcns.Path

import System.Posix.PosixString (FileStatus, isDirectory, isSymbolicLink)

getFileInfo :: MonadIO m => Path Abs -> m FileInfo
getFileInfo path = tryGetFileInfo path <&> fromMaybe File{path, name = takeFileName path, status = Nothing}

tryGetFileInfo :: MonadIO m => Path Abs -> m (Maybe FileInfo)
tryGetFileInfo path = do
  tryJust (getSymbolicLinkStatus path)
    >>= traverse \status ->
      if
        | isDirectory status -> getDirInfo
        | isSymbolicLink status -> getSymlinkInfo status
        | otherwise -> pure $ emptyFile & #status ?~ status
 where
  name = takeFileName path
  emptyFile = File{path, name, status = Nothing}

  getSymlinkInfo :: MonadIO m => FileStatus -> m FileInfo
  getSymlinkInfo status = fmap (fromMaybe emptyLink) $ tryJust $ do
    -- "/" is never a symlink, so `takeDirectory` should be fine here
    linkedPath <- withPath id (takeDirectory path </>) <$> readSymbolicLink path
    nestedInfo <- tryGetFileInfo linkedPath
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
nameOf SavedDir{dir = DirTree{path}} = takeFileName path

{- | Focuses an arbitrarily nested `SavedDir`.
Mostly reduntant, since it's usually called with `child` that already removes link nesting via `symlinked`,
but better be safe than sorry
-}
savedDir :: AffineTraversal' FileInfo DirTree
savedDir = symlinked % #_SavedDir

{- | Focuses the target of a (potentially nested) symlink
>>> import Lcns.Path
>>> path <- getCurrentDirectory
>>> no = Link{path, name = takeFileName path, link = Nothing, status = Nothing}
>>> no & #link ?~ no & has symlinked
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
