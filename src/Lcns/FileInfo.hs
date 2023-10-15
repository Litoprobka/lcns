{-# LANGUAGE LambdaCase #-}

module Lcns.FileInfo (getFileInfo, isDir, isRealDir, isLink, nameOf) where

import Lcns.Prelude

import Lcns.Path

import System.Posix.PosixString (FileStatus, isDirectory, isSymbolicLink)

getFileInfo :: MonadIO m => Path Abs -> m FileInfo
getFileInfo path = do
  try @SomeException (getSymbolicLinkStatus path)
    >>= \case
      Left _ -> pure emptyFile
      Right status
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
    nestedInfo <- getFileInfo linkedPath
    pure $ emptyLink & #link ?~ nestedInfo
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
isLink Link{} = True
isLink _ = False

nameOf :: FileInfo -> Path Rel
nameOf File{name} = name
nameOf Dir{name} = name
nameOf Link{name} = name
nameOf SavedDir{dir = DirTree{path}} = takeFileName path
