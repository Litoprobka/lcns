{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Lcns.FileInfo (getFileInfo, isDir, isRealDir, isLink) where

import Lcns.Prelude

import Control.Exception (try)
import Lcns.Path

import System.Posix.PosixString (FileStatus, isDirectory, isSymbolicLink)

getFileInfo :: Path Abs -> IO FileInfo
getFileInfo path = do
  let name = takeFileName path
  status' <- getSymbolicLinkStatus path
  (typedInfo, status) <- getTypedInfo status' path

  pure FileInfo{..}

getTypedInfo :: FileStatus -> Path Abs -> IO (FileType, FileStatus)
getTypedInfo status path' = do
  if
    | isDirectory status -> getDirInfo
    | isSymbolicLink status -> getSymlinkInfo
    | otherwise -> pure (File FileData, status)
 where
  getSymlinkInfo = do
    linkedPath <- withPath id (takeDirectory path' </>) <$> readSymbolicLink path'
    try @SomeException (getFileStatus linkedPath)
      >>= mapM (`getTypedInfo` linkedPath)
        <&> \case
          Left _ -> (Link Nothing, status)
          Right fileInfo -> first (Link . Just) fileInfo

  getDirInfo = do
    itemCount <-
      try @SomeException (listDirectory path')
        <&> either (const Nothing) (Just <. length)
    pure (Dir DirData{..}, status)

isRealDir :: FileInfo -> Bool
isRealDir FileInfo{typedInfo = Dir _} = True
isRealDir _ = False

isDir :: FileInfo -> Bool
isDir FileInfo{typedInfo = info} = dirHelper info
 where
  dirHelper (Dir _) = True
  dirHelper (Link (Just l)) = dirHelper l
  dirHelper _ = False

isLink :: FileInfo -> Bool
isLink FileInfo{typedInfo = Link _} = True
isLink _ = False
