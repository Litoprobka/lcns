{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Lcns.FileInfo (getFileInfo, isDir, isRealDir, isLink) where

import Lcns.Prelude

import Control.Exception (try)
import RawFilePath.Directory (listDirectory)
import System.Posix.ByteString (
  FileStatus,
  getFileStatus,
  getSymbolicLinkStatus,
  isDirectory,
  isSymbolicLink,
  readSymbolicLink,
 )

getFileInfo :: RawFilePath -> IO FileInfo
getFileInfo name = do
  status' <- getSymbolicLinkStatus name
  (typedInfo, status) <- getTypedInfo status' name

  pure $ FileInfo{..}
 where
  getTypedInfo :: FileStatus -> RawFilePath -> IO (FileType, FileStatus)
  getTypedInfo status path = do
    if
      | isDirectory status -> do
          itemCount <- length <$> listDirectory path
          pure (Dir $ DirData{..}, status)
      | isSymbolicLink status -> do
          path' <- readSymbolicLink path
          try @SomeException (getFileStatus path')
            >>= mapM (`getTypedInfo` path')
              <&> \case
                Left _ -> (Link Nothing, status)
                Right fileInfo -> first (Link . Just) fileInfo
      | otherwise -> pure (File FileData, status)

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
