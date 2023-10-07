{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Lcns.FileInfo (getFileInfo, isDir, isRealDir, isLink) where

import Lcns.Prelude

import Control.Exception (try)
import Lcns.Path

import Data.List.NonEmpty qualified as NE (unzip) -- why the hell is generalised `unzip` exported there?
import System.Posix.PosixString (FileStatus, isDirectory, isSymbolicLink)

getFileInfo :: Path Abs -> IO FileInfo
getFileInfo path = do
  let name = takeFileName path
  try @SomeException (getSymbolicLinkStatus path)
    >>= \case
      Right status' -> do
        (typedInfo, status) <- NE.unzip <$> getTypedInfo status' path
        pure FileInfo{..}
      Left _ -> pure FileInfo{name, status = Nothing, typedInfo = Nothing}

getTypedInfo :: FileStatus -> Path Abs -> IO (Maybe (FileType, FileStatus))
getTypedInfo status path' = fmap eitherToMaybe $ try @SomeException $ do
  if
    | isDirectory status -> getDirInfo
    | isSymbolicLink status -> getSymlinkInfo
    | otherwise -> pure (File FileData, status)
 where
  getSymlinkInfo = do
    -- "/" is never a symlink, so `takeDirectory` should be fine here
    linkedPath <- withPath id (takeDirectory path' </>) <$> readSymbolicLink path'
    try @SomeException (getFileStatus linkedPath)
      >>= mapM (`getTypedInfo` linkedPath)
      <&> \case
        Right (Just typedInfo) -> first (Link <. Just) typedInfo
        _ -> (Link Nothing, status)

  getDirInfo = do
    itemCount <-
      try @SomeException (listDirectory path')
        <&> either (const Nothing) (Just <. length)
    pure (Dir DirData{..}, status)

isRealDir :: FileInfo -> Bool
isRealDir FileInfo{typedInfo = Just (Dir _)} = True
isRealDir _ = False

isDir :: FileInfo -> Bool
isDir FileInfo{typedInfo = Nothing} = False
isDir FileInfo{typedInfo = Just info} = dirHelper info
 where
  dirHelper (Dir _) = True
  dirHelper (Link (Just l)) = dirHelper l
  dirHelper _ = False

isLink :: FileInfo -> Bool
isLink FileInfo{typedInfo = Just (Link _)} = True
isLink _ = False
