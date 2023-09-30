module Lcns.FilePathExtras (makeAbsolute) where

import Lcns.Prelude

import System.FilePath.ByteString (takeDirectory, (</>))
import System.Posix.ByteString (getWorkingDirectory)

makeAbsolute :: RawFilePath -> IO RawFilePath
makeAbsolute ".." = takeDirectory <$> getWorkingDirectory
makeAbsolute path = do
  parentDir <- getWorkingDirectory
  pure $ parentDir </> path
