module Lcns.Path (
  fromAbs,
  fromRel,
  takeFileName,
  takeDirectory,
  getCurrentDirectory,
  getParentDirectory,
  makeAbsolute,
  (</>),
  listDirectory,
  getFileStatus,
  getSymbolicLinkStatus,
  readSymbolicLink,
  withPath,
  decode,
  removeFile,
  fromRaw,
  setCurrentDirectory,
  doesDirectoryExist,
  executeFile,
) where

import Lcns.Prelude

import System.Directory.OsPath qualified as D
import System.OsPath (OsPath)
import System.OsPath qualified as OP

import System.Posix.PosixString qualified as PS

-- convincing `coerce` to work
import System.OsString.Internal.Types (OsString (..), PosixString (..))
import System.Posix.ByteString (RawFilePath)

-- * Some convenience functions to write Path wrappers, not meant for external use

toSBS :: Path any -> ShortByteString
toSBS = coerce

-- System.Directory doesn't provide a PlatformPath API, but some functions are handier than those provided by Unix
toOS :: Path any -> OsPath
toOS = coerce

withPosix :: MonadIO m => (PosixString -> IO a) -> Path any -> m a
withPosix f (Path path) = io $ f path

mkRel :: OsPath -> Path Rel
mkRel = coerce

mkAbs :: OsPath -> Path Abs
mkAbs = coerce

sameAs :: Path a -> OsPath -> Path a
sameAs _ = coerce

wrapOS :: (OsPath -> OsPath) -> Path a -> Path b
wrapOS f = toOS .> f .> coerce

wrapIO :: MonadIO m => (OsPath -> IO a) -> Path any -> m a
wrapIO f = toOS .> f .> io

-- * Filepath wrappers

takeFileName :: Path any -> Path Rel
takeFileName = wrapOS OP.takeFileName

takeDirectory :: Path a -> Path a
takeDirectory = wrapOS OP.takeDirectory

infixr 5 </>
(</>) :: Path a -> Path Rel -> Path a
parent </> child = sameAs parent $ toOS parent `OP.combine` toOS child

-- * Directory wrappers

getCurrentDirectory :: MonadIO m => m (Path Abs)
getCurrentDirectory = mkAbs <$> io D.getCurrentDirectory

setCurrentDirectory :: MonadIO m => Path any -> m ()
setCurrentDirectory = wrapIO D.setCurrentDirectory

doesDirectoryExist :: MonadIO m => Path any -> m Bool
doesDirectoryExist = wrapIO D.doesDirectoryExist

removeFile :: MonadIO m => Path any -> m ()
removeFile = wrapIO D.removeFile

makeAbsolute :: MonadIO m => Path any -> m (Path Abs)
makeAbsolute path
  | toSBS path == ".." = getParentDirectory
  | otherwise = io $ mkAbs <$> io (D.makeAbsolute $ toOS path)

listDirectory :: MonadIO m => Path any -> m [Path Rel]
listDirectory path = map mkRel <$> io (D.listDirectory $ toOS path)

-- * Posix wrappers

getFileStatus :: MonadIO m => Path any -> m PS.FileStatus
getFileStatus = withPosix PS.getFileStatus

getSymbolicLinkStatus :: MonadIO m => Path any -> m PS.FileStatus
getSymbolicLinkStatus = withPosix PS.getSymbolicLinkStatus

readSymbolicLink :: MonadIO m => Path any -> m (Path Unknown)
readSymbolicLink = withPosix PS.readSymbolicLink .> fmap Path

executeFile :: MonadIO m => Path any -> Bool -> [PosixString] -> Maybe [(PosixString, PosixString)] -> m a
executeFile (Path path) usePATH args env =
  io $ PS.executeFile path usePATH args env

-- * Misc

getParentDirectory :: MonadIO m => m (Path Abs)
getParentDirectory = takeDirectory <$> getCurrentDirectory

fromAbs :: Path Abs -> ByteString
fromAbs = toSBS .> fromShort

fromRel :: Path Rel -> ByteString
fromRel = toSBS .> fromShort

fromRaw :: RawFilePath -> Path any
fromRaw = toShort .> coerce

decode :: Path any -> Text
decode = toSBS .> fromShort .> decodeUtf8

withPath :: (Path Abs -> a) -> (Path Rel -> a) -> Path any -> a
withPath onAbs onRel path'
  | OP.isAbsolute path = onAbs $ mkAbs path
  | otherwise = onRel $ mkRel path
 where
  path = toOS path'