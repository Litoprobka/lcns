module Lcns.Path (
  fromAbs,
  fromRel,
  takeFileName,
  takeDirectory,
  getCurrentDirectory,
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
  listDirectoryAbs,
  combineWithDots,
  takeParent,
  tryListDirectory,
  getModificationTime,
  tryGetModTime,
  readFileBS,
  splitDirectories,
  joinPath,
  root,
  empty,
) where

import Lcns.Prelude hiding (empty)

import Data.ByteString qualified as BS (readFile)
import System.Directory.OsPath qualified as D
import System.OsPath (OsPath)
import System.OsPath qualified as OP
import System.Posix.PosixString qualified as PS

-- convincing `coerce` to work

import Data.Time (UTCTime)
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

-- chances are, OP.joinPath is faster than a naive foldr (</>)
joinPath :: [Path Rel] -> Path Rel
joinPath = map toOS .> OP.joinPath .> mkRel

isAbsolute :: Path unknown -> Bool
isAbsolute = toOS .> OP.isAbsolute

-- the type signature is a bit janky, but it has to be, because "/" is Abs and the rest are not
splitDirectories :: Path any -> (Maybe (Path Abs), [Path Rel])
splitDirectories path = case OP.splitDirectories $ toOS path of
  (maybeRoot : rest)
    | isAbsolute path -> (Just $ mkAbs maybeRoot, map mkRel rest)
  chunks -> (Nothing, map mkRel chunks)

-- * Directory wrappers

getCurrentDirectory :: MonadIO m => m (Path Abs)
getCurrentDirectory = mkAbs <$> io D.getCurrentDirectory

setCurrentDirectory :: MonadIO m => Path any -> m ()
setCurrentDirectory = wrapIO D.setCurrentDirectory

doesDirectoryExist :: MonadIO m => Path Abs -> m Bool
doesDirectoryExist = wrapIO D.doesDirectoryExist

removeFile :: MonadIO m => Path any -> m ()
removeFile = wrapIO D.removeFile

listDirectory :: MonadIO m => Path any -> m [Path Rel]
listDirectory path = map mkRel <$> io (D.listDirectory $ toOS path)

listDirectoryAbs :: MonadIO m => Path a -> m [Path a]
listDirectoryAbs path = map (path </>) <$> listDirectory path

getModificationTime :: MonadIO m => Path any -> m UTCTime
getModificationTime = wrapIO D.getModificationTime

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

fromAbs :: Path Abs -> ByteString
fromAbs = toSBS .> fromShort

fromRel :: Path Rel -> ByteString
fromRel = toSBS .> fromShort

fromRaw :: RawFilePath -> Path Unknown
fromRaw = toShort .> coerce

decode :: Path any -> Text
decode = toSBS .> fromShort .> decodeUtf8

withPath :: (Path Abs -> a) -> (Path Rel -> a) -> Path any -> a
withPath onAbs onRel path'
  | isAbsolute path' = onAbs $ mkAbs path
  | otherwise = onRel $ mkRel path
 where
  path = toOS path'

-- root path
root :: Path Abs
root = coerce ("/" :: ShortByteString)

empty :: Path Rel
empty = coerce ("" :: ShortByteString)

{- | Combine two paths. If the second path is "..", normalise it
(once again, I couldn't come up with a decent name)
-}
combineWithDots :: Path a -> Path Rel -> Maybe (Path a)
combineWithDots base rel
  | toSBS rel == ".." = takeParent base
  | otherwise = Just $ base </> rel

takeParent :: Path a -> Maybe (Path a)
takeParent path
  | toSBS path == "" = Nothing
  | toSBS path == "/" = Nothing
  | otherwise = Just $ takeDirectory path

tryListDirectory :: MonadIO m => Path a -> m [Path a]
tryListDirectory path = do
  try @SomeException (listDirectoryAbs path) <&> fromRight []

tryGetModTime :: MonadIO m => Path any -> m (Maybe UTCTime)
tryGetModTime = tryJust <. getModificationTime

readFileBS :: MonadIO m => Path any -> m ByteString
readFileBS path = io $ BS.readFile $ toString $ decode path
