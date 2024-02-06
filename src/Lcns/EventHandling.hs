{-# LANGUAGE LambdaCase #-}

module Lcns.EventHandling (
  handleVtyEvent,
  handleAppEvent,
  handleEventWith,
  moveUp,
  moveDown,
  open,
  moveBack,
  delete,
  invertSort,
  toggleDotfiles,
  refreshWatchers,
) where

import Lcns.DirHistory
import Lcns.FileInfo
import Lcns.FileTracker
import Lcns.ListUtils qualified as LU
import Lcns.Prelude

import Brick
import Brick.Widgets.List (
  listClear,
  listRemove,
  listSelectedElement,
 )
import Graphics.Vty.Input.Events
import Lcns.Path
import System.INotify qualified as IN (Event (..))
import System.Posix.ByteString (RawFilePath)
import Lcns.FileMap (addRawFile, lookupId)
import Control.Exception (PatternMatchFail(..))

moveUp :: AppM ()
moveUp = goUp >> refreshWatchers

moveDown :: AppM ()
moveDown = goDown >> refreshWatchers

open :: AppM ()
open =
  preuse childId
    >>= onJust (openFile >=> const refreshWatchers)

moveBack :: AppM ()
moveBack = goLeft >> refreshWatchers

delete :: AppM ()
delete = do
  useOrPass (curDir % #files % to listSelectedElement % _Just) \(index, fileId) -> do
      fileMap <- use #fileMap
      let (Just file) = fileMap ^? ix fileId
      removeFile file.path
      curDir % #files %= listRemove index

invertSort :: AppM ()
invertSort = #sortFunction % #reversed %= not >> rebuild

toggleDotfiles :: AppM ()
toggleDotfiles = #sortFunction % #showDotfiles %= not >> rebuild

refreshWatchers :: (MonadIO m, MonadState AppState m, MonadReader AppEnv m) => m ()
refreshWatchers = do
  env <- ask
  getDirPure <- getDirs
  traversing (#watchers % #all) killWatcher -- hinotify (or inotify itself) treats multiple WatchDescriptor-s  on the same directory as one
  traversing (#watchers % #all) (refreshWatch env getDirPure) -- so we have to remove all of them before recreating*

getDirs :: (MonadIO m, MonadState AppState m) => m (WhichDir -> Maybe (Path Abs))
getDirs = do
  path <- use $ curDir % #path
  childDirPath <- preuse $ childDir % #path
  pure $ \case
    Parent -> takeParent path
    Current -> Just path
    Child -> childDirPath

refreshWatch :: MonadIO m => AppEnv -> (WhichDir -> Maybe (Path Abs)) -> DirWatcher -> m DirWatcher
refreshWatch env getDir dw =
  getDir dw.dir & \case
    Nothing -> killWatcher dw
    Just dir' ->
      watchDir
        dw
        env.inotify
        dir'
        env.channel

handleVtyEvent :: Event -> AppM ()
handleVtyEvent event = do
  cfg <- asking #config
  case event of
    EvKey key mods -> cfg.keybindings key mods
    _ -> pass

handleAppEvent :: LcnsEvent -> AppM ()
handleAppEvent (DirEvent dir event) = case event of
  IN.Accessed _ (Just path) -> evUpdate path
  IN.Modified _ (Just path) -> evUpdate path
  IN.Attributes _ (Just path) -> evUpdate path
  IN.Opened _ (Just path) -> evUpdate path
  IN.Created _ path -> evCreate path
  IN.MovedIn _ path _ -> evCreate path
  IN.Deleted _ path -> evDelete path
  IN.MovedOut _ path _ -> evDelete path
  _ -> pass -- for Ignored and the like
 where
  files :: AffineTraversal' AppState FileSeq
  files = (\opt -> opt % #files) $ case dir of
    Current -> curDir % idTrav
    Parent -> parentDir
    Child -> childDir

  actOnFile
    :: AddFileOrNot -- whether to create and add a new file
    -> (FileMap -> SortFunction -> FileId -> FileSeq -> FileSeq) -- FileSeq update function
    -> RawFilePath
    -> Path Abs
    -> AppM ()
  actOnFile addFile f fileName parentPath = do
    let path = parentPath </> takeFileName (fromRaw fileName)
    when (addFile == AddFile) do
      rawFileInfo <- getFileInfo path
      #fileMap %= addRawFile rawFileInfo

    sortf <- use #sortFunction
    fileMap <- use #fileMap
    let fid = case lookupId fileMap path of
            Nothing -> bug $ PatternMatchFail "actOnFile called on a non-existent file"
            Just fid' -> fid'
    files %= f fileMap sortf fid

  withParent :: (Path Abs -> AppM ()) -> AppM ()
  withParent action = do
    getDirs ?? dir >>= \case
      Just parentPath -> action parentPath
      Nothing ->
        files %= listClear

  evUpdate :: RawFilePath -> AppM ()
  evUpdate = withParent <. actOnFile AddFile LU.update
  evCreate = withParent <. actOnFile AddFile LU.insert
  evDelete = withParent <. actOnFile Don't (\_ _ -> LU.delete)

data AddFileOrNot = AddFile | Don't deriving Eq

handleEventWith :: BrickEvent n LcnsEvent -> AppM ()
handleEventWith event = case event of
  VtyEvent vtye -> handleVtyEvent vtye
  AppEvent appEv -> handleAppEvent appEv
  MouseDown{} -> pass
  MouseUp{} -> pass

openFile :: FileId -> AppM ()
openFile fid = do
  fileMap <- use #fileMap
  if isDir fileMap fid
    then goRight
    else do
      let name = fileMap ^? ix fid % #name <&> (\(Path p) -> p) & maybeToList
      executeFile (fromRaw "xdg-open") True name Nothing

rebuild :: AppM ()
rebuild = do
  useOrPass curDirId (refreshSavedDir' True)
  useOrPass parent (refreshSavedDir' True)
  refreshSelected' True