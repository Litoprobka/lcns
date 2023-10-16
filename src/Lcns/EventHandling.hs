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

import Lcns.DirTree
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

moveUp :: AppM ()
moveUp = goUp >> refreshWatchers

moveDown :: AppM ()
moveDown = goDown >> refreshWatchers

open :: AppM ()
open =
  preuse (#dir % child)
    >>= onJust (openFile >=> const refreshWatchers)

moveBack :: AppM ()
moveBack = goLeft >> refreshWatchers

delete :: AppM ()
delete = do
  preuse (#dir % #files % to listSelectedElement % _Just)
    >>= onJust \(index, file) -> do
      removeFile file.path
      #dir % #files %= listRemove index

invertSort :: AppM ()
invertSort = #sortFunction % #reversed %= not >> rebuild

toggleDotfiles :: AppM ()
toggleDotfiles = #sortFunction % #showDotfiles %= not >> rebuild

refreshWatchers :: (MonadIO m, MonadState AppState m) => m ()
refreshWatchers = do
  watchers <- use #watchers
  getDirPure <- getDirs
  traversing (#watchers % #all) killWatcher -- hinotify (or inotify itself) treats multiple WatchDescriptor-s  on the same directory as one
  traversing (#watchers % #all) (refreshWatch watchers getDirPure) -- so we have to remove all of them before recreating*

getDirs :: (MonadIO m, MonadState AppState m) => m (WhichDir -> Maybe (Path Abs))
getDirs = do
  path <- use $ #dir % #path
  childDirPath <- preuse $ #dir % child % savedDir % #path
  pure $ \case
    Parent -> takeParent path
    Current -> Just path
    Child -> childDirPath

refreshWatch :: MonadIO m => INotifyState -> (WhichDir -> Maybe (Path Abs)) -> DirWatcher -> m DirWatcher
refreshWatch watchers getDir dw =
  getDir dw.dir & \case
    Nothing -> killWatcher dw
    Just dir' ->
      watchDir
        dw
        watchers.inotify
        dir'
        watchers.channel

handleVtyEvent :: Config -> Event -> AppM ()
handleVtyEvent cfg = \case
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
  files = (\opt -> #dir % opt % #files) $ case dir of
    Current -> idTrav
    Parent -> #parent % _Just
    Child -> child % savedDir

  actOnFile :: (SortFunction -> FileInfo -> FileSeq -> FileSeq) -> RawFilePath -> Path Abs -> AppM ()
  actOnFile f path parent = do
    fileInfo <- getFileInfo $ parent </> takeFileName (fromRaw path)
    sortf <- use #sortFunction
    files %= f sortf fileInfo

  withParent :: (Path Abs -> AppM ()) -> AppM ()
  withParent action = do
    getDirs ?? dir >>= \case
      Just parent -> action parent
      Nothing ->
        files %= listClear

  evUpdate = withParent <. actOnFile LU.update
  evCreate = withParent <. actOnFile LU.insert
  evDelete name = withParent $ const $ files %= LU.delete (takeFileName $ fromRaw name)

handleEventWith :: Config -> BrickEvent n LcnsEvent -> AppM ()
handleEventWith cfg event = case event of
  VtyEvent vtye -> handleVtyEvent cfg vtye
  AppEvent appEv -> handleAppEvent appEv
  MouseDown{} -> pass
  MouseUp{} -> pass

openFile :: FileInfo -> AppM ()
openFile file
  | isDir file = goRight
openFile nonDir =
  executeFile (fromRaw "xdg-open") True name Nothing
 where
  name = nonDir ^? #name <&> (\(Path p) -> p) & maybeToList

rebuild :: AppM ()
rebuild = do
  traversing #dir (refreshSavedDir' True)
  traversing (#dir % #parent % _Just) (refreshSavedDir' True)
  refreshSelected' True