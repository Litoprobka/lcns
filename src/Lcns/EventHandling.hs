{-# LANGUAGE LambdaCase #-}

module Lcns.EventHandling (
  refreshState,
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
) where

import Lcns.FileInfo
import Lcns.FileTracker
import Lcns.ListUtils qualified as LU
import Lcns.Prelude
import Lcns.Sort qualified as S

import Brick
import Brick.Widgets.List (
  list,
  listClear,
  listMoveDown,
  listMoveUp,
  listSelectedElement,
 )
import Data.Sequence qualified as Seq
import Graphics.Vty.Input.Events
import Lcns.Path
import System.INotify qualified as IN (Event (..))
import System.Posix.ByteString (RawFilePath)

handleVtyEvent :: Config -> Event -> AppM ()
handleVtyEvent cfg = \case
  EvKey key mods -> cfg.keybindings key mods
  _ -> pass

moveUp :: AppM ()
moveUp = updFiles listMoveUp >> updateChildDir

moveDown :: AppM ()
moveDown = updFiles listMoveDown >> updateChildDir

open :: AppM ()
open = onJust openFile =<< selected

moveBack :: AppM ()
moveBack = openFile $ takeFileName $ fromRaw ".."

delete :: AppM ()
delete = selected >>= onJust (io <. removeFile) >> updAndRefresh id

invertSort :: AppM ()
invertSort = selected >>= onJust (io <. removeFile) >> updAndRefresh id

toggleDotfiles :: AppM ()
toggleDotfiles = updAndRefresh $ #showDotfiles %~ not

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
  files :: Lens' AppState FileSeq
  files = (% #list) $ case dir of
    Current -> #files
    Parent -> #parentFiles
    Child -> #childFiles

  actOnFile :: (SortFunction -> FileInfo -> FileSeq -> FileSeq) -> RawFilePath -> Path Abs -> AppM ()
  actOnFile f path parent = do
    fileInfo <- io $ getFileInfo $ parent </> takeFileName (fromRaw path)
    sortf <- use #sortFunction
    files %= f sortf fileInfo

  withParent :: (Path Abs -> AppM ()) -> AppM ()
  withParent action =
    get >>= (`getDir` dir) >>= \case
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

selection :: AffineFold AppState (Path Rel)
selection =
  #files
    % #list
    % to listSelectedElement
    % _Just
    % _2
    % #name

selected :: AppM (Maybe (Path Rel))
selected = preuse selection

addListFocusToSelections :: AppState -> AppState
addListFocusToSelections st =
  st & #selections % at st.dir .~ st ^? selection

updAndRefresh :: (AppState -> AppState) -> AppM ()
updAndRefresh f = do
  modify addListFocusToSelections
  s <- get
  put =<< refreshState s.dir (f s)
  #files % #list %= LU.selectMaybe (s ^? selection) -- temporary

updateChildDir :: AppM ()
updateChildDir = do
  modify addListFocusToSelections
  s <- get
  s
    & traverseOf (#watchers % #childWatcher) (refreshWatch s)
    >>= traverseOf #childFiles (refreshFiles s)
    >>= put

updFiles :: (FileSeq -> FileSeq) -> AppM ()
updFiles f = #files % #list %= f

openFile :: Path Rel -> AppM ()
openFile file@(Path name) =
  do
    curDir <- use #dir
    curDir `combineWithDots` file
      & onJust (openFileUnnested curDir)
 where
  openFileUnnested dir absFile =
    ifM
      (doesDirectoryExist absFile)
      ( do
          -- if `dir` is "/", `takeDirectory dir` will be gibberish
          -- it is not a problem if we modify #selections in this order (the incorrect path gets overriden), but it's kinda whacky
          #selections % at (takeDirectory dir) ?= takeFileName dir
          modify addListFocusToSelections
          put =<< refreshState absFile =<< get

          preuse (#selections % ix absFile)
          >>= onJust (LU.select .> modifying (#files % #list))
      )
      (executeFile (fromRaw "xdg-open") True [name] Nothing)

refreshState :: MonadIO m => Path Abs -> AppState -> m AppState
refreshState dir appState =
  do
    withNewDir
    & traverseOf #allFiles (refreshFiles withNewDir)
    >>= traverseOf (#watchers % #all) killWatcher -- hinotify (or inotify itself) treats multiple WatchDescriptor-s  on the same directory as one
    >>= traverseOf (#watchers % #all) (refreshWatch withNewDir) -- so we have to remove all of them before recreating
 where
  withNewDir =
    appState
      & addListFocusToSelections
      & #dir .~ dir

refreshFiles :: MonadIO m => AppState -> DirFiles -> m DirFiles
refreshFiles appState files = do
  getDir appState files.dir >>= \case
    Nothing -> pure $ files & #list %~ listClear
    Just dir -> do
      dirFiles <- mapM (io <. getFileInfo) =<< tryListDirectory dir
      pure $
        files
          & #list .~ list files.name (Seq.fromList $ filterHidden $ sortDir dirFiles) 1
          & #list %~ LU.selectMaybe (appState ^? #selections % ix dir)
 where
  sortDir = sortBy $ S.cmpWith appState.sortFunction

  filterHidden
    | appState.showDotfiles = id
    | otherwise = filter (not . isDotfile)

  isDotfile :: FileInfo -> Bool
  isDotfile f = f.name ^? to fromRel % _head == Just (toEnum $ fromEnum '.')

childDir :: MonadIO m => AppState -> m (Maybe (Path Abs))
childDir appState =
  traverse doesDirectoryExist childDirPath <&> \case
    Just True -> childDirPath
    _ -> Nothing
 where
  childDirPath =
    (appState ^? #selections % ix appState.dir)
      <|> (appState ^? selection)
      <&> (appState.dir </>)

getDir :: MonadIO m => AppState -> WhichDir -> m (Maybe (Path Abs))
getDir appState = \case
  Parent -> pure $ takeParent appState.dir
  Current -> pure $ Just appState.dir
  Child -> childDir appState

refreshWatch :: MonadIO m => AppState -> DirWatcher -> m DirWatcher
refreshWatch appState dw =
  getDir appState dw.dir >>= \case
    Nothing -> killWatcher dw
    Just dir' ->
      watchDir
        dw
        appState.watchers.inotify
        dir'
        appState.watchers.channel