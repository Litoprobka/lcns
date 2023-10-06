{-# LANGUAGE LambdaCase #-}

module Lcns.EventHandling (refreshState, handleVtyEvent, handleAppEvent, handleEvent) where

import Lcns.FileInfo
import Lcns.FileTracker
import Lcns.ListUtils qualified as LU
import Lcns.Prelude
import Lcns.Sort

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

handleVtyEvent :: Event -> EventM n AppState ()
handleVtyEvent event = case event of
  EvKey (KChar 'q') [MCtrl] -> halt
  EvKey KUp [] -> updFiles listMoveUp >> updateChildDir -- temporary solution, should be replaced by incremental updates
  EvKey KDown [] -> updFiles listMoveDown >> updateChildDir
  EvKey KRight [] -> do
    onJust openFile =<< selected
  EvKey KLeft [] -> openFile $ takeFileName $ fromRaw ".."
  EvKey KDel [] -> selected >>= onJust (io <. removeFile) >> updAndRefresh id
  EvKey (KChar 'r') [MCtrl] -> updAndRefresh $ #sortFunction %~ invertSort
  EvKey (KChar 'd') [MCtrl] -> updAndRefresh $ #showDotfiles %~ not
  _ -> pass

handleAppEvent :: LcnsEvent -> EventM n AppState ()
handleAppEvent (DirEvent dir event) = case event of
  IN.Attributes _ Nothing -> pass
  IN.Attributes _ (Just path) -> evUpdate path
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

  actOnFile f path parent = do
    fileInfo <- io $ getFileInfo $ parent </> takeFileName (fromRaw path)
    sortf <- use #sortFunction
    files %= f sortf fileInfo

  withParent action =
    get >>= (`getDir` dir) >>= \case
      Just parent -> action parent
      Nothing ->
        files %= listClear

  evUpdate = withParent <. actOnFile LU.update
  evCreate = withParent <. actOnFile LU.insert
  evDelete name = withParent $ const $ files %= LU.delete (takeFileName $ fromRaw name)

handleEvent :: BrickEvent n LcnsEvent -> EventM n AppState ()
handleEvent event = case event of
  VtyEvent vtye -> handleVtyEvent vtye
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

selected :: EventM n AppState (Maybe (Path Rel))
selected = preuse selection

updAndRefresh :: (AppState -> AppState) -> EventM n AppState ()
updAndRefresh f = do
  s <- get
  (#selections % at s.dir .=) =<< selected
  put =<< refreshState s.dir (f s)
  #files % #list %= LU.selectMaybe (s ^? selection) -- temporary

updateChildDir :: EventM n AppState ()
updateChildDir = updAndRefresh id

updFiles :: (FileSeq -> FileSeq) -> EventM n AppState ()
updFiles f = #files % #list %= f

openFile :: Path Rel -> EventM n AppState ()
openFile file@(Path name) =
  do
    curDir <- use #dir
    -- if `curDir` is "/", `parentDir` will be gibberish
    -- it is not a problem if we modify #selections in this order (the incorrect path gets overriden), but it's kinda whacky
    let parentDir = takeDirectory curDir
    curDir `combineWithDots` file
      & onJust (openFileUnnested curDir parentDir)
 where
  openFileUnnested dir parent absFile =
    ifM
      (doesDirectoryExist absFile)
      ( do
          curFile <- selected
          #selections % at parent ?= takeFileName dir
          #selections % at dir .= curFile
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
    >>= traverseOf (#watchers % #all) killWatcher
    >>= traverseOf (#watchers % #all) (refreshWatch withNewDir)
 where
  withNewDir =
    appState
      & #dir .~ dir
      & #selections % at appState.dir .~ appState ^? selection

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
  sortDir = sortBy $ cmpWith appState.sortFunction

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