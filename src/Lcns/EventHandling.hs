module Lcns.EventHandling (refreshState, handleVtyEvent, handleAppEvent, handleEvent) where

import Lcns.FileInfo
import Lcns.FileTracker
import Lcns.ListUtils qualified as LU
import Lcns.Prelude
import Lcns.Sort

import Brick
import Brick.Widgets.List (
  list,
  listMoveBy,
  listSelectedElement,
 )
import Data.Sequence qualified as Seq
import Graphics.Vty.Input.Events
import Lcns.Path
import System.INotify qualified as IN (Event (..))

handleVtyEvent :: Event -> EventM n AppState ()
handleVtyEvent event = case event of
  EvKey (KChar 'q') [MCtrl] -> halt
  EvKey KUp [] -> updFiles $ listMoveBy (-1)
  EvKey KDown [] -> updFiles $ listMoveBy 1
  EvKey KRight [] -> do
    onJust openFile =<< selected
  EvKey KLeft [] -> openFile $ fromRaw ".."
  EvKey KDel [] -> selected >>= onJust (io <. removeFile) >> refresh
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
  files = case dir of
    Current -> #files
    Parent -> #parentFiles
    Child -> #childFiles

  getParent = case dir of
    Current -> pure ""
    Parent -> io getParentDir
    Child -> selected <&> fromMaybe ""

  act f path = do
    parent <- getParent
    fi <- io $ getFileInfo $ parent </> $ fromRaw path
    sortf <- gets (.sortFunction)
    files %= f sortf fi

  evUpdate = act LU.update
  evCreate = act LU.insert
  evDelete = updFiles <. LU.delete <. fromRaw

handleEvent :: BrickEvent n LcnsEvent -> EventM n AppState ()
handleEvent event = case event of
  VtyEvent vtye -> handleVtyEvent vtye
  AppEvent appEv -> handleAppEvent appEv
  MouseDown{} -> pass
  MouseUp{} -> pass

selection :: AffineFold AppState RawFilePath
selection =
  #files
    % to listSelectedElement
    % _Just
    % _2
    % #name

selected :: EventM n AppState (Maybe RawFilePath)
selected = preuse selection

updAndRefresh :: (AppState -> AppState) -> EventM n AppState ()
updAndRefresh f = do
  s <- get
  put =<< io (refreshState $ f s)
  s ^? selection & onJust (modifying #files <. LU.select)
  #parentFiles %= LU.select (takeFileName s.dir) -- we know that dir doesn't contain a trailing /

refresh :: EventM n AppState ()
refresh = updAndRefresh id

updFiles :: (FileSeq -> FileSeq) -> EventM n AppState ()
updFiles f = #files %= f

openFile :: Path Rel -> EventM n AppState ()
openFile file@(Path name) = do
  ifM
    (doesDirectoryExist file)
    ( do
        curDir <- getCurrentDirectory
        parentDir <- io getParentDir
        curFile <- selected
        absFile <- makeAbsolute file

        #selections % at curDir .= curFile
        #selections % at parentDir ?= takeFileName curDir -- curDir doesn't contain a trailing slash
        setCurrentDirectory file
        refresh

        preuse (#selections % ix absFile)
        >>= onJust (LU.select .> modifying #files)
    )
    (executeFile (fromRaw "xdg-open") True [name] Nothing)

refreshState :: AppState -> IO AppState
refreshState appState =
  do
    dir <- getCurrentDirectory
    parent <- getParentDir

    curWatcher <- refreshWatch dir #dirWatcher
    parentWatcher <- refreshWatch parent #parentWatcher

    files <- refreshFiles "current files" dir
    parentFiles <- refreshFiles "parent files" parent

    pure $
      appState{dir, files, parentFiles}
        & #watchers % #dirWatcher .~ curWatcher
        & #watchers % #parentWatcher .~ parentWatcher
 where
  refreshWatch :: DemoteDir dir => RawFilePath -> Lens' INotifyState (DirWatcher dir) -> IO (DirWatcher dir)
  refreshWatch dir optic = do
    watchDir
      (appState ^. #watchers % optic)
      appState.watchers.inotify
      dir
      appState.watchers.channel

  refreshFiles :: Text -> RawFilePath -> IO FileSeq
  refreshFiles listName dir = do
    dirFiles <- mapM ((getFileInfo <. (dir </>)) <=< makeAbsolute) =<< listDirectory dir
    pure $ list listName (Seq.fromList $ filterHidden $ sortDir dirFiles) 1

  sortDir = sortBy $ cmpWith appState.sortFunction

  filterHidden
    | appState.showDotfiles = id
    | otherwise = filter (not . isDotfile)


  isDotfile :: FileInfo -> Bool
  isDotfile f = f.name ^? to fromRel % _head == Just (toEnum $ fromEnum '.')