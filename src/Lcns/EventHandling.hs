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
import Lcns.FilePathExtras (makeAbsolute)
import RawFilePath.Directory
import System.FilePath.ByteString
import System.INotify qualified as IN (Event (..))
import System.Posix.ByteString (
  changeWorkingDirectory,
  executeFile,
  getWorkingDirectory,
 )

handleVtyEvent :: Event -> EventM n AppState ()
handleVtyEvent event = case event of
  EvKey (KChar 'q') [MCtrl] -> halt
  EvKey KUp [] -> updFiles $ listMoveBy (-1)
  EvKey KDown [] -> updFiles $ listMoveBy 1
  EvKey KRight [] -> do
    onJust openFile =<< selected
  EvKey KLeft [] -> openFile ".."
  EvKey KDel [] -> selected >>= onJust (io <. removeFile) >> refresh
  EvKey (KChar 'r') [MCtrl] -> updAndRefresh $ #sortFunction %~ invertSort
  EvKey (KChar 'd') [MCtrl] -> updAndRefresh $ #showDotfiles %~ not
  _ -> pass

handleAppEvent :: LcnsEvent -> EventM n AppState ()
handleAppEvent (DirEvent event) = case event of
  IN.Attributes _ Nothing -> pass
  IN.Attributes _ (Just path) -> evUpdate path
  IN.Created _ path -> evCreate path
  IN.MovedIn _ path _ -> evCreate path
  IN.Deleted _ path -> evDelete path
  IN.MovedOut _ path _ -> evDelete path
  _ -> pass -- for Ignored and the like
 where
  act f path = do
    fi <- io $ getFileInfo path
    sortf <- gets (.sortFunction)
    #files %= f sortf fi
  evUpdate = act LU.update
  evCreate = act LU.insert
  evDelete = updFiles . LU.delete

handleEvent :: BrickEvent n LcnsEvent -> EventM n AppState ()
handleEvent event = case event of
  VtyEvent vtye -> handleVtyEvent vtye
  AppEvent appEv -> handleAppEvent appEv
  MouseDown{} -> pass
  MouseUp{} -> pass

selected :: EventM n AppState (Maybe RawFilePath)
selected =
  preuse $
    #files
      % to listSelectedElement
      % _Just
      % _2
      % #name

updAndRefresh :: (AppState -> AppState) -> EventM n AppState ()
updAndRefresh f = do
  s <- get
  put =<< io (refreshState $ f s)

refresh :: EventM n AppState ()
refresh = updAndRefresh id

updFiles :: (FileSeq -> FileSeq) -> EventM n AppState ()
updFiles f = #files %= f

openFile :: RawFilePath -> EventM n AppState ()
openFile file = do
  ifM
    (io $ doesDirectoryExist file)
    ( do
        curDir <- io getWorkingDirectory
        curFile <- selected
        absFile <- io $ makeAbsolute file

        #selections % at curDir .= curFile
        io $ changeWorkingDirectory file
        refresh

        when
          (file == "..")
          ( #selections
              % at absFile
              %= Just
                <. fromMaybe (makeRelative absFile curDir)
          )

        preuse (#selections % ix absFile)
          >>= onJust (LU.select .> modifying #files)
    )
    (io $ executeFile "xdg-open" True [file] Nothing)

refreshState :: AppState -> IO AppState
refreshState appState =
  do
    dir <- getWorkingDirectory

    dirWatcher <-
      Just -- mess
        <$> watchDir
          appState.watchers.dirWatcher
          appState.watchers.inotify
          dir
          appState.watchers.channel

    dirFiles <- mapM getFileInfo =<< listDirectory dir
    let files = list "dir" (Seq.fromList $ filterHidden $ sortDir dirFiles) 1

    pure $
      appState{dir, files}
        & #watchers % #dirWatcher .~ dirWatcher
 where
  sortDir = sortBy $ cmpWith appState.sortFunction

  filterHidden
    | appState.showDotfiles = id
    | otherwise = filter (not . isDotfile)
  isDotfile :: FileInfo -> Bool
  isDotfile f = f.name ^? _head == Just (toEnum $ fromEnum '.')