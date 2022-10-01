{-# LANGUAGE MultiWayIf #-}

module LCNS (tui) where

import           Relude

import           Brick                     hiding (Down, on)
import           Brick.Widgets.Border      (border)
import           Brick.Widgets.List        (list, listMoveBy,
                                            listSelectedElement, renderList)
import           Data.Default              (def)
import qualified Data.Sequence             as Seq
import           Graphics.Vty.Attributes
import           Graphics.Vty.Input.Events
import           Lens.Micro                ((%~), (^.))
import           System.Directory          hiding (isSymbolicLink)
import           System.FilePath           (takeDirectory)
import           System.Posix              (isDirectory, isSymbolicLink)

import           Types

tui :: IO ()
tui = do
    initialState <- buildInitialState
    _ <- defaultMain lcns initialState
    pass

buildInitialState :: IO AppState
buildInitialState = refreshState def

refreshState :: AppState -> IO AppState
refreshState appState = do
    curDir <- getCurrentDirectory
    dirFiles <- mapM mkFileInfo =<< listDirectory curDir
    let dirContents = list "dir" (Seq.fromList $ sortBy sortDir dirFiles) 1
    pure $ appState {_currentFiles = dirContents, _currentDir = curDir}
    where
        sortDir = case appState ^. sortFunction of
          Def              -> compare `on` tuple
          Reversed         -> compare `on` Down . tuple
          Custom f         -> f
          CustomReversed f -> flip f
        tuple file = (Down (file ^. isDirLink || (file ^. status & isDirectory)), file ^. name)

drawTUI :: AppState -> [Widget ResourceName]
drawTUI s = one $ border $ renderList renderFile True $ s ^. currentFiles where

    renderFile isSelected file =
        withAttr (if
            | isSelected                      -> "selected"
            | file ^. isDirLink               -> "dirlink"
            | file ^. status & isSymbolicLink -> "link"
            | file ^. status & isDirectory    -> "directory"
            | otherwise                       -> "file")
        $ str $ file ^. name

lcns :: App AppState e ResourceName
lcns = App
    { appDraw = drawTUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty
        [ ("selected", currentAttr `withBackColor` brightBlue `withForeColor` black `withStyle` bold)
        , ("directory", currentAttr `withStyle` bold `withForeColor` brightBlue)
        , ("link", currentAttr `withForeColor` cyan)
        , ("dirlink", currentAttr `withForeColor` cyan `withStyle` bold)
        ]
    }

handleEvent :: AppState -> BrickEvent n e -> EventM n (Next AppState)
handleEvent s event = case event of
    VtyEvent vtye -> case vtye of
        EvKey (KChar 'q') [MCtrl] -> halt s
        EvKey KUp [] -> changeState $ currentFiles %~ listMoveBy (-1)
        EvKey KDown [] -> changeState $ currentFiles %~ listMoveBy 1
        EvKey KRight [] -> do
            changeDir selected
        EvKey KLeft [] -> changeDir $ takeDirectory $ s ^. currentDir
        EvKey KDel [] -> liftIO (removeFile selected) >> updState'
        EvKey (KChar 'r') [MCtrl] -> updState (sortFunction %~ invertSort)
        _ -> continue s
    _ -> continue s
    where
        selected = case listSelectedElement $ s ^. currentFiles of
            Just (_, file) -> file ^. name
            Nothing        -> error "impossible"

        changeState f = continue $ f s
        updState f = liftIO (refreshState $ f s) >>= continue
        updState' = updState id
        changeDir dir = do
            liftIO $ whenM (doesDirectoryExist dir) $ setCurrentDirectory dir
            updState'


