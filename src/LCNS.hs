{-# LANGUAGE MultiWayIf #-}

module LCNS (tui) where

import Relude

import Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.List 
--import Brick.Widgets.List (list, renderList, listMoveBy, listMoveTo)

import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes

import System.Directory hiding (isSymbolicLink)
import System.FilePath (takeDirectory)
import System.Posix (isSymbolicLink, isDirectory)

import qualified Data.Sequence as Seq
import UITypes
import Lens.Micro ((%~), (^.))
import Lens.Micro.Extras (view)

tui :: IO ()
tui = do
    initialState <- buildInitialState
    _ <- defaultMain lcns initialState
    pass

buildInitialState :: IO AppState
buildInitialState = do
    curDir <- getCurrentDirectory
    dirFiles <- mapM mkFileInfo =<< listDirectory curDir
    let dirContents = list "dir" (Seq.fromList $ sortOn (not . isDirectory . view status) dirFiles) 1
    pure $ AppState {_currentFiles = dirContents, _currentDir = curDir}

drawTUI :: AppState -> [Widget ResourceName]
drawTUI s = one $ border $ renderList renderFile True $ s ^. currentFiles where

    renderFile isSelected file =
        withAttr (if
            | isSelected -> "selected"
            | file ^. status & isSymbolicLink -> "link" -- note: getFileStatus does not detect symlinks
            | file ^. status & isDirectory -> "directory"
            | otherwise -> "file") 
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
        EvKey KDel [] -> liftIO (removeFile selected) >> updState
        _ -> continue s
    _ -> continue s
    where
        selected = case listSelectedElement $ s ^. currentFiles of
            Just (_, file) -> file ^. name
            Nothing -> error "impossible"

        changeState f = continue $ f s
        updState = liftIO buildInitialState >>= continue
        changeDir dir = do
            liftIO $ whenM (doesDirectoryExist dir) $ setCurrentDirectory dir
            updState
            

