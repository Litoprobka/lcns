{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Lcns.Main (lcns) where

import           Relude

import           Brick                     hiding (Down, on)
import           Brick.Widgets.Border      (border)
import           Brick.Widgets.List        (list, listMoveBy,
                                            listSelectedElement, renderList)
import           Data.Default              (def)
import qualified Data.Sequence             as Seq
import           Graphics.Vty.Attributes
import           Graphics.Vty.Input.Events
import           System.Directory          hiding (isSymbolicLink)
import           System.Posix              (isDirectory, isSymbolicLink)

import           Lcns.Config
import           Lcns.Types


lcns :: Config -> IO ()
lcns _ = do
    _ <- defaultMain app =<< buildInitialState
    pass

buildInitialState :: IO AppState
buildInitialState = refreshState def

refreshState :: AppState -> IO AppState
refreshState appState = do
    curDir <- getCurrentDirectory
    dirFiles <- mapM mkFileInfo =<< listDirectory curDir
    let dirContents = list "dir" (Seq.fromList $ filterHidden $ sortDir dirFiles) 1
    pure $ appState {currentFiles = dirContents, currentDir = curDir}
    where
        sortDir = sortBy $ case appState.sortFunction of
          Def              -> compare `on` tuple
          Reversed         -> compare `on` Down . tuple
          Custom f         -> f
          CustomReversed f -> flip f
        tuple file = (Down (file.isDirLink || (file.status & isDirectory)), file.name)

        filterHidden | appState.showDotfiles = id
                     | otherwise = filter (not . isDotfile)
        isDotfile f = Just '.' == listToMaybe f.name

drawTUI :: AppState -> [Widget ResourceName]
drawTUI s = one $ border $ renderList renderFile True $ s.currentFiles where

    renderFile isSelected file =
        withAttr (attrName $ if
            | isSelected                   -> "selected"
            | file.isDirLink               -> "dirlink"
            | file.status & isSymbolicLink -> "link"
            | file.status & isDirectory    -> "directory"
            | otherwise                    -> "file")
        $ str $ file.name

app :: App AppState e ResourceName
app = App
    { appDraw = drawTUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pass
    , appAttrMap = const $ attrMap currentAttr
        [ (attrName "selected", currentAttr `withBackColor` brightBlue `withForeColor` black `withStyle` bold)
        , (attrName "directory", currentAttr `withStyle` bold `withForeColor` brightBlue)
        , (attrName "link", currentAttr `withForeColor` cyan)
        , (attrName "dirlink", currentAttr `withForeColor` cyan `withStyle` bold)
        ]
    }

handleEvent :: BrickEvent n e -> EventM n AppState ()
handleEvent event = case event of
    VtyEvent vtye -> case vtye of
        EvKey (KChar 'q') [MCtrl] -> halt
        EvKey KUp [] -> modify $ \s -> s{currentFiles = s.currentFiles & listMoveBy (-1)}
        EvKey KDown [] -> modify $ \s -> s{currentFiles = s.currentFiles & listMoveBy 1}
        EvKey KRight [] -> do
            changeDir =<< selected
        EvKey KLeft [] -> changeDir ".."
        EvKey KDel [] -> selected >>= liftIO . removeFile >> updState'
        EvKey (KChar 'r') [MCtrl] -> updState $ \s -> s{sortFunction = invertSort s.sortFunction} -- perhaps dropping Lens wasn't a good idea
        EvKey (KChar 'd') [MCtrl] -> updState $ \s -> s{showDotfiles = not s.showDotfiles}
        _ -> pass
    _ -> pass
    where
        selected = get <&> (.currentFiles) <&> listSelectedElement <&> \case
            Just (_, file :: FileInfo) -> file.name
            Nothing        -> error "impossible"

        updState :: (AppState -> AppState) -> EventM n AppState ()
        updState f = do
            s <- get
            ns <- liftIO $ refreshState $ f s
            put ns
        updState' = updState id
        changeDir dir = do
            liftIO $ whenM (doesDirectoryExist dir) $ setCurrentDirectory dir
            updState'


