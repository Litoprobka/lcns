{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Lcns.Main (lcns) where

import           Relude

import           Brick                     hiding (Down, on)
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
import System.Posix.ByteString (COff(COff))
import System.Posix (fileSize)


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

preview :: Widget ResourceName
preview = padAll 1 $ txt "preview" <=> txt "placeholder"

parentDir :: Widget ResourceName
parentDir = padAll 1 $ txt "parent dir" <=> txt "placeholder"

drawTUI :: AppState -> [Widget ResourceName]
drawTUI s = one $ topPanel <=> hBox [parentDir, renderList renderFile True s.currentFiles, preview] <=> bottomPanel where

    renderFile isSelected file =
        withAttr (attrName $ if
            | isSelected                   -> "selected"
            | file.isDirLink               -> "dirlink"
            | file.status & isSymbolicLink -> "link"
            | file.status & isDirectory    -> "directory"
            | otherwise                    -> "file")
        $ line file

    line file = padLeftRight 1 $ str file.name <+> fillLine <+> size file
    size file = -- TODO: handle directorie
        let (COff n) = fileSize file.status in str $ if -- this is ugly
        | n < 1_000 -> show n <> " B"
        | n < 1_000_000 -> n `div'` 1_000 <> " K"
        | n < 1_000_000 -> n `div'` 1_000_000 <> " M"
        | otherwise -> n `div'` 1_000_000_000_000 <> " G"
    
    div' :: Int64 -> Double -> String
    div' x y = show $ fromIntegral x / y

    topPanel = str s.currentDir <+> fillLine
    bottomPanel = fillLine -- placeholder
    fillLine = vLimit 1 $ fill ' '

app :: App AppState e ResourceName
app = App
    { appDraw = drawTUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pass
    , appAttrMap = mkAttrMap
    }

mkAttrMap :: AppState -> AttrMap
mkAttrMap = const $ attrMap (fg white) $ first attrName <$>
        [ ("selected", currentAttr `withBackColor` brightBlue `withForeColor` black `withStyle` bold)
        , ("directory", currentAttr `withStyle` bold `withForeColor` brightBlue)
        , ("link", currentAttr `withForeColor` cyan)
        , ("dirlink", currentAttr `withForeColor` cyan `withStyle` bold)
        ]

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
            Nothing                    -> error "impossible"

        updState :: (AppState -> AppState) -> EventM n AppState ()
        updState f = do
            s <- get
            ns <- liftIO $ refreshState $ f s
            put ns
        updState' = updState id
        changeDir dir = do
            liftIO $ whenM (doesDirectoryExist dir) $ setCurrentDirectory dir
            updState'


