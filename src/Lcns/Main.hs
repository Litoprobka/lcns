{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Lcns.Main (lcns) where

import           Relude

import           Brick                     hiding (Down, on)
import           Brick.Widgets.List        (list, listMoveBy,
                                            listSelectedElement, renderList)
import           Data.ByteString           (toFilePath)
import qualified Data.HashMap.Strict       as Map
import qualified Data.Sequence             as Seq
import           Graphics.Vty.Attributes
import           Graphics.Vty.Input.Events
import           Numeric                   (showFFloat)
import           System.Directory          hiding (isSymbolicLink)
import           System.FilePath           (makeRelative)
import qualified System.INotify            as IN (Event (..), initINotify,
                                                  killINotify)
import           System.Posix              (executeFile, fileSize)
import           System.Posix.ByteString   (COff (COff))

import           Brick.BChan               (BChan, newBChan)
import           Lcns.Config
import           Lcns.FileInfo
import           Lcns.FileTracker
import qualified Lcns.ListUtils            as LU
import           Lcns.Sort
import           Lcns.Types
import           Lcns.Utils

lcns :: Config -> IO ()
lcns _ = do
    channel <- newBChan 8 -- in theory, 3 should work
    cleanup =<< mainWithFileTracker channel app =<< buildInitialState channel

buildInitialState :: BChan LcnsEvent -> IO AppState
buildInitialState channel = do
    inotify <- IN.initINotify
    refreshState AppState
            { currentFiles = list "empty" Seq.empty 0
            , currentDir = ""
            , sortFunction = SF { reversed = False, func = Nothing }
            , showDotfiles = True
            , inotify
            , dirWatcher = Nothing
            , parentWatcher = Nothing
            , childWatcher = Nothing
            , channel
            , selectionCache = Map.empty
            }

refreshState :: AppState -> IO AppState
refreshState appState = do
    currentDir <- getCurrentDirectory
    dirWatcher <- io $ Just -- mess
        <$> watchDir appState.dirWatcher
                     appState.inotify
                     currentDir
                     appState.channel

    dirFiles <- mapM getFileInfo =<< listDirectory currentDir
    let currentFiles = list "dir" (Seq.fromList $ filterHidden $ sortDir dirFiles) 1
    pure $ appState {currentFiles, currentDir, dirWatcher }
    where
        sortDir = sortBy $ cmpWith appState.sortFunction

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
        withAttr (attrName $ if isSelected
            then "selected"
            else fileAttr file.typedInfo)
        $ line file

    fileAttr fileType = case fileType of
        Link Nothing           -> "invalid-link"
        Link (Just (Dir _))    -> "directory-link"
        Link (Just (File _))   -> "link"
        Link (Just nestedLink) -> fileAttr nestedLink
        Dir _                  -> "directory"
        File _                 -> "file"

    line file = padLeftRight 1 $ str file.name <+> fillLine <+> str (size file)
    size :: FileInfo -> String
    size file@FileInfo{ typedInfo = File _ } =
        let (COff n) = fileSize file.status in if -- this is ugly
        | n < 2 ^! 10 -> show n <> " B"
        | n < 2 ^! 20 -> n `div'` (2 ^! 10) $ " K"
        | n < 2 ^! 30 -> n `div'` (2 ^! 20) $ " M"
        | otherwise   -> n `div'` (2 ^! 30) $ " G"

    size FileInfo{ typedInfo = Dir di } = show di.itemCount
    size file@FileInfo{ typedInfo = Link (Just l) } = size file{ typedInfo = l }
    size FileInfo{ typedInfo = Link Nothing } = "N/A"

    infixl 8 ^!
    (^!) :: Num a => a -> Int -> a
    (^!) = (^)

    div' :: Int64 -> Double -> String -> String
    div' x y = showFFloat (Just 2) (fromIntegral x / y)

    topPanel = str s.currentDir <+> fillLine
    bottomPanel = fillLine -- placeholder
    fillLine = vLimit 1 $ fill ' '

app :: App AppState LcnsEvent ResourceName
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
        , ("directory-link", currentAttr `withForeColor` cyan `withStyle` bold)
        , ("invalid-link", currentAttr `withForeColor` blue)
        ]

-- * Event handling * --

handleVtyEvent :: Event -> EventM n AppState ()
handleVtyEvent event = case event of
    EvKey (KChar 'q') [MCtrl] -> halt
    EvKey KUp [] -> updFiles $ listMoveBy (-1)
    EvKey KDown [] -> updFiles $ listMoveBy  1
    EvKey KRight [] -> do
        onJust openFile =<< selected
    EvKey KLeft [] -> openFile ".."
    EvKey KDel [] -> selected >>= onJust (io <. removeFile) >> refresh
    EvKey (KChar 'r') [MCtrl] -> updAndRefresh $ \s -> s{sortFunction = invertSort s.sortFunction} -- perhaps dropping Lens wasn't a good idea
    EvKey (KChar 'd') [MCtrl] -> updAndRefresh $ \s -> s{showDotfiles = not s.showDotfiles}
    _ -> pass

cleanup :: AppState -> IO ()
cleanup st =
    IN.killINotify st.inotify

selected :: EventM n AppState (Maybe FilePath)
selected =
    gets (.currentFiles)
    <&> listSelectedElement
    <<&>> snd
    <<&>> (.name)

updAndRefresh :: (AppState -> AppState) -> EventM n AppState ()
updAndRefresh f = do
    s <- get
    put =<< io (refreshState $ f s)

refresh :: EventM n AppState ()
refresh = updAndRefresh id

updFiles :: (FileSeq -> FileSeq) -> EventM n AppState ()
updFiles f = do
    modify (\s -> s{currentFiles = f s.currentFiles})

openFile :: FilePath -> EventM n AppState ()
openFile file = do
    ifM (io $ doesDirectoryExist file)
        (do
            curDir  <- io getCurrentDirectory
            curFile <- selected
            absFile <- makeAbs' file

            modifySelection curDir (const curFile)
            io $ setCurrentDirectory file
            refresh

            when (file == "..")
                (modifySelection absFile $ Just <.
                    fromMaybe (makeRelative absFile curDir))

            gets ((.selectionCache) .> Map.lookup absFile)
                >>= onJust (LU.select .> updFiles)
            )
        (io $ executeFile "xdg-open" True [file] Nothing)
    where
        modifySelection dir f = modify $ \st ->
            st{selectionCache = Map.alter f dir st.selectionCache}

        -- used to call canonicalizePath only when necessary
        makeAbs' :: FilePath -> EventM n AppState FilePath
        makeAbs' ".." = io $ canonicalizePath ".."
        makeAbs' path = io $ makeAbsolute path

handleAppEvent :: LcnsEvent -> EventM n AppState ()
handleAppEvent (DirEvent event) = case event of
    IN.Attributes _ Nothing     -> pass
    IN.Attributes _ (Just path) -> evUpdate path
    IN.Created  _ path          -> evCreate path
    IN.MovedIn  _ path _        -> evCreate path
    IN.Deleted  _ path          -> evDelete path
    IN.MovedOut _ path _        -> evDelete path
    _                           -> pass -- for Ignored and the like
    where
        act f path = do
            fi <- io $ getFileInfo =<< toFilePath path
            sortf <- gets (.sortFunction)
            updFiles $ f sortf fi
        evUpdate = act LU.update
        evCreate = act LU.insert
        evDelete rawPath = do
            path <- io $ toFilePath rawPath
            updFiles $ LU.delete path

handleEvent :: BrickEvent n LcnsEvent -> EventM n AppState ()
handleEvent event = case event of
    VtyEvent vtye  -> handleVtyEvent vtye
    AppEvent appEv -> handleAppEvent appEv
    MouseDown {}   -> pass
    MouseUp {}     -> pass



