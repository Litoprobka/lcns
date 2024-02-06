{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Lcns.Main (lcns) where

import Brick hiding (Down, on)
import Brick.BChan (newBChan)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.List (listElements, listNameL, renderList)
import Config.Dyre qualified as Dyre
import Graphics.Vty.Attributes
import Lcns.Config
import Lcns.DirHistory (buildDirHistory, childOrLink, curDir, parent, refreshSelected)
import Lcns.EventHandling
import Lcns.FileInfo (isDir, isLink, isRealDir, nameOf, savedDir, symlinked)
import Lcns.FileMap (empty, fileIn)
import Lcns.FileTracker
import Lcns.Path
import Lcns.Prelude hiding (preview)
import Numeric (showFFloat)
import System.INotify (withINotify)
import System.Posix.ByteString (COff (COff), fileSize)

lcns :: Config -> IO ()
lcns = Dyre.wrapMain $ Dyre.newParams "lcns" lcnsMain const

lcnsMain :: Config -> IO ()
lcnsMain config = do
  channel <- newBChan 8 -- in theory, 3 should work
  void $ withINotify \inotify -> do
    let env = AppEnv{..}
    buildInitialState env
      >>= mainWithFileTracker channel (app env)

buildInitialState :: AppEnv -> IO AppState
buildInitialState env = do
  let watchers =
        INotifyState
          { parentWatcher = emptyWatcher Parent
          , dirWatcher = emptyWatcher Current
          , childWatcher = emptyWatcher Child
          }
  let sortFunction = SF{reversed = False, func = Nothing, showDotfiles = True}
  path <- getCurrentDirectory
  (dir, (_, fileMap)) <- (sortFunction, Lcns.FileMap.empty) & runStateT (buildDirHistory path)

  AppState{..} & execStateT (refreshSelected >> refreshWatchers) & usingReaderT env
 where
  emptyWatcher dir = DirWatcher{dir, watcher = Nothing}

spacer :: Widget ResourceName
spacer = txt "\8203" -- it's a kind of magic...

drawTUI :: AppState -> [Widget ResourceName]
drawTUI s =
  one $
    topPanel
      <=> hBox
        [ hLimitPercent 25 $ renderDir "parent-" False do
            pId <- s ^? parent
            s ^? #fileMap % savedDir pId
        , spacer
        , hLimitPercent 40 $ renderDir "current-" True $ s ^? curDir
        , spacer
        , preview childFile
        ]
      <=> bottomPanel
 where
  childFile = s ^? curDir % childOrLink % fileIn s.fileMap
  topPanel =
    padLeft (Pad 5) $
      hBox
        [ withAttr (attrName "top-panel") $ txt $ withSlash $ decode $ s ^. curDir % #path
        , withAttr (attrName "top-panel" <> attrName "selected") $ txt $ maybe "" (decode <. nameOf) childFile
        ]
  withSlash "/" = "/"
  withSlash text = text <> "/"

  bottomPanel = fillLine -- placeholder

  -- note: since `preview` is only ever called from `drawTUI` via `% child`, we don't have to handle symlinks here at all
  preview :: Maybe FileInfo -> Widget ResourceName
  preview = maybe emptyWidget \case
    SavedDir dir -> renderDir "preview-" False (Just dir)
    File{contents} -> contents & maybe (hCenter $ txt "Could't read file") txt
    _ -> padRight Max $ padAll 1 $ txt "preview" <=> txt "placeholder"

  renderDir :: ResourceName -> Bool -> Maybe DirNode -> Widget ResourceName
  renderDir prefix hasFocus =
    maybe emptyWidget $
      view #files .> over (lensVL listNameL) (prefix <>) .> renderList renderFile hasFocus

  renderFile :: Bool -> FileId -> Widget n
  renderFile isSelected fid =
    case s.fileMap ^? ix fid of
      Nothing -> txt "huh? there is no such file"
      Just file ->
        withAttr
          ( mkAttr $
              if isSelected
                then ["selected"]
                else fileAttr s.fileMap fid
          )
          $ line file

  line :: FileInfo -> Widget n
  line file = padLeftRight 1 $ txt (decode $ nameOf file) <+> fillLine <+> str (size file)

  size :: FileInfo -> String
  size = \case
    SavedDir{dir} -> dir.files & listElements & length & show
    Dir{itemCount} -> maybe "?" show itemCount
    Link{link = maybeId} -> fromMaybe "N/A" do
      fid <- maybeId
      file <- s.fileMap ^? ix fid
      pure $ size file
    File{status} ->
      case fileSize <$> status of
        Nothing -> "?"
        Just (COff n) ->
          if
            -- this is ugly
            | n < 2 ^! 10 -> show n <> " B"
            | n < 2 ^! 20 -> n `div'` (2 ^! 10) $ " KiB"
            | n < 2 ^! 30 -> n `div'` (2 ^! 20) $ " MiB"
            | otherwise -> n `div'` (2 ^! 30) $ " GiB"

fillLine :: Widget n
fillLine = vLimit 1 $ fill ' '

fileAttr :: FileMap -> FileId -> [String]
fileAttr fileMap fileId = case fileMap ^? ix fileId of
  Nothing -> []
  Just file
    | isRealDir file -> ["directory"]
    | isDir fileMap fileId -> ["link", "directory"]
    | fileMap & hasn't (symlinked fileId) -> ["invalid-link"] -- `symlinked` fails iff it doesn't point to a valid file
    | isLink file -> ["link"]
    | otherwise -> ["file"]

infixl 8 ^!
(^!) :: Num a => a -> Int -> a
(^!) = (^)

div' :: Int64 -> Double -> String -> String
div' x y = showFFloat (Just 2) (fromIntegral x / y)

app :: AppEnv -> App AppState LcnsEvent ResourceName
app env =
  App
    { appDraw = drawTUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = withEnv env <. handleEventWith
    , appStartEvent = pass
    , appAttrMap = mkAttrMap
    }

-- because Brick doesn't export AttrName's constructor
mkAttr :: [String] -> AttrName
mkAttr = foldMap attrName

mkAttrMap :: AppState -> AttrMap
mkAttrMap =
  const $
    attrMap (fg white) $
      traversed % _1 %~ mkAttr $
        [ (["selected"], currentAttr `withBackColor` brightBlue `withForeColor` black `withStyle` bold)
        , (["directory"], currentAttr `withStyle` bold `withForeColor` brightBlue)
        , (["link"], currentAttr `withForeColor` cyan)
        , (["link", "directory"], currentAttr `withStyle` bold)
        , (["invalid-link"], currentAttr `withForeColor` blue)
        , (["top-panel"], currentAttr `withStyle` bold `withForeColor` brightBlue) -- happens to be identical to `"director"`, but that's up to change
        , (["top-panel", "selected"], currentAttr `withForeColor` white)
        ]
