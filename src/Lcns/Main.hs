{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Lcns.Main (lcns) where

import Brick hiding (Down, on)
import Brick.BChan (BChan, newBChan)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.List (listElements, renderList)
import Graphics.Vty.Attributes
import Lcns.Config
import Lcns.DirTree (buildDir, buildParentDir, child, refreshSelected)
import Lcns.EventHandling
import Lcns.FileInfo (isDir, isLink, isRealDir, nameOf, symlinked)
import Lcns.FileTracker
import Lcns.Path
import Lcns.Prelude hiding (preview)
import Numeric (showFFloat)
import System.INotify (INotify, withINotify)
import System.Posix.ByteString (COff (COff), fileSize)

lcns :: Config -> IO ()
lcns cfg = do
  channel <- newBChan 8 -- in theory, 3 should work
  void $
    withINotify $
      buildInitialState channel
        >=> mainWithFileTracker channel (app cfg)

buildInitialState :: BChan LcnsEvent -> INotify -> IO AppState
buildInitialState channel inotify = do
  let watchers =
        INotifyState
          { inotify
          , channel
          , parentWatcher = emptyWatcher Parent
          , dirWatcher = emptyWatcher Current
          , childWatcher = emptyWatcher Child
          }
  let sortFunction = SF{reversed = False, func = Nothing, showDotfiles = True}
  path <- getCurrentDirectory

  parentDir <- buildParentDir path sortFunction
  dir <- buildDir (dirBuilder path & #parent .~ parentDir) sortFunction

  AppState{..} & execStateT (refreshSelected >> refreshWatchers)
 where
  emptyWatcher dir = DirWatcher{dir, watcher = Nothing}

-- note: since `preview` is only ever called from `drawTUI` via `% child`, we don't have to handle symlinks here at all
preview :: Maybe FileInfo -> Widget ResourceName
preview = maybe emptyWidget \case
  SavedDir dir -> renderDir False (Just dir)
  File{contents} -> contents & maybe (hCenter $ txt "Could't read file") txt
  _ -> padRight Max $ padAll 1 $ txt "preview" <=> txt "placeholder"

spacer :: Widget ResourceName
spacer = txt "\8203" -- it's a kind of magic...

drawTUI :: AppState -> [Widget ResourceName]
drawTUI s =
  one $
    topPanel
      <=> hBox
        [ hLimitPercent 25 $ renderDir False s.dir.parent
        , spacer
        , hLimitPercent 40 $ renderDir True $ s ^? #dir
        , spacer
        , preview $ s ^? #dir % child
        ]
      <=> bottomPanel
 where
  topPanel =
    padLeft (Pad 5) $
      hBox
        [ withAttr (attrName "top-panel") $ txt (decode s.dir.path <> "/")
        , withAttr (attrName "top-panel" <> attrName "selected") $ txt $ maybe "" (decode <. nameOf) (s ^? #dir % child)
        ]
  bottomPanel = fillLine -- placeholder

renderDir :: Bool -> Maybe DirTree -> Widget ResourceName
renderDir hasFocus = maybe emptyWidget $ view #files .> renderList renderFile hasFocus

fillLine :: Widget n
fillLine = vLimit 1 $ fill ' '

line :: FileInfo -> Widget n
line file = padLeftRight 1 $ txt (decode $ nameOf file) <+> fillLine <+> str (size file)

renderFile :: Bool -> FileInfo -> Widget n
renderFile isSelected file =
  withAttr
    ( mkAttr $
        if isSelected
          then ["selected"]
          else fileAttr file
    )
    $ line file

fileAttr :: FileInfo -> [String]
fileAttr file
  | isRealDir file = ["directory"]
  | isDir file = ["link", "directory"]
  | isn't symlinked file = ["invalid-link"] -- `symlinked` fails iff it doesn't point to a valid file
  | isLink file = ["link"]
  | otherwise = ["file"]

size :: FileInfo -> String
size = \case
  SavedDir{dir} -> dir.files & listElements & length & show
  Dir{itemCount} -> maybe "?" show itemCount
  Link{link = Nothing} -> "N/A"
  Link{link = Just fi} -> size fi
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

infixl 8 ^!
(^!) :: Num a => a -> Int -> a
(^!) = (^)

div' :: Int64 -> Double -> String -> String
div' x y = showFFloat (Just 2) (fromIntegral x / y)

app :: Config -> App AppState LcnsEvent ResourceName
app cfg =
  App
    { appDraw = drawTUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEventWith cfg
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
