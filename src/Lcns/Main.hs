{-# LANGUAGE MultiWayIf #-}

module Lcns.Main (lcns) where

import Brick hiding (Down, on)
import Brick.BChan (BChan, newBChan)
import Brick.Widgets.List (renderList)
import Data.HashMap.Strict qualified as Map
import Graphics.Vty.Attributes
import Lcns.Config
import Lcns.EventHandling
import Lcns.FileTracker
import Lcns.ListUtils qualified as LU
import Lcns.Path
import Lcns.Prelude hiding (preview)
import Numeric (showFFloat)
import System.INotify (INotify, withINotify)
import System.Posix.ByteString (COff (COff), fileSize)

lcns :: Config -> IO ()
lcns _ = do
  channel <- newBChan 8 -- in theory, 3 should work
  void $
    withINotify $
      buildInitialState channel
        >=> mainWithFileTracker channel app

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
  dir <- getCurrentDirectory
  refreshState dir
    =<< refreshState
      dir
      AppState
        { files = emptyFiles Current "files"
        , parentFiles = emptyFiles Parent "parent files"
        , childFiles = emptyFiles Child "child files"
        , dir
        , sortFunction = SF{reversed = False, func = Nothing}
        , showDotfiles = True
        , watchers
        , selections = Map.empty
        }
 where
  emptyFiles :: WhichDir -> Text -> DirFiles
  emptyFiles dir name = DirFiles{dir, name, list = LU.empty name}

  emptyWatcher dir = DirWatcher{dir, watcher = Nothing}

preview :: DirWatcher -> FileSeq -> Widget ResourceName
preview w childFiles = case w.watcher of
  Just _ -> renderDir False childFiles
  Nothing -> padAll 1 $ txt "preview" <=> txt "placeholder"

spacer :: Widget ResourceName
spacer = txt "\8203" -- it's a kind of magic...

drawTUI :: AppState -> [Widget ResourceName]
drawTUI s =
  one $
    topPanel
      <=> hBox
        [ renderDir False s.parentFiles.list
        , spacer
        , renderDir True s.files.list
        , spacer
        , preview s.watchers.childWatcher s.childFiles.list
        ]
      <=> bottomPanel
 where
  topPanel = txt (decode s.dir) <+> fillLine
  bottomPanel = fillLine -- placeholder

renderDir :: Bool -> FileSeq -> Widget ResourceName
renderDir = renderList renderFile

fillLine :: Widget n
fillLine = vLimit 1 $ fill ' '

line :: FileInfo -> Widget n
line file = padLeftRight 1 $ txt (decode file.name) <+> fillLine <+> str (size file)

renderFile :: Bool -> FileInfo -> Widget n
renderFile isSelected file =
  withAttr
    ( attrName $
        if isSelected
          then "selected"
          else fileAttr file.typedInfo
    )
    $ line file

fileAttr :: IsString a => FileType -> a
fileAttr fileType = case fileType of
  Link Nothing -> "invalid-link"
  Link (Just (Dir _)) -> "directory-link"
  Link (Just (File _)) -> "link"
  Link (Just nestedLink) -> fileAttr nestedLink
  Dir _ -> "directory"
  File _ -> "file"

size :: FileInfo -> String
size file@FileInfo{typedInfo = File _} =
  let (COff n) = fileSize file.status
   in if
        -- this is ugly
        | n < 2 ^! 10 -> show n <> " B"
        | n < 2 ^! 20 -> n `div'` (2 ^! 10) $ " KiB"
        | n < 2 ^! 30 -> n `div'` (2 ^! 20) $ " MiB"
        | otherwise -> n `div'` (2 ^! 30) $ " GiB"
size FileInfo{typedInfo = Dir di} = maybe "?" show di.itemCount
size file@FileInfo{typedInfo = Link (Just l)} = size file{typedInfo = l}
size FileInfo{typedInfo = Link Nothing} = "N/A"

infixl 8 ^!
(^!) :: Num a => a -> Int -> a
(^!) = (^)

div' :: Int64 -> Double -> String -> String
div' x y = showFFloat (Just 2) (fromIntegral x / y)

app :: App AppState LcnsEvent ResourceName
app =
  App
    { appDraw = drawTUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pass
    , appAttrMap = mkAttrMap
    }

mkAttrMap :: AppState -> AttrMap
mkAttrMap =
  const $
    attrMap (fg white) $
      first attrName
        <$> [ ("selected", currentAttr `withBackColor` brightBlue `withForeColor` black `withStyle` bold)
            , ("directory", currentAttr `withStyle` bold `withForeColor` brightBlue)
            , ("link", currentAttr `withForeColor` cyan)
            , ("directory-link", currentAttr `withForeColor` cyan `withStyle` bold)
            , ("invalid-link", currentAttr `withForeColor` blue)
            ]
