{-# LANGUAGE MultiWayIf #-}

module Lcns.Main (lcns) where

import Brick hiding (Down, on)
import Brick.BChan (BChan, newBChan)
import Brick.Widgets.List (list, renderList)
import Data.HashMap.Strict qualified as Map
import Data.Sequence qualified as Seq
import Graphics.Vty.Attributes
import Lcns.Config
import Lcns.EventHandling
import Lcns.FileTracker
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
      mainWithFileTracker channel app
        <=< buildInitialState channel

buildInitialState :: BChan LcnsEvent -> INotify -> IO AppState
buildInitialState channel inotify = do
  let watchers =
        INotifyState
          { inotify
          , channel
          , dirWatcher = Nothing
          , parentWatcher = Nothing
          , childWatcher = Nothing
          }
  refreshState
    AppState
      { files = list "empty" Seq.empty 0
      , dir = Path mempty
      , sortFunction = SF{reversed = False, func = Nothing}
      , showDotfiles = True
      , watchers
      , selections = Map.empty
      }

preview :: Widget ResourceName
preview = padAll 1 $ txt "preview" <=> txt "placeholder"

parentDir :: Widget ResourceName
parentDir = padAll 1 $ txt "parent dir" <=> txt "placeholder"

drawTUI :: AppState -> [Widget ResourceName]
drawTUI s =
  one $
    topPanel
      <=> hBox [parentDir, renderList renderFile True s.files, preview]
      <=> bottomPanel
 where
  renderFile isSelected file =
    withAttr
      ( attrName $
          if isSelected
            then "selected"
            else fileAttr file.typedInfo
      )
      $ line file

  fileAttr fileType = case fileType of
    Link Nothing -> "invalid-link"
    Link (Just (Dir _)) -> "directory-link"
    Link (Just (File _)) -> "link"
    Link (Just nestedLink) -> fileAttr nestedLink
    Dir _ -> "directory"
    File _ -> "file"

  line file = padLeftRight 1 $ txt (decode file.name) <+> fillLine <+> str (size file)
  size :: FileInfo -> String
  size file@FileInfo{typedInfo = File _} =
    let (COff n) = fileSize file.status
     in if
          -- this is ugly
          | n < 2 ^! 10 -> show n <> " B"
          | n < 2 ^! 20 -> n `div'` (2 ^! 10) $ " K"
          | n < 2 ^! 30 -> n `div'` (2 ^! 20) $ " M"
          | otherwise -> n `div'` (2 ^! 30) $ " G"
  size FileInfo{typedInfo = Dir di} = maybe "N/A" show di.itemCount
  size file@FileInfo{typedInfo = Link (Just l)} = size file{typedInfo = l}
  size FileInfo{typedInfo = Link Nothing} = "N/A"

  infixl 8 ^!
  (^!) :: Num a => a -> Int -> a
  (^!) = (^)

  div' :: Int64 -> Double -> String -> String
  div' x y = showFFloat (Just 2) (fromIntegral x / y)

  topPanel = txt (decode s.dir) <+> fillLine
  bottomPanel = fillLine -- placeholder
  fillLine = vLimit 1 $ fill ' '

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
