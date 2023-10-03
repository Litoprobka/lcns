module Lcns.FileTracker (watchDir, mainWithFileTracker, killWatcher) where

import Lcns.Prelude

import Brick (App, customMain)
import Brick.BChan (BChan, writeBChan)
import Graphics.Vty (defaultConfig, mkVty)
import Lcns.Path (fromAbs)
import System.INotify (
  Event (..),
  EventVariety (..),
  INotify,
  addWatch,
  removeWatch,
 )

trackedEvents :: [EventVariety]
trackedEvents =
  [ -- Attrib
    Move
  , Create
  , Delete
  ]

-- at some point I should figure out linear types & linear optics, then use them here
-- but for now it is just an invariant:
-- `DirWatcher`s should only be interacted with by `watchDir`/ killWatcher
watchDir :: MonadIO m => DirWatcher -> INotify -> Path Abs -> BChan LcnsEvent -> m DirWatcher
watchDir w inotify path channel = do
  newWatch <- io $ addWatch inotify trackedEvents (fromAbs path) sendEvent
  killWatcher w <&> #watcher ?~ newWatch
 where
  sendEvent Ignored = pass
  sendEvent event =
    writeBChan channel $ DirEvent (w ^. #dir) event

killWatcher :: MonadIO m => DirWatcher -> m DirWatcher
killWatcher w = do
  io $ w ^. #watcher & onJust removeWatch
  pure $ w & #watcher .~ Nothing

mainWithFileTracker :: Ord n => BChan e -> App s e n -> s -> IO s
mainWithFileTracker channel app st = do
  let builder = mkVty defaultConfig
  initialVty <- builder

  customMain initialVty builder (Just channel) app st
