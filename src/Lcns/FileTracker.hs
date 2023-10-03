module Lcns.FileTracker (watchDir, mainWithFileTracker) where

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

-- reinventing a tiny bit of singletons, huh
class DemoteDir (dir :: WhichDir) where
  whichDir :: DirWatcher dir -> WhichDir

instance DemoteDir 'Current where
  whichDir = const Current

instance DemoteDir 'Parent where
  whichDir = const Parent

instance DemoteDir 'Child where
  whichDir = const Child

trackedEvents :: [EventVariety]
trackedEvents =
  [ -- Attrib
    Move
  , Create
  , Delete
  ]

watchDir :: Maybe WatchDescriptor -> INotify -> Path Abs -> BChan LcnsEvent -> IO WatchDescriptor
watchDir Nothing = watchNew
watchDir (Just descriptor) = watchOther descriptor

watchOther :: WatchDescriptor -> INotify -> Path Abs -> BChan LcnsEvent -> IO WatchDescriptor
watchOther descriptor inotify path channel = do
  removeWatch descriptor
  watchNew inotify path channel

watchDir :: DemoteDir dir => DirWatcher dir -> INotify -> Path Abs -> BChan LcnsEvent -> IO (DirWatcher dir)
watchDir w@(DirWatcher maybeWatch) inotify path channel = do
  maybeWatch & onJust removeWatch
  watchNew (whichDir w) inotify path channel

watchNew :: WhichDir -> INotify -> Path Abs -> BChan LcnsEvent -> IO (DirWatcher dir)
watchNew dir inotify path channel =
  DirWatcher <. Just <$> addWatch inotify trackedEvents (fromAbs path) sendEvent
 where
  sendEvent Ignored = pass
  sendEvent event =
    writeBChan channel (DirEvent dir event)

mainWithFileTracker :: Ord n => BChan e -> App s e n -> s -> IO s
mainWithFileTracker channel app st = do
  let builder = mkVty defaultConfig
  initialVty <- builder

  customMain initialVty builder (Just channel) app st
