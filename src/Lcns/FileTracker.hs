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
  WatchDescriptor,
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

watchDir :: Maybe WatchDescriptor -> INotify -> Path Abs -> BChan LcnsEvent -> IO WatchDescriptor
watchDir Nothing = watchNew
watchDir (Just descriptor) = watchOther descriptor

watchOther :: WatchDescriptor -> INotify -> Path Abs -> BChan LcnsEvent -> IO WatchDescriptor
watchOther descriptor inotify path channel = do
  removeWatch descriptor
  watchNew inotify path channel

watchNew :: INotify -> Path Abs -> BChan LcnsEvent -> IO WatchDescriptor
watchNew inotify path channel =
  addWatch inotify trackedEvents (fromAbs path) sendEvent
 where
  sendEvent Ignored = pass
  sendEvent event =
    writeBChan channel (DirEvent event)

mainWithFileTracker :: Ord n => BChan e -> App s e n -> s -> IO s
mainWithFileTracker channel app st = do
  let builder = mkVty defaultConfig
  initialVty <- builder

  customMain initialVty builder (Just channel) app st
