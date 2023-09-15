module Lcns.FileTracker where

import           Relude

import           Lcns.Types
import           Lcns.Utils

import           Brick          (App, customMain)
import           Brick.BChan    (BChan, newBChan, writeBChan)
import           Graphics.Vty   (defaultConfig, mkVty)
import           System.INotify (EventVariety (..), INotify, WatchDescriptor,
                                 addWatch, removeWatch, Event(..))

trackedEvents :: [EventVariety]
trackedEvents =
    [ --Attrib
      Move
     , Create
    , Delete
    ]

watchDir :: Maybe WatchDescriptor -> INotify -> FilePath -> BChan LcnsEvent -> IO WatchDescriptor
watchDir Nothing           = watchNew
watchDir (Just descriptor) = watchOther descriptor

watchOther :: WatchDescriptor -> INotify -> FilePath -> BChan LcnsEvent -> IO WatchDescriptor
watchOther descriptor inotify path channel = do
    removeWatch descriptor
    watchNew inotify path channel

watchNew :: INotify -> FilePath -> BChan LcnsEvent -> IO WatchDescriptor
watchNew inotify path channel =
    addWatch inotify trackedEvents (fromString path) sendEvent
    where
        sendEvent Ignored = pass
        sendEvent event =
            writeBChan channel (DirEvent event)

mainWithFileTracker :: Ord n => BChan e -> App s e n -> s -> IO s
mainWithFileTracker channel app st = do
    let builder = mkVty defaultConfig
    initialVty <- builder

    customMain initialVty builder (Just channel) app st
