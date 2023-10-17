{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- v  NoFieldSelectors makes partial fields safe to use
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lcns.Types (
  ResourceName,
  FileSeq,
  Relativity (..),
  Path (..),
  FileInfo (..),
  SortFunction (..),
  INotifyState (..),
  AppState (..),
  WhichDir (..),
  LcnsEvent (..),
  DirWatcher (..),
  AppM,
  Config (..),
  DirTree (..),
  DirBuilder (..),
)
where

import Brick (EventM)
import Brick.BChan (BChan)
import Brick.Widgets.List (GenericList)
import Data.Time (UTCTime)
import Graphics.Vty.Input (Key, Modifier)
import Optics.Operators ((.~))
import Optics.TH (
  fieldLabelsRulesFor,
  generateUpdateableOptics,
  makeFieldLabelsFor,
  makeFieldLabelsNoPrefix,
  makeFieldLabelsWith,
  makePrismLabels,
 )
import Relude
import System.INotify (Event, INotify, WatchDescriptor)
import System.OsPath.Posix (PosixPath)
import System.Posix.Files (FileStatus)

type ResourceName = Text -- what is this used for?

data Relativity
  = Rel
  | Abs
  | Unknown

-- invariant: Path _   never contains a trailing slash
-- invariant: Path Abs is never empty
newtype Path (rel :: Relativity) = Path PosixPath
  deriving (Eq, Ord, Hashable, Show)

type FileSeq = GenericList ResourceName Seq FileInfo

data DirTree = DirTree
  { modTime :: UTCTime
  , path :: Path Abs
  , files :: FileSeq
  , parent :: Maybe DirTree
  }

-- copypasting ain't nice, but it's still better than a lot of nesting
-- optics also makes it convenient (and safe) to work with partial record fields
-- this would have been way nicer with row-polymorphic records
data FileInfo
  = Dir
      { path :: Path Abs
      , name :: Path Rel
      , itemCount :: Maybe Int -- invariant: the selected file in current dir is never a Dir
      }
  | Link
      { path :: Path Abs
      , name :: Path Rel
      , status :: Maybe FileStatus
      , link :: Maybe FileInfo
      }
  | File
      { path :: Path Abs
      , name :: Path Rel
      , status :: Maybe FileStatus
      , contents :: Maybe Text
      }
  | SavedDir {dir :: DirTree}

makeFieldLabelsNoPrefix ''DirTree
makeFieldLabelsNoPrefix ''FileInfo
makePrismLabels ''FileInfo

data SortFunction = SF
  { reversed :: Bool
  , func :: Maybe (FileInfo -> FileInfo -> Ordering)
  , showDotfiles :: Bool
  }

makeFieldLabelsNoPrefix ''SortFunction

data WhichDir
  = Current
  | Parent
  | Child
  deriving (Show, Eq)

data LcnsEvent
  = DirEvent WhichDir Event

data DirWatcher = DirWatcher
  { dir :: WhichDir
  , watcher :: Maybe WatchDescriptor
  }

makeFieldLabelsWith (fieldLabelsRulesFor [("dir", "dir")] & generateUpdateableOptics .~ False) ''DirWatcher
makeFieldLabelsWith (fieldLabelsRulesFor [("watcher", "watcher")]) ''DirWatcher

data INotifyState = INotifyState
  { channel :: BChan LcnsEvent
  , inotify :: INotify
  , parentWatcher :: DirWatcher
  , dirWatcher :: DirWatcher
  , childWatcher :: DirWatcher
  }

makeFieldLabelsNoPrefix ''INotifyState
makeFieldLabelsFor [("parentWatcher", "all"), ("dirWatcher", "all"), ("childWatcher", "all")] ''INotifyState

-- #all is not the best name, but it will do for now

data AppState = AppState
  { dir :: DirTree
  , sortFunction :: SortFunction
  , watchers :: INotifyState
  }

makeFieldLabelsNoPrefix ''AppState
makeFieldLabelsFor [("parentFiles", "allFiles"), ("files", "allFiles"), ("childFiles", "allFiles")] ''AppState

type AppM a = EventM ResourceName AppState a

newtype Config = Config
  { keybindings :: Key -> [Modifier] -> AppM ()
  }

data DirBuilder = DirBuilder
  { path :: Path Abs
  , parent :: Maybe DirTree
  , maybeModTime :: Maybe UTCTime
  , prevSelection :: Maybe (Path Rel)
  }

makeFieldLabelsNoPrefix ''DirBuilder
