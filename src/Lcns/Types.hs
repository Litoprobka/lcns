{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lcns.Types (
  ResourceName,
  FileSeq,
  Relativity (..),
  Path (..),
  DirData (..),
  FileData (..),
  FileType (..),
  FileInfo (..),
  SortFunction (..),
  INotifyState (..),
  AppState (..),
  WhichDir (..),
  LcnsEvent (..),
  DirWatcher (..),
)
where

import Brick.BChan (BChan)
import Brick.Widgets.List (GenericList)
import Optics.Operators ((.~))
import Optics.TH (
  fieldLabelsRulesFor,
  generateUpdateableOptics,
  makeFieldLabelsFor,
  makeFieldLabelsNoPrefix,
  makeFieldLabelsWith,
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

newtype DirData = DirData
  { itemCount :: Maybe Int
  }

makeFieldLabelsNoPrefix ''DirData

data FileData = FileData -- this doesn't seem like good naming to me
  {
  }

data FileType
  = Dir DirData
  | Link ~(Maybe FileType)
  | File FileData

data FileInfo = FileInfo
  { name :: Path Rel
  , status :: FileStatus
  , typedInfo :: FileType -- couldn't come up with a better name
  }

makeFieldLabelsNoPrefix ''FileInfo

type FileSeq = GenericList ResourceName Seq FileInfo

data SortFunction = SF
  { reversed :: Bool
  , func :: Maybe (FileInfo -> FileInfo -> Ordering)
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
  { files :: FileSeq
  , parentFiles :: FileSeq
  , childFiles :: FileSeq
  , dir :: Path Abs
  , sortFunction :: SortFunction
  , showDotfiles :: Bool
  , watchers :: INotifyState
  , selections :: HashMap (Path Abs) (Path Rel)
  }

makeFieldLabelsNoPrefix ''AppState
