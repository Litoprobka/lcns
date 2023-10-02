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
  LcnsEvent (..),
)
where

import Brick.BChan (BChan)
import Brick.Widgets.List (GenericList)
import Optics.TH (makeFieldLabelsNoPrefix)
import Relude
import System.INotify (Event, INotify, WatchDescriptor)
import System.OsPath.Posix (PosixPath)
import System.Posix.Files (FileStatus)

type ResourceName = Text -- what is this used for?

data Relativity
  = Rel
  | Abs
  | Unknown

-- invariant: Path should never contain a trailing slash
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

newtype LcnsEvent
  = DirEvent Event

data INotifyState = INotifyState
  { channel :: BChan LcnsEvent
  , inotify :: INotify
  , -- maybe descriptors are stil kinda meh
    dirWatcher :: Maybe WatchDescriptor
  , parentWatcher :: Maybe WatchDescriptor
  , childWatcher :: Maybe WatchDescriptor
  }

makeFieldLabelsNoPrefix ''INotifyState

data AppState = AppState
  { files :: FileSeq
  , dir :: Path Abs
  , sortFunction :: SortFunction
  , showDotfiles :: Bool
  , watchers :: INotifyState
  , selections :: HashMap (Path Abs) (Path Rel)
  }

makeFieldLabelsNoPrefix ''AppState
