module Lcns.Types (
    ResourceName,
    FileSeq,
    DirData(..),
    FileData(..),
    FileType(..),
    FileInfo(..),
    SortFunction(..),
    AppState(..),
    LcnsEvent(..)
) where

import           Relude

import           Brick.Widgets.List (GenericList, list)

import           Brick.BChan        (BChan)
import           System.INotify     (Event, INotify, WatchDescriptor)
import           System.Posix.Files (FileStatus)
import           GHC.Generics (Generic)
--import System.FilePath -- note: a new version of filepath supports OsString API, I should consider switching when it gets into LTS

type ResourceName = String -- what is this used for?
type FileSeq = GenericList ResourceName Seq FileInfo

data DirData = DirData
    { itemCount :: Int
    } deriving Generic
data FileData = FileData -- this doesn't seem like good naming to me
    {
    }
data FileType
    = Dir DirData
    | Link (Maybe FileType)
    | File FileData

data FileInfo = FileInfo
    { name      :: FilePath
    , status    :: FileStatus
    , typedInfo :: FileType -- couldn't come up with a better name
    } deriving Generic

data SortFunction = SF
    { reversed :: Bool
    , func     :: Maybe (FileInfo -> FileInfo -> Ordering)
    } deriving Generic

data AppState = AppState
    { currentFiles   :: FileSeq
    , currentDir     :: FilePath
    , sortFunction   :: SortFunction
    , showDotfiles   :: Bool

    -- this is ugly, rewrite asap
    , channel        :: BChan LcnsEvent
    , inotify        :: INotify
    , dirWatcher     :: Maybe WatchDescriptor
    , parentWatcher  :: Maybe WatchDescriptor
    , childWatcher   :: Maybe WatchDescriptor

    , selectionCache :: HashMap FilePath FilePath
    } deriving Generic

data LcnsEvent
    = DirEvent Event
