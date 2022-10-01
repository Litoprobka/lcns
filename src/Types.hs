{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Types (
    ResourceName,
    FileInfo, name, perms, status, mkFileInfo, isDirLink,
    SortFunction(..), invertSort,
    AppState(..), currentFiles, currentDir, sortFunction
) where

import           Relude

import           Brick.Widgets.List (GenericList, list)
import           Data.Default
import           Data.Sequence      as Seq (empty)
import           Lens.Micro.TH      (makeLenses)
import           System.Directory   (Permissions, doesDirectoryExist,
                                     doesFileExist, emptyPermissions,
                                     getPermissions)
import           System.Posix       (isSymbolicLink)
import           System.Posix.Files (FileStatus, getSymbolicLinkStatus)
--import System.FilePath -- note: a new version of filepath supports OsString API, I should consider switching when it gets into LTS

type ResourceName = String -- what is this used for?

data FileInfo = FileInfo
    { _name      :: FilePath
    , _perms     :: Permissions -- in theory, Permissions is more convenient than FileMode
    , _status    :: FileStatus
    , _isDirLink :: Bool -- an ugly workaround
    }

makeLenses ''FileInfo

mkFileInfo :: FilePath -> IO FileInfo
mkFileInfo _name = do
    _perms <- ifM (doesFileExist _name `mOr` doesDirectoryExist _name)
        (getPermissions _name)
        (pure emptyPermissions)
    _status <- getSymbolicLinkStatus _name
    _isDirLink <- (isSymbolicLink _status &&) <$> doesDirectoryExist _name
    pure $ FileInfo {..}
    where mOr = liftA2 (||)

data SortFunction
    = Def
    | Reversed
    | Custom (FileInfo -> FileInfo -> Ordering)
    | CustomReversed (FileInfo -> FileInfo -> Ordering)

invertSort :: SortFunction -> SortFunction
invertSort Def                = Reversed
invertSort Reversed           = Def
invertSort (Custom f)         = CustomReversed f
invertSort (CustomReversed f) = Custom f

data AppState = AppState
    { _currentFiles :: GenericList ResourceName Seq FileInfo
    , _currentDir   :: FilePath
    , _sortFunction :: SortFunction
    }

makeLenses ''AppState

instance Default AppState where
    def = AppState
        { _currentFiles = list "empty" Seq.empty 0
        , _currentDir = ""
        , _sortFunction = Def
        }

