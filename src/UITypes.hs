{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

module UITypes (
    ResourceName,
    AppState(..),
    name, perms, status, mkFileInfo,
    currentFiles, currentDir
) where

import Relude
import Lens.Micro.TH
import Brick.Widgets.List (GenericList)
import System.Posix.Files (FileStatus, getFileStatus)
import System.Directory (Permissions, getPermissions)
--import System.FilePath -- note: a new version of filepath supports OsString API, I should consider switching when it gets into LTS

type ResourceName = String -- what is this used for?

data FileInfo = FileInfo
    { _name :: FilePath
    , _perms :: Permissions -- in theory, Permissions is more convenient than FileMode
    , _status :: FileStatus
    }

makeLenses ''FileInfo

mkFileInfo :: FilePath -> IO FileInfo
mkFileInfo _name = do
    _perms <- getPermissions _name
    _status <- getFileStatus _name
    pure $ FileInfo {..}

data AppState = AppState
    { _currentFiles :: GenericList ResourceName Seq FileInfo
    , _currentDir :: FilePath
    }

makeLenses ''AppState