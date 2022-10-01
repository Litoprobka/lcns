module Lcns.Types (
    ResourceName,
    FileInfo(..), mkFileInfo,
    SortFunction(..), invertSort,
    AppState(..),
) where

import           Relude

import           Brick.Widgets.List (GenericList, list)
import           Data.Default
import           Data.Sequence      as Seq (empty)
import           System.Directory   (Permissions, doesDirectoryExist,
                                     doesFileExist, emptyPermissions,
                                     getPermissions)
import           System.Posix       (isSymbolicLink)
import           System.Posix.Files (FileStatus, getSymbolicLinkStatus)
--import System.FilePath -- note: a new version of filepath supports OsString API, I should consider switching when it gets into LTS

type ResourceName = String -- what is this used for?

data FileInfo = FileInfo
    { name      :: FilePath
    , perms     :: Permissions -- in theory, Permissions is more convenient than FileMode
    , status    :: FileStatus
    , isDirLink :: Bool -- an ugly workaround
    }

mkFileInfo :: FilePath -> IO FileInfo
mkFileInfo name = do
    perms <- ifM (doesFileExist name `mOr` doesDirectoryExist name)
        (getPermissions name)
        (pure emptyPermissions)
    status <- getSymbolicLinkStatus name
    isDirLink <- (isSymbolicLink status &&) <$> doesDirectoryExist name
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
    { currentFiles :: GenericList ResourceName Seq FileInfo
    , currentDir   :: FilePath
    , sortFunction :: SortFunction
    , showDotfiles :: Bool
    }

instance Default AppState where
    def = AppState
        { currentFiles = list "empty" Seq.empty 0
        , currentDir = ""
        , sortFunction = Def
        , showDotfiles = True
        }
