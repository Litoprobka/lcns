{-# LANGUAGE MultiWayIf #-}
module Lcns.Types (
    ResourceName,
    DirData(..),
    FileData(..),
    FileType(..),
    FileInfo(..), mkFileInfo, isDir,
    SortFunction(..), invertSort,
    AppState(..),
) where

import           Relude

import           Brick.Widgets.List (GenericList, list)
import           Data.Default
import           Data.Sequence      as Seq (empty)
import           System.Directory   (doesDirectoryExist, listDirectory)
import           System.Posix       (isSymbolicLink, readSymbolicLink)
import           System.Posix.Files (FileStatus, getSymbolicLinkStatus)
--import System.FilePath -- note: a new version of filepath supports OsString API, I should consider switching when it gets into LTS

type ResourceName = String -- what is this used for?

data DirData = DirData
    { itemCount :: Int
    }
data FileData = FileData -- this doesn't seem like good naming to me
    {
    }
data FileType
    = Dir DirData
    | Link FileType
    | File FileData

data FileInfo = FileInfo
    { name      :: FilePath
    , status    :: FileStatus
    , typedInfo :: FileType -- couldn't come up with a better name
    }

mkFileInfo :: FilePath -> IO FileInfo
mkFileInfo name = do
    status <- getSymbolicLinkStatus name
    typedInfo <- mkTypedInfo status name

    pure $ FileInfo {..} where
        mkTypedInfo :: FileStatus -> FilePath -> IO FileType
        mkTypedInfo status path = do
            isDir' <- doesDirectoryExist path
            if
                | isDir' -> do
                    itemCount <- length <$> listDirectory path
                    pure $ Dir $ DirData{..}
                | isSymbolicLink status -> do
                    path' <- readSymbolicLink path
                    status' <- getSymbolicLinkStatus path'
                    Link <$> mkTypedInfo status' path'
                | otherwise -> pure $ File FileData

isDir :: FileInfo -> Bool
isDir FileInfo{typedInfo = Dir _} = True
isDir _                           = False


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
