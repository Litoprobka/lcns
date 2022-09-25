{-# LANGUAGE TemplateHaskell #-}

module UITypes (
    ResourceName,
    AppState(..),
    currentFiles,
    currentDir
) where

import Relude
import Lens.Micro.TH
import Brick.Widgets.List (GenericList)

type ResourceName = String

data AppState = AppState
    { _currentFiles :: GenericList ResourceName Seq FilePath
    , _currentDir :: FilePath
    } deriving Show

makeLenses ''AppState