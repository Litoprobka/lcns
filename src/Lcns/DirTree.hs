{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lcns.DirTree (
  goUp,
  goDown,
  goLeft,
  goRight,
  child,
  childOrLink,
  buildDir,
  refreshSelected,
  refreshSelected',
  refreshSavedDir,
  refreshSavedDir',
  buildParentDir,
) where

import Lcns.Path
import Lcns.Prelude
import Lcns.Sort qualified as S

import Brick.Widgets.List (list, listClear, listModify, listMoveBy, listSelectedElement)
import Data.Sequence qualified as Seq (fromList)
import Lcns.FileInfo (getFileInfo, nameOf, savedDir, symlinked)
import Lcns.ListUtils qualified as LU

-- listSelectedElementL is a Traversal rather than AffineTraversal, and there's no easy way to downcast
childOrLink :: AffineTraversal' DirTree FileInfo
childOrLink = #files % atraversal getElem setElem
 where
  getElem l =
    listSelectedElement l & maybe (Left l) (Right <. snd)
  setElem l item = l & listModify (const item)

-- | focuses the selected file, traversing through symlinks
child :: AffineTraversal' DirTree FileInfo
child = childOrLink % symlinked

scroll :: Int -> AppM ()
scroll dist = do
  #dir % #files %= listMoveBy dist
  refreshSelected

goUp :: AppM ()
goUp = scroll (-1)

goDown :: AppM ()
goDown = scroll 1

goLeft :: AppM ()
goLeft = do
  prevDir <- use #dir
  prevDir.parent & onJust \parent -> do
    #dir .= parent
    #dir % child .= SavedDir prevDir -- note: we don't have to call LU.select here
    let refreshOrBuild (Just parentDir) = Just <$> refreshSavedDir parentDir
        refreshOrBuild Nothing = use #sortFunction >>= buildParentDir parent.path -- confusingly, this refers to *now-current dir*.path
    traversing (#dir % #parent) refreshOrBuild

goRight :: AppM ()
goRight = do
  prevDir <- use #dir
  -- note that `child` *cannot* be a Dir here
  prevDir ^? child % savedDir & onJust \childDir -> do
    #dir .= childDir
    #dir % #parent ?= prevDir
    refreshSelected

refreshSelected' :: (MonadIO m, MonadState AppState m) => Bool -> m ()
refreshSelected' shouldRefresh = do
  traversing (#dir % child % savedDir) (refreshSavedDir' shouldRefresh)
  traversing (#dir % child) \case
    Dir{path} -> do
      dir <- use #dir
      use #sortFunction
        >>= buildDir (dirBuilder path & #parent ?~ dir)
        <&> SavedDir -- note that `child` contains `symlinked`, so this does not remove link nesting
    nonDir -> pure nonDir

refreshSelected :: (MonadIO m, MonadState AppState m) => m ()
refreshSelected = refreshSelected' False

refreshSavedDir' :: (MonadIO m, MonadState AppState m) => Bool -> DirTree -> m DirTree
refreshSavedDir' shouldRefresh dir =
  tryGetModTime dir.path >>= \case
    Nothing -> pure $ dir & #files %~ listClear
    Just modTime
      | dir.modTime >= modTime && not shouldRefresh -> pure dir
      | otherwise ->
          use #sortFunction
            >>= buildDir
              DirBuilder
                { path = dir.path
                , parent = dir.parent
                , maybeModTime = Just modTime
                , prevSelection = dir ^? child % to nameOf
                }

{- | rebuild the file list of a DirTree
TODO: if the old file list contained `SavedDir`s, don't throw away their saved selections
-}
refreshSavedDir :: (MonadIO m, MonadState AppState m) => DirTree -> m DirTree
refreshSavedDir = refreshSavedDir' False

buildParentDir :: MonadIO m => Path Abs -> SortFunction -> m (Maybe DirTree)
buildParentDir currentPath sortF =
  takeParent currentPath & traverse \parentPath ->
    sortF & buildDir (dirBuilder parentPath & #prevSelection ?~ takeFileName currentPath)

-- | builds a new DirTree node
buildDir :: MonadIO m => DirBuilder -> SortFunction -> m DirTree
buildDir DirBuilder{..} sortF = do
  fileList <- mapM getFileInfo =<< tryListDirectory path
  modTime <- whenNothing maybeModTime (getModificationTime path) -- TODO: handle inacessible dirs
  let files =
        list (decode path) (Seq.fromList $ filterHidden $ sortDir fileList) 1
          & LU.selectMaybe prevSelection

  pure DirTree{..}
 where
  sortDir = sortBy $ S.cmpWith sortF

  filterHidden
    | sortF.showDotfiles = id
    | otherwise = filter (not <. isDotfile)

  isDotfile :: FileInfo -> Bool
  isDotfile file = nameOf file ^? to decode % _head == Just '.'
