{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lcns.DirTree (
  goUp,
  goDown,
  goLeft,
  goRight,
  cur,
  child,
  childOrLink,
  parent,
  buildDirNode,
  refreshSelected,
  refreshSelected',
  refreshSavedDir,
  refreshSavedDir',
  buildDirTree,
  curDir,
  childDir,
) where

import Lcns.Path
import Lcns.Prelude
import Lcns.Sort qualified as S

import Brick.Widgets.List (list, listClear, listModify, listMoveBy, listSelectedElement)
import Data.List.NonEmpty qualified as NE
import Data.Sequence qualified as Seq (fromList)
import Lcns.FileInfo (getFileInfo, nameOf, savedDir, symlinked)
import Lcns.ListUtils qualified as LU
import Lcns.Path qualified as Path
import Relude.Extra (head1)

-- | a shorthand for `#dir % cur` that seems to appear everywhere
curDir :: Lens' AppState DirNode
curDir = #dir % cur

-- why on earth isn't NonEmpty an instance of Cons?
cur :: Lens' DirTree DirNode
cur = lens getL setL
 where
  getL = head1
  setL (_ :| xs) x = x :| xs

parent :: AffineTraversal' AppState DirNode
parent = #dir % ix 1

-- listSelectedElementL is a Traversal rather than AffineTraversal, and there's no easy way to downcast
childOrLink :: AffineTraversal' DirNode FileInfo
childOrLink = #files % atraversal getElem setElem
 where
  getElem l =
    listSelectedElement l & maybe (Left l) (Right <. snd)
  setElem l item = l & listModify (const item)

-- | a shorthand for #dir % child
childDir :: AffineTraversal' AppState FileInfo
childDir = #dir % child

-- | focuses the selected file, traversing through symlinks
child :: AffineTraversal' DirTree FileInfo
child = cur % childOrLink % symlinked

scroll :: Int -> AppM ()
scroll dist = do
  curDir % #files %= listMoveBy dist
  refreshSelected

goUp :: AppM ()
goUp = scroll (-1)

goDown :: AppM ()
goDown = scroll 1

goLeft :: AppM ()
goLeft = do
  #dir %= tug goLeft'
  traversing parent refreshSavedDir
  traversing curDir refreshSavedDir

goRight :: AppM ()
goRight = do
  #dir %= tug goRight'
  refreshSelected

goLeft' :: DirTree -> Maybe DirTree
goLeft' =
  NE.uncons .> \case
    (_, Nothing) -> Nothing
    -- note: we don't have to call LU.select here
    (curDir', Just parentTree) ->
      Just $ parentTree & child .~ SavedDir curDir'

goRight' :: DirTree -> Maybe DirTree
goRight' dirTree =
  -- note that `child` *cannot* be a Dir here
  dirTree ^? child % savedDir <&> (`NE.cons` dirTree)

refreshSelected' :: (MonadIO m, MonadState AppState m) => Bool -> m ()
refreshSelected' forceRefresh = do
  traversing (childDir % savedDir) (refreshSavedDir' forceRefresh)
  traversing childDir \case
    Dir{path} -> do
      sortFunction <- use #sortFunction
      SavedDir <$> buildDirNode (dirBuilder path) sortFunction
    -- note that `child` contains `symlinked`, so this does not remove link nesting
    file@File{path, contents} -> do
      newContents <- case contents of
        Nothing -> tryJust $ decodeUtf8 <$> readFileBS path
        justContents -> pure justContents
      pure $ file & #contents .~ newContents
    other -> pure other

refreshSelected :: (MonadIO m, MonadState AppState m) => m ()
refreshSelected = refreshSelected' False

refreshSavedDir' :: (MonadIO m, MonadState AppState m) => Bool -> DirNode -> m DirNode
refreshSavedDir' forceRefresh dir =
  tryGetModTime dir.path >>= \case
    Nothing -> pure $ dir & #files %~ listClear
    Just modTime
      | dir.modTime >= modTime && not forceRefresh -> pure dir
      | otherwise ->
          use #sortFunction
            >>= buildDirNode
              DirBuilder
                { path = dir.path
                , maybeModTime = Just modTime
                , prevSelection = dir ^? childOrLink % to nameOf
                }

{- | rebuild the file list of a DirTree

TODO: if the old file list contained `SavedDir`s, don't throw away their saved selections
-}
refreshSavedDir :: (MonadIO m, MonadState AppState m) => DirNode -> m DirNode
refreshSavedDir = refreshSavedDir' False

-- | builds a new DirNode
buildDirNode :: MonadIO m => DirBuilder -> SortFunction -> m DirNode
buildDirNode DirBuilder{..} sortF = do
  fileList <- mapM getFileInfo =<< tryListDirectory path
  modTime <- whenNothing maybeModTime (getModificationTime path) -- TODO: handle inacessible dirs
  let files =
        list (decode path) (Seq.fromList $ filterHidden $ sortDir fileList) 1
          & LU.selectMaybe prevSelection

  pure DirNode{..}
 where
  sortDir = sortBy $ S.cmpWith sortF

  filterHidden
    | sortF.showDotfiles = id
    | otherwise = filter (not <. isDotfile)

  isDotfile :: FileInfo -> Bool
  isDotfile file = nameOf file ^? to decode % _head == Just '.'

buildDirTree :: MonadIO m => SortFunction -> Path Abs -> m DirTree
buildDirTree sortF path = do
  let (_, nonRootChunks) = Path.splitDirectories path
  let paths = NE.reverse $ (Path.root </>) <. joinPath <$> NE.inits nonRootChunks
  traverse buildDirNode' paths
 where
  buildDirNode' path' = buildDirNode (dirBuilder path') sortF
