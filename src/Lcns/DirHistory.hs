{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lcns.DirHistory (
  goUp,
  goDown,
  goLeft,
  goRight,
  cur,
  childOrLink,
  parent,
  addDirNode,
  refreshSelected,
  refreshSelected',
  refreshSavedDir,
  refreshSavedDir',
  buildDirHistory,
  curDir,
  curDirId,
  childDir,
  child,
  childId,
  parentDir,
) where

import Lcns.Path
import Lcns.Prelude
import Lcns.Sort qualified as S

import Brick.Widgets.List (list, listClear, listModify, listMoveBy, listSelectedElement)
import Data.List.NonEmpty qualified as NE
import Data.Sequence qualified as Seq (fromList)
import Lcns.FileInfo (getFileInfo, isDir, nameOf, savedDir, symlinked)
import Lcns.FileMap (addFileOverride, addRawFile, lookupId, getIdOrCrash)
import Lcns.ListUtils qualified as LU
import Lcns.Path qualified as Path
import Relude.Extra (head1)
import Control.Exception (PatternMatchFail(..))

-- | a shorthand for `#dir % cur` that seems to appear everywhere
curDirId :: Lens' AppState FileId
curDirId = #dir % cur

-- focuses the current DirNode in an AppState
curDir :: Lens' AppState DirNode
curDir = lens getElem setElem
 where
  getElem s = case s.fileMap ^? savedDir (s ^. curDirId) of
    Nothing -> bug $ PatternMatchFail "current directory is not in FileMap (or not a SavedDir)"
    Just dirNode -> dirNode
  setElem s dirNode = s & #fileMap % savedDir (s ^. curDirId) .~ dirNode

-- why on earth isn't NonEmpty an instance of Cons?
cur :: Lens' DirHistory FileId
cur = lens getL setL
 where
  getL = head1
  setL (_ :| xs) x = x :| xs

parent :: AffineTraversal' AppState FileId
parent = #dir % ix 1

parentDir :: AffineTraversal' AppState DirNode
parentDir = atraversal tryGet update where
  tryGet s = maybe (Left s) Right do
    parentId <- s ^? parent
    s.fileMap ^? savedDir parentId
  update s newDir = fromMaybe s do
    parentId <- s ^? parent
    pure $ s & #fileMap % savedDir parentId .~ newDir
-- this feels like boilerplate

-- listSelectedElementL is a Traversal rather than AffineTraversal, and there's no easy way to downcast
childOrLink :: AffineTraversal' DirNode FileId
childOrLink = #files % atraversal tryGet update
 where
  tryGet l = 
    listSelectedElement l & maybe (Left l) (Right <. snd)
  update l item = l & listModify (const item)

childLens :: (FileId -> AffineTraversal' FileMap a) -> AffineTraversal' AppState a
childLens optic = atraversal tryGet update where
  tryGet s = maybe (Left s) Right $ do
    dir <- s ^? curDir
    childId' <- dir ^? childOrLink
    s ^? #fileMap % optic childId'

  update s item = fromMaybe s do
    dir <- s ^? curDir
    childId' <- dir ^? childOrLink
    pure $ s & #fileMap % optic childId' .~ item

-- | focuses the selected dir (if any) in current dir, traversing through symlinks
childDir :: AffineTraversal' AppState DirNode
childDir = childLens savedDir

-- | focuses the selected file in current dir, traversing through symlinks
child :: AffineTraversal' AppState FileInfo
child = childLens symlinked
  

childId :: AffineTraversal' AppState FileId
childId = curDir % childOrLink

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
  dir <- use #dir
  case NE.uncons dir of
    (_, Nothing) -> pass
    (newChildId, Just newHistory) -> do
      #dir .= newHistory
      curDir % childOrLink .= newChildId
      useOrPass parent refreshSavedDir
      useOrPass curDirId refreshSavedDir

goRight :: AppM ()
goRight = do
  fileMap <- use #fileMap
  useOrPass childId \childId ->
    if isDir fileMap childId
      then do
        #dir %= NE.cons childId
        refreshSelected
      else pass

refreshSelected' :: (MonadIO m, MonadState AppState m) => Bool -> m ()
refreshSelected' forceRefresh = do
  useOrPass childId (refreshSavedDir' forceRefresh)
  fileMap <- use #fileMap
  useOrPass childId \childId -> case fileMap ^? ix childId of
    Just Dir{path} -> void $ addDirNode (dirBuilder path)
    -- note that `child` contains `symlinked`, so this does not remove link nesting
    Just File{path, contents} -> do
      newContents <- case contents of
        Nothing -> tryJust $ decodeUtf8 <$> readFileBS path
        justContents -> pure justContents
      #fileMap % ix childId % #contents .= newContents
    _ -> pass

refreshSelected :: (MonadIO m, MonadState AppState m) => m ()
refreshSelected = refreshSelected' False

refreshSavedDir' :: (MonadIO m, MonadState AppState m) => Bool -> FileId -> m ()
refreshSavedDir' forceRefresh dirId = do
  fileMap <- use #fileMap
  let (Just dir) = fileMap ^? savedDir dirId -- I'm sorry...
  tryGetModTime dir.path >>= \case
    Nothing -> #fileMap % savedDir dirId % #files %= listClear
    Just modTime
      | dir.modTime >= modTime && not forceRefresh -> pass
      | otherwise ->
          void $
            addDirNode
              DirBuilder
                { path = dir.path
                , maybeModTime = Just modTime
                , prevSelection = dir ^? childOrLink
                }

{- | rebuild the file list of a DirTree

TODO: if the old file list contained `SavedDir`s, don't throw away their saved selections
-}
refreshSavedDir :: (MonadIO m, MonadState AppState m) => FileId -> m ()
refreshSavedDir = refreshSavedDir' False

-- | builds a new DirNode and adds it to the FileMap
addDirNode :: (MonadIO m, MonadState s m, AlmostAppState s) => DirBuilder -> m FileId
addDirNode DirBuilder{..} = do
  rawFileList <- mapM getFileInfo =<< tryListDirectory path
  modifying fileMapL $ applyAll (addRawFile <$> rawFileList)
  modTime <- whenNothing maybeModTime (getModificationTime path) -- TODO: handle inacessible dirs

  -- fourmolu, let me add a line break
  fileMap <- use fileMapL
  sortf <- use sortFunctionL
  let fileList = mapMaybe ((.path) .> lookupId fileMap) rawFileList
  let sortDir = sortBy $ S.cmpWith fileMap sortf
  let isDotfile fid = fileMap ^? ix fid % to nameOf % to decode % _head == Just '.'
  let filterHidden
        | sortf.showDotfiles = id
        | otherwise = filter (not <. isDotfile)

  let files =
        list (decode path) (Seq.fromList $ filterHidden $ sortDir fileList) 1
          & LU.selectMaybe prevSelection

  modifying fileMapL $ addFileOverride (SavedDir DirNode{..})
  updatedMap <- use fileMapL
  let dirId = getIdOrCrash updatedMap path
  pure dirId

buildDirHistory :: (MonadIO m, MonadState s m, AlmostAppState s) => Path Abs -> m DirHistory
buildDirHistory path = do
  let (_, nonRootChunks) = Path.splitDirectories path
  let paths = NE.reverse $ (Path.root </>) <. joinPath <$> NE.inits nonRootChunks
  traverse addDirNode' paths
 where
  addDirNode' path' = addDirNode (dirBuilder path')