module Lcns.Recompilation (recompileAndRun) where

import Lcns.Prelude

import Data.Time (UTCTime)
import System.Directory
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Posix (executeFile)
import System.Process (createProcess, cwd, proc, waitForProcess)

data Dirs = Dirs
  { cache :: FilePath
  , config :: FilePath
  }

recompileAndRun' :: Bool -> IO ()
recompileAndRun' forceRecompile = do
  cache <- getXdgDirectory XdgCache "lcns"
  createDirectoryIfMissing True cache
  config <- getXdgDirectory XdgConfig "lcns"

  ifM
    ((forceRecompile ||) <$> shouldRecompile Dirs{..})
    ( do
        putTextLn "Recompiling lcns..."
        exitCode <- compile Dirs{..}
        guard $ exitCode == ExitSuccess
    )
    (putTextLn "Recompilation is not needed")

  executeFile (cache </> "lcns") False [] Nothing

recompileAndRun :: IO ()
recompileAndRun = recompileAndRun' False

compile :: Dirs -> IO ExitCode
compile dirs = do
  (_, _, _, ph) <-
    createProcess
      ( proc
          "ghc"
          [ "--make"
          , dirs.config </> "lcns.hs"
          , "-i"
          , "-ilib"
          , "-fforce-recomp"
          , "-threaded"
          , "-rtsopts"
          , "-with-rtsopts=-N"
          , "-main-is"
          , "main"
          , "-v0"
          , "-outputdir"
          , dirs.cache </> "build"
          , "-o"
          , dirs.cache </> "lcns"
          ]
      )
        { cwd = Just dirs.config
        }
  waitForProcess ph

shouldRecompile :: Dirs -> IO Bool
shouldRecompile dirs = do
  binModTime <- getModTime $ dirs.cache </> "lcns"
  configModTime <- getModTime $ dirs.config </> "lcns.hs"
  pure $ fromMaybe True $ (<) <$> binModTime <*> configModTime

getModTime :: FilePath -> IO (Maybe UTCTime)
getModTime path =
  tryJust $ getModificationTime path
