module Lcns.Recompilation (recompileAndRun) where

import           Relude

import           System.Directory
import           System.FilePath   ((</>))
import           System.Posix      (executeFile)
import           System.Process    (createProcess, proc, waitForProcess, cwd)
import System.Exit (ExitCode(ExitSuccess))


recompileAndRun :: IO ()
recompileAndRun = do
    cacheDir <- getXdgDirectory XdgCache "lcns"
    createDirectoryIfMissing True cacheDir
    configDir  <- getXdgDirectory XdgConfig "lcns"

    -- TODO: check if the program should be recompiled
    putTextLn "Recompiling lcns..."
    exitCode <- compile cacheDir configDir
    guard $ exitCode == ExitSuccess
    executeFile (cacheDir </> "lcns") False [] Nothing

    where
        compile cache configDir = do
            (_, _, _, ph) <- createProcess (proc "ghc"
                [ "--make"
                , configDir </> "lcns.hs"
                , "-i"
                , "-ilib"
                , "-fforce-recomp"
                , "-threaded"
                , "-rtsopts"
                , "-with-rtsopts=-N"
                , "-main-is", "main"
                , "-v0"
                , "-outputdir", cache </> "build"
                , "-o", cache </> "lcns"
                ]){ cwd = Just configDir }
            waitForProcess ph
