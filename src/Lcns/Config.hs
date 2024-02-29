{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lcns.Config (Config (..), module Lcns.EventHandling, Key (..), Modifier (..)) where

import Lcns.EventHandling hiding (
  handleAppEvent,
  handleEventWith,
  handleVtyEvent,
 )
import Lcns.Prelude

import Brick (halt)
import Config.Dyre.Relaunch
import Data.Default
import Graphics.Vty.Input (Key (..), Modifier (..))

quit :: AppM ()
quit = AppM $ lift halt

reload :: AppM ()
reload = AppM $ liftIO $ relaunchMaster Nothing

instance Default Config where
  def = Config{keybindings, tabSize = 4}
   where
    keybindings = \cases
      (KChar 'q') [] -> reload
      (KChar 'q') [MCtrl] -> quit
      KUp [] -> moveUp
      KDown [] -> moveDown
      KRight [] -> open
      KLeft [] -> moveBack
      KDel [] -> delete
      (KChar 'r') [MCtrl] -> invertSort
      (KChar 'd') [MCtrl] -> toggleDotfiles
      _ _ -> pass
