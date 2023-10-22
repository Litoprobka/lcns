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
import Data.Default
import Graphics.Vty.Input (Key (..), Modifier (..))

instance Default Config where
  def = Config{keybindings}
   where
    keybindings = \cases
      (KChar 'q') [MCtrl] -> AppM $ lift halt -- this... is not nice
      KUp [] -> moveUp
      KDown [] -> moveDown
      KRight [] -> open
      KLeft [] -> moveBack
      KDel [] -> delete
      (KChar 'r') [MCtrl] -> invertSort
      (KChar 'd') [MCtrl] -> toggleDotfiles
      _ _ -> pass
