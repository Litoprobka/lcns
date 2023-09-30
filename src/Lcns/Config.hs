module Lcns.Config (Config (..)) where

import Data.Default

data Config = Config
  { placeholder :: ()
  }

instance Default Config where
  def = Config ()
