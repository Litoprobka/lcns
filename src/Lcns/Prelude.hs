module Lcns.Prelude (
  module Relude,
  module Lcns.Types,
  module Optics,
  module Optics.Operators,
  module Optics.State.Operators,
  (<..),
  (.>),
  (<.),
  (<<&>>),
  applyWhen,
  applyIf,
  applyJust,
  flipOrder,
  onJust,
  io,
  eitherToMaybe,
) where

import Relude hiding (uncons)

import Lcns.Types

import Optics
import Optics.Operators
import Optics.State.Operators

infixr 9 <..

{-# INLINE (<..) #-}
(<..) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f <.. g) x y = f $ g x y

infixl 9 .>

{-# INLINE (.>) #-}
(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = g . f

infixr 9 <.

{-# INLINE (<.) #-}
(<.) :: (b -> c) -> (a -> b) -> a -> c
(<.) = (.)

infixl 1 <<&>>

-- | left-to-right double `fmap` (flipped version of `<<$>>`)
(<<&>>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<<&>>) = flip (<<$>>)

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f = f
applyWhen _ _ = id

applyIf :: (a -> Bool) -> (a -> a) -> a -> a
applyIf p f x = if p x then f x else x

applyJust :: (a -> Maybe b) -> (b -> a -> a) -> a -> a
applyJust f g x = case f x of
  Nothing -> x
  Just v -> g v x

flipOrder :: Ordering -> Ordering
flipOrder GT = LT
flipOrder LT = GT
flipOrder EQ = EQ

onJust :: Applicative f => (a -> f ()) -> Maybe a -> f ()
onJust = flip whenJust

io :: MonadIO m => IO a -> m a
io = liftIO

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = preview _Right