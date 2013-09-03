module Unification.Class
  ( Unifiable(..)
  ) where

import Data.Traversable

class Traversable f => Unifiable f where
  matchWith :: (a -> b -> c) -> f a -> f b -> Maybe (f c)

instance Unifiable [] where
  matchWith f = go where
    go (x:xs) (y:ys) = fmap (f x y :) (go xs ys)
    go []     []     = Just []
    go _      _      = Nothing

instance Unifiable Maybe where
  matchWith f (Just a) (Just b) = Just (Just (f a b))
  matchWith _ Nothing  Nothing  = Just Nothing
  matchWith _ _        _        = Nothing
