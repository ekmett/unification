module Unification.Kernel
  ( Kernel(..)
  , unifyWith
  , zonkWith
  ) where

import Data.Void
import Control.Monad.Free
import Control.Monad (liftM)
import Unification.Class

-- Tim Sheard's generic unification via two level types

data Kernel m v f = Kernel
  { newVar   :: m (v f)
  , readVar  :: v f -> m (Maybe (Free f (v f)))
  , writeVar :: v f -> Free f (v f) -> m ()
  , eqVar    :: v f -> v f -> Bool
  , occurs   :: v f -> Free f (v f) -> m Void
  , mismatch :: f (Free f (v f)) -> f (Free f (v f)) -> m Void
  }

unifyVarWith :: (Monad m, Unifiable) =>
  Kernel m v f -> v f -> Free f (v f) -> m (Free f (v f))
unifyVarWith k a x = readVar k a >>= \mt -> case mt of
  Nothing -> do
    x' <- zonkWith k x
    if elem v x' then vacuousM $ occurs v x'
                 else writeVar k a x'
  Just x -> do
     x' <- unifyWith k t x
     writeVar k a x'
     return x'

unifyWith :: (Monad m, Unifiable f) =>
  Kernel m v f -> Free f (v f) -> Free f (v f) -> m (Free f (v f))
unifyWith k t@(Pure v) (Pure u) | v == u = return t
unifyWith k (Pure a) y = unifyVarWith k a y
unifyWith k x (Pure a) = unifyVarWith k a x
unifyWith k (Free xs) (Free ys) =
  maybe (vacuousM $ mismatch xs ys)
        (liftM Free . sequence)
        (matchWith (unifyWith k) xs ys)

-- | Eliminate substitutions
zonkWith :: (Monad m, Monad f, Traversable f) =>
  Kernel m v f -> Free f (v f) -> m (Free f (v f))
zonkWith k t = traverse go t where
  go v = readVar k v >>= \mv -> case mv of
    Nothing -> return (return v)
    Just t2 -> do
      t3 <- zonkWith k t2
      writeVar k v t3 -- path compression
      return t3
