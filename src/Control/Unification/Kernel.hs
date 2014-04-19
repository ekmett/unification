{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
module Control.Unification.Kernel
  ( Kernel(..)
  , unify
  , zonk
  ) where

import Control.Applicative
import Control.Monad.Free
import Control.Monad (join)
import Control.Unification.Class
import Data.Foldable as F
import Data.Traversable

-- Tim Sheard's generic unification via two level types

data Kernel (m :: * -> *) (v :: (* -> *) -> *) (f :: * -> *) = Kernel
  { newVar   :: m (v f)
  , readVar  :: v f -> m (Maybe (Free f (v f)))
  , writeVar :: v f -> Free f (v f) -> m ()
  , eqVar    :: v f -> v f -> Bool
  , occurs   :: forall a. m a
  , mismatch :: forall a. m a
  }

unifyVar :: (Monad m, Alternative m, Unified f) => Kernel m v f -> v f -> Free f (v f) -> m (Free f (v f))
unifyVar k a x = readVar k a >>= \mt -> case mt of
  Nothing -> do
    x' <- zonk k x
    if F.any (eqVar k a) x' then occurs k
                            else x' <$ writeVar k a x'
  Just y -> do
     y' <- unify k x y
     y' <$ writeVar k a y'

unify :: (Monad m, Alternative m, Unified f) => Kernel m v f -> Free f (v f) -> Free f (v f) -> m (Free f (v f))
unify k t@(Pure v) (Pure u) | eqVar k v u = return t
unify k (Pure a) y          = unifyVar k a y
unify k x (Pure a)          = unifyVar k a x
unify k (Free xs) (Free ys) = Free <$> unified (unify k) xs ys
                          <|> mismatch k

-- | Eliminate substitutions
zonk :: (Monad m, Applicative m, Traversable f) => Kernel m v f -> Free f (v f) -> m (Free f (v f))
zonk k = fmap join . traverse go where
  go v = readVar k v >>= \mv -> case mv of
    Nothing -> return $ Pure v
    Just t -> do
      t' <- zonk k t
      t' <$ writeVar k v t' -- path compression
