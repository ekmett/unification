{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE BangPatterns #-}
module Control.Unification.Class
  ( Unified(unified)
  , GUnified(gunified)
  ) where

import Control.Applicative
import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Coproduct
import Data.Functor.Identity
import Data.Functor.Product
import Data.Traversable
import GHC.Generics

class Traversable f => Unified f where
  -- |
  -- @
  -- 'unified' ('const' f) a a = 'unified' ('const' . f) a a = 'traverse' f a
  -- @
  unified :: Alternative m => (a -> b -> m c) -> f a -> f b -> m (f c)
  default unified :: (Alternative m, Generic1 f, GUnified (Rep1 f)) => (a -> b -> m c) -> f a -> f b -> m (f c)
  unified f as bs = to1 <$> gunified f (from1 as) (from1 bs)

instance Eq m => Unified (Const m) where
  unified _ (Const m) (Const n)
    | m == n    = pure $ Const m
    | otherwise = empty

instance Eq m => Unified (Constant m) where
  unified _ (Constant m) (Constant n)
    | m == n    = pure $ Constant m
    | otherwise = empty

instance Unified f => Unified (Cofree f) where
  unified f (a :< as) (b :< bs) = (:<) <$> f a b <*> unified (unified f) as bs

instance Unified f => Unified (Free f) where
  unified f (Pure a)  (Pure b)  = Pure <$> f a b
  unified f (Free as) (Free bs) = Free <$> unified (unified f) as bs
  unified _ _         _         = empty

instance (Unified f, Unified g) => Unified (Product f g) where
  unified f (Pair a b) (Pair c d) = Pair <$> unified f a c <*> unified f b d

instance (Unified f, Unified g) => Unified (Coproduct f g) where
  unified f (Coproduct (Left  m)) (Coproduct (Left  n)) = Coproduct . Left  <$> unified f m n
  unified f (Coproduct (Right m)) (Coproduct (Right n)) = Coproduct . Right <$> unified f m n
  unified _ _                     _                     = empty

instance Unified Identity where
  unified f (Identity a) (Identity b) = Identity <$> f a b

instance (Unified f, Unified g) => Unified (Compose f g) where
  unified f (Compose m) (Compose n) = Compose <$> unified (unified f) m n

instance Unified [] where
  unified f = go where
    go (x:xs) (y:ys) = (:) <$> f x y <*> go xs ys
    go []     []     = pure []
    go _      _      = empty

instance Unified Maybe where
  unified f (Just a) (Just b) = Just <$> f a b
  unified _ Nothing  Nothing  = pure Nothing
  unified _ _        _        = empty

instance Eq a => Unified (Either a) where
  unified _ (Left a)  (Left b) | a == b = pure $ Left a
  unified f (Right a) (Right b)         = Right <$> f a b
  unified _ _         _                 = empty

class GUnified f where
  gunified :: Alternative m => (a -> b -> m c) -> f a -> f b -> m (f c)

instance GUnified V1 where
  gunified _ !_ !_ = empty -- you should bottom out before getting here

instance GUnified U1 where
  gunified _ U1 U1 = pure U1

instance GUnified Par1 where
  gunified f (Par1 a) (Par1 b) = Par1 <$> f a b

instance Unified f => GUnified (Rec1 f) where
  gunified f (Rec1 as) (Rec1 bs) = Rec1 <$> unified f as bs

instance Eq c => GUnified (K1 i c) where
  gunified _ (K1 a) (K1 b)
    | a == b    = pure (K1 a)
    | otherwise = empty

instance GUnified f => GUnified (M1 i c f) where
  gunified f (M1 as) (M1 bs) = M1 <$> gunified f as bs

instance (GUnified f, GUnified g) => GUnified (f :*: g) where
  gunified f (as :*: bs) (cs :*: ds) = (:*:) <$> gunified f as cs <*> gunified f bs ds

instance (GUnified f, GUnified g) => GUnified (f :+: g) where
  gunified f (L1 as) (L1 bs) = L1 <$> gunified f as bs
  gunified f (R1 as) (R1 bs) = R1 <$> gunified f as bs
  gunified _ _       _       = empty

instance (Unified f, GUnified g) => GUnified (f :.: g) where
  gunified f (Comp1 as) (Comp1 bs) = Comp1 <$> unified (gunified f) as bs
