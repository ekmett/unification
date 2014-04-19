{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE DeriveDataTypeable #-}
#endif
module Control.Unification.Exception
  ( UnificationException(..)
  ) where

import Data.Typeable
import Control.Exception
import Control.Monad.Free

data UnificationException f a
  = Occurs a (Free f a)
  | Mismatch (f (Free f a)) (f (Free f a))
#if __GLASGOW_HASKELL__ >= 707
  deriving Typeable
#endif

newtype Showable = Showable (Int -> String -> String)

instance Show Showable where
  showsPrec d (Showable f) = f d

-- shown :: Show a => a -> Showable
-- shown a = Showable (\d -> showsPrec d a)

shownFree :: (Functor f, Show (f Showable), Show a) => Free f a -> Showable
shownFree a = Showable (\d -> showFree d a)

showFree :: (Functor f, Show (f Showable), Show a) => Int -> Free f a -> String -> String
showFree d (Pure a)  = showParen (d > 10) $ showString "Pure " . showsPrec 10 a
showFree d (Free as) = showParen (d > 10) $ showString "Free " . showsPrec 10 (fmap shownFree as)

instance (Functor f, Show (f Showable), Show a) => Show (UnificationException f a) where
  showsPrec d (Occurs a as) = showParen (d > 10) $
    showString "Occurs " . showsPrec 11 a . showChar ' ' . showFree 11 as
  showsPrec d (Mismatch x y) = showParen (d > 10) $
    showString "Mismatch " . showsPrec 11 (fmap shownFree x) . showChar ' ' . showsPrec 11 (fmap shownFree y)

#if __GLASGOW_HASKELL__ < 707

unificationExceptionTyCon :: TyCon
#ifdef MIN_VERSION_base(4,4,0)
unificationExceptionTyCon = mkTyCon3 "unification" "Unification.Exception" "UnificationException"
#else
unificationExceptionTyCon = mkTyCon "Unification.Exception.UnificationException"
#endif
{-# NOINLINE unificationExceptionTyCon #-}

instance Typeable1 f => Typeable1 (UnificationException f) where
  typeOf1 p = mkTyConApp unificationExceptionTyCon [typeOf (argOf p)]
    where argOf :: t f a -> f ()
          argOf _ = undefined
#else
#define Typeable1 Typeable
#endif

instance (Typeable1 f, Functor f, Show (f Showable), Typeable a, Show a) => Exception (UnificationException f a)
