{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Unification.Exception
  ( Occurs(..)
  , Mismatch(..)
  ) where

import Data.Typeable
import Control.Exception
import Control.Monad.Free
import Prelude.Extras

data Occurs = Occurs deriving (Typeable, Show)
data Mismatch = Mismatch deriving (Typeable, Show)

instance Exception Occurs
instance Exception Mismatch
