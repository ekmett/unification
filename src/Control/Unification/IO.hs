{-# LANGUAGE DeriveDataTypeable #-}
module Control.Unification.IO
  ( ioKernel
  , unifyIO
  , IOVar(..)
  ) where

import Data.Functor
import Data.IORef
import Control.Exception
import Control.Monad.Error
import Control.Monad.Free
import Control.Unification.Class
import Control.Unification.Exception
import Control.Unification.Kernel
import Data.Typeable

newtype IOVar f = IOVar (IORef (Maybe (Free f (IOVar f))))
  deriving (Eq, Typeable)

ioKernel :: Unified f => Kernel IO IOVar f
ioKernel = Kernel
  { newVar   = IOVar <$> newIORef Nothing
  , readVar  = \(IOVar v)   -> readIORef v
  , writeVar = \(IOVar v) t -> writeIORef v (Just t)
  , eqVar    = (==)
  , occurs   = throwIO Occurs
  , mismatch = throwIO Mismatch
  }

unifyIO :: (Typeable1 f, Unified f) => Free f (IOVar f) -> Free f (IOVar f) -> IO (Free f (IOVar f))
unifyIO = unify ioKernel
