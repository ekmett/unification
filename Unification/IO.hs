module Unification.IO
  ( ioKernel
  , unifyIO
  , IOVar(..)
  ) where

import Data.Void
import Data.IORef
import Control.Exception
import Control.Monad.Free
import Unification.Class
import Unification.Exception
import Unification.Kernel

newtype IOVar f = IOVar (IORef (Maybe (Free f (IOVar f))))

ioKernel :: Unifiable f => Kernel IO IOVar f
ioKernel = Kernel
  { newVar   = IOVar <$> newIORef
  , readVar  = \(IOVar v)   -> readIORef v
  , writeVar = \(IOVar v) t -> writeIORef v (Just t)
  , eqVar    = (==)
  , occurs   = \ v f -> throwIO (Occurs v f)
  , mismatch = \ f g -> throwIO (Mismatch f g)
  }

unifyIO :: Unifiable f => Free f (IOVar f) -> Free f (IOVar f) -> IO (Free f (IOVar f))
unifyIO = unifyWith ioKernel
