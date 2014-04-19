module Control.Unification.ST
  ( STVar(..)
  , stKernel
  , unifyST
  ) where

import Control.Monad.ST
import Control.Unification.Class
import Control.Unification.Kernel
import Control.Unification.Exception
import Data.Void
import Data.STRef

newtype STVar s f = STVar (STRef s (Maybe (Free f (STVar s f))))

stKernel :: Unifiable f => Kernel (ST s) (STVar s) f
stKernel = Kernel
  { newVar   = STVar <$> newSTRef
  , readVar  = \(STVar v)   -> readSTRef v
  , writeVar = \(STVar v) t -> writeSTRef v (Just t)
  , eqVar    = (==)
  , occurs   = unsafeIOtoST $ throwIO Occurs
  , mismatch = unsafeIOtoST $ throwIO Mismatch
  }

unifyST :: Unifiable f =>
  Free f (STVar s f) -> Free f (STVar s f) -> ST s (Free f (STVar s f))
unifyST = unifyWith stKernel
