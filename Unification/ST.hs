module Unification.ST
  ( STVar(..)
  , stKernel
  , unifyST
  ) where

import Data.Void
import Control.Monad.ST
import Data.STRef
import Unification.Class
import Unification.Kernel
import Unification.Exception

newtype STVar s f = STVar (STRef s (Maybe (Free f (STVar s f))))

stKernel :: Unifiable f => Kernel (ST s) (STVar s) f
stKernel = Kernel
  { newVar   = STVar <$> newSTRef
  , readVar  = \(STVar v)   -> readSTRef v
  , writeVar = \(STVar v) t -> writeSTRef v (Just t)
  , eqVar    = (==)
  , occurs   = \ v f -> vacuousM $ unsafeIOtoST $ throwIO $ Occurs v f
  , mismatch = \ f g -> vacuousM $ unsafeIOtoST $ throwIO $ Mismatch f g
  }

unifyST :: Unifiable f =>
  Free f (STVar s f) -> Free f (STVar s f) -> ST s (Free f (STVar s f))
unifyST = unifyWith stKernel
