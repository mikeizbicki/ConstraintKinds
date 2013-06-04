{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.ConstraintKinds.Monad
    where

import GHC.Prim
import qualified Control.Monad as Monad
import Prelude hiding (Monad, (>>=), (>>), return)
import qualified Prelude as Prelude

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG

import Control.ConstraintKinds.Foldable
import Control.ConstraintKinds.Functor
import Control.ConstraintKinds.Pointed
import Control.ConstraintKinds.Applicative

-------------------------------------------------------------------------------
-- class Monad

class (Applicative m) => Monad m where
    type MonadConstraint m x :: Constraint
    type MonadConstraint m x = ApplicativeConstraint m x

    (>>=)       :: (MonadConstraint m a, MonadConstraint m b) => m a -> (a -> m b) -> m b
    (>>)        :: (MonadConstraint m a, MonadConstraint m b) => m a ->       m b  -> m b
    m >> k      = m >>= \_ -> k
    
-------------------------------------------------------------------------------
-- instances

instance Control.ConstraintKinds.Monad.Monad [] where
    (>>=)       = (Monad.>>=)
    (>>)        = (Monad.>>)

instance Monad V.Vector where
    {-# INLINE (>>=) #-}
    (>>=) = flip V.concatMap
  
-- instance Monad VU.Vector where
--     {-# INLINE (>>=) #-}
--     (>>=) = flip VU.concatMap
