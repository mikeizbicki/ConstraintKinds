{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.ConstraintKinds.Monad
    where

import GHC.Prim
import qualified Control.Monad as Monad
import Prelude hiding (Functor, fmap, Monad, (>>=), (>>), return)
import qualified Prelude as Prelude

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG

import Control.ConstraintKinds.Functor
import Control.ConstraintKinds.Pointed
import Control.ConstraintKinds.Applicative

-------------------------------------------------------------------------------
-- class Monad

class (Functor m) => Monad m where

    return :: 
        ( FunctorConstraint m a
        ) => a -> m a

    join :: 
        ( FunctorConstraint m a
        , FunctorConstraint m (m a)
        ) => m (m a) -> m a
    join m = m >>= id

    (>>=) :: 
        ( FunctorConstraint m a
        , FunctorConstraint m b
        , FunctorConstraint m (m b)
        ) => m a -> (a -> m b) -> m b
    a >>= b = join $ fmap b a

    (>>) :: 
        ( FunctorConstraint m a
        , FunctorConstraint m b
        , FunctorConstraint m (m b)
        ) => m a -> m b -> m b
    m >> k = m >>= \_ -> k
    
-------------------------------------------------------------------------------
-- instances

instance Control.ConstraintKinds.Monad.Monad [] where
    return = Monad.return
    (>>=)  = (Monad.>>=)
    (>>)   = (Monad.>>)

instance Monad V.Vector where
    return = Monad.return
    (>>=) = flip V.concatMap
  
instance Monad VU.Vector where
    return = VU.singleton
    (>>=) = flip VU.concatMap
