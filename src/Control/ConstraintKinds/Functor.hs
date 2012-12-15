{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Control.ConstraintKinds.Functor
    where

import GHC.Prim

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as G
import qualified Prelude as P

-------------------------------------------------------------------------------
-- class Functor

class Functor f where
    type FunctorConstraint f x :: Constraint
    type FunctorConstraint f x = ()
    
    fmap :: (FunctorConstraint f a, FunctorConstraint f b) => (a -> b) -> f a -> f b

-- | An infix synonym for 'fmap'.
(<$>) :: (Functor f, FunctorConstraint f a, FunctorConstraint f b) => (a -> b) -> f a -> f b
(<$>) = fmap

-------------------------------------------------------------------------------
-- Instances

instance Functor [] where
    type FunctorConstraint [] x = ()
    {-# INLINE fmap #-}
    fmap = P.map

instance Functor V.Vector where
    {-# INLINE fmap #-}
    fmap = V.map

instance Functor VU.Vector where
    type FunctorConstraint VU.Vector x = VU.Unbox x
    {-# INLINE fmap #-}
    fmap = VU.map
