{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.ConstraintKinds.Pointed
    where

import GHC.Prim

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as G
import qualified Prelude as P

import Control.ConstraintKinds.Functor

-------------------------------------------------------------------------------
-- class Functor

class (Functor f) => Pointed f where
    type PointedConstraint f x :: Constraint
    type PointedConstraint f x = FunctorConstraint f x
    
    point :: (PointedConstraint f a) => a -> f a

-------------------------------------------------------------------------------
-- Instances

instance Pointed [] where
    {-# INLINE point #-}
    point x = [x]

instance Pointed V.Vector where
    {-# INLINE point #-}
    point = V.singleton

instance Pointed VU.Vector where
    type PointedConstraint VU.Vector x = VU.Unbox x
    {-# INLINE point #-}
    point = VU.singleton
