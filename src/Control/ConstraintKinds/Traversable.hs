{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Control.ConstraintKinds.Traversable
    where

import GHC.Prim
import Control.Applicative
import Control.Monad hiding (Functor,fmap,mapM)
import Prelude hiding (Functor, fmap, mapM)
import qualified Prelude as P

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG

import Control.ConstraintKinds.Foldable
import Control.ConstraintKinds.Functor

-------------------------------------------------------------------------------
-- class Traversable

class (Functor t, Foldable t) => Traversable t where
    type TraversableConstraint t x :: Constraint
    type TraversableConstraint t x = ()

    traverse :: (TraversableConstraint t a, TraversableConstraint t b, FunctorConstraint t (f b), FunctorConstraint t a, Applicative f) 
        => (a -> f b) -> t a -> f (t b)
--     traverse f xs = sequenceA $ fmap f xs

    sequenceA :: (TraversableConstraint t a, TraversableConstraint t b, TraversableConstraint t (f a), FunctorConstraint t (f a), Applicative f) 
        => t (f a) -> f (t a)
    sequenceA = traverse id

    mapM :: (TraversableConstraint t a, TraversableConstraint t b, FunctorConstraint t a, FunctorConstraint t (WrappedMonad m b), Monad m) 
        => (a -> m b) -> t a -> m (t b)
--     mapM f = unwrapMonad . traverse (WrapMonad . f)

    sequence :: (TraversableConstraint t a, TraversableConstraint t b, TraversableConstraint t (m a), FunctorConstraint t (WrappedMonad m a), FunctorConstraint t (m a), Monad m) 
        => t (m a) -> m (t a)
    sequence = mapM id
    
-------------------------------------------------------------------------------
-- instances

instance Traversable [] where
    {-# INLINE traverse #-} -- so that traverse can fuse
    traverse f = Prelude.foldr cons_f (pure [])
      where cons_f x ys = (:) <$> f x <*> ys

    mapM = P.mapM

instance Traversable V.Vector where
    {-# INLINE traverse #-}
    traverse f xs = V.fromList <$> traverse f (V.toList xs)

    {-# INLINE mapM #-}
    mapM = V.mapM

    {-# INLINE sequence #-}
    sequence = V.sequence

instance Traversable VU.Vector where
    type TraversableConstraint VU.Vector x = VU.Unbox x

    {-# INLINE traverse #-}
    traverse f xs = VU.fromList <$> traverse f (VU.toList xs)

    {-# INLINE mapM #-}
    mapM = VU.mapM

    {-# INLINE sequence #-}
    sequence = VG.sequence
