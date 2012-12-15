{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Control.ConstraintKinds.Applicative
    where

import GHC.Prim

import qualified Control.Monad as M
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as G

import Control.ConstraintKinds.Functor
import Prelude hiding (Functor, fmap, foldl, foldr)

-------------------------------------------------------------------------------
-- class Applicative

class Functor f => Applicative f where
    type ApplicativeConstraint f x :: Constraint
    type ApplicativeConstraint f x = ()

    pure :: (ApplicativeConstraint f a) => a -> f a

    (<*>) :: (ApplicativeConstraint f a, ApplicativeConstraint f b) => f (a -> b) -> f a -> f b

--     (*>) :: (FunctorConstraint f a, FunctorConstraint f (b -> c)) => f a -> f b -> f b
--     (*>) = liftA2 (const id)
-- 
--     (<*) :: f a -> f b -> f a
--     (<*) = liftA2 const

-- | Lift a function to actions.
-- This function may be used as a value for `fmap` in a `Functor` instance.
-- liftA :: (ApplicativeConstraint f a, ApplicativeConstraint f (a -> b), ApplicativeConstraint f b, Applicative f) => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

-- | Lift a binary function to actions.
-- liftA2 :: (FunctorConstraint f a, FunctorConstraint f (b -> c), Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

-- | Lift a ternary function to actions.
-- liftA3 :: (FunctorConstraint f a, FunctorConstraint f (b -> c -> d), Applicative f)=> (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = f <$> a <*> b <*> c

-------------------------------------------------------------------------------
-- Instances

instance Applicative [] where
    pure = M.return
    (<*>) = M.ap
    
instance Applicative V.Vector where
    {-# INLINE pure #-}
    pure = V.singleton

    {-# INLINE (<*>) #-}
    (<*>) = M.ap
  
instance Applicative VU.Vector where
    type ApplicativeConstraint VU.Vector x = VU.Unbox x

    {-# INLINE pure #-}
    pure = VU.singleton

    {-# INLINE (<*>) #-}
    (<*>) = undefined -- M.ap
