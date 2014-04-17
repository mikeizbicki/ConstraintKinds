{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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

    type ConstraintPure f a :: Constraint
    type ConstraintPure f a = ()

    pure :: 
        ( ConstraintPure f a
        ) => a -> f a

    type ConstraintApplyDomain f x :: Constraint
    type ConstraintApplyDomain f x = () 

    type ConstraintApplyRange f x :: Constraint
    type ConstraintApplyRange f x = () 

    (<*>) :: 
        ( ConstraintApplyDomain f a
        , ConstraintApplyRange f b
        ) => f (a -> b) -> f a -> f b

    (*>) :: 
        ( ConstraintApplyDomain f b
        , ConstraintApplyRange f b
        , ConstraintMapDomain f a
        , ConstraintMapRange f (b -> b)
        , Applicative f
        ) => f a -> f b -> f b
    (*>) = liftA2 (const id)

    (<*) :: 
        ( ConstraintApplyDomain f b
        , ConstraintApplyRange f a
        , ConstraintMapDomain f a
        , ConstraintMapRange f (b -> a)
        , Applicative f
        ) => f a -> f b -> f a
    (<*) = liftA2 const

-- | Lift a function to actions.
-- This function may be used as a value for `fmap` in a `Functor` instance.
liftA :: 
    ( ConstraintApplyDomain f a
    , ConstraintPure f (a -> b)
    , ConstraintApplyRange f b
    , Applicative f
    ) => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

-- | Lift a binary function to actions.
liftA2 ::
    ( ConstraintApplyDomain f b
    , ConstraintApplyRange f c
    , ConstraintMapDomain f a
    , ConstraintMapRange f (b -> c)
    , Applicative f
    ) => (a -> b -> c) -> f a -> f b -> f c 
liftA2 f a b = f <$> a <*> b

-------------------------------------------------------------------------------
-- Instances

instance Applicative [] where
    
    {-# INLINE pure #-}
    pure = return

    {-# INLINE (<*>) #-}
    (<*>) = M.ap
    
instance Applicative V.Vector where
    {-# INLINE pure #-}
    pure = return
    
    {-# INLINE (<*>) #-}
    (<*>) = M.ap
  
-- instance Applicative VU.Vector where
--     {-# INLINE (<*>) #-}
--     (<*>) = undefined -- M.ap
