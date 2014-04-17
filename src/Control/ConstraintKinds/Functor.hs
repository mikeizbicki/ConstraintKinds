{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Control.ConstraintKinds.Functor
    where

import GHC.Prim
import Text.ParserCombinators.ReadPrec

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
    type ConstraintMapDomain f x :: Constraint
    type ConstraintMapDomain f x = ()
    
    type ConstraintMapRange f x :: Constraint
    type ConstraintMapRange f x = ()

    fmap :: 
        ( ConstraintMapDomain f a
        , ConstraintMapRange f b
        ) => (a -> b) -> f a -> f b

-- | An infix synonym for 'fmap'.
(<$>) :: 
    ( Functor f
    , ConstraintMapDomain f a
    , ConstraintMapRange f b
    ) => (a -> b) -> f a -> f b
(<$>) = fmap

-------------------------------------------------------------------------------
-- Instances

-- instance Functor [] where
--     {-# INLINE fmap #-}
--     fmap = P.map
-- 
-- instance Functor ReadPrec where
--     {-# INLINE fmap #-}
--     fmap = P.fmap
-- 
-- instance Functor V.Vector where
--     {-# INLINE fmap #-}
--     fmap = V.map
-- 
-- instance Functor VU.Vector where
--     type ConstraintMapDomain VU.Vector x = VU.Unbox x
--     type ConstraintMapRange VU.Vector x = VU.Unbox x
--     {-# INLINE fmap #-}
--     fmap = VU.map
