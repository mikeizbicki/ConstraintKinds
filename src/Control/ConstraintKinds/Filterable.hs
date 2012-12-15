{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Control.ConstraintKinds.Filterable
    where

import GHC.Prim

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as G
import qualified Prelude as P

class Filterable f where
    type FilterableConstraint f x :: Constraint
    type FilterableConstraint f x = ()

    filter :: (FilterableConstraint f a) => (a -> P.Bool) -> f a -> f a
    ifilter :: (FilterableConstraint f a) => (P.Int -> a -> P.Bool) -> f a -> f a

instance Filterable [] where
    filter = L.filter
    ifilter cond xs = go 0 xs
        where
            go n [] = []
            go n (x:xs) = if cond n x
                then x:(go (n P.+ 1) xs)
                else go (n P.+ 1) xs
    
instance Filterable V.Vector where
    filter = V.filter
    ifilter = V.ifilter
    
instance Filterable VU.Vector where
    type FilterableConstraint VU.Vector x = VU.Unbox x    
    filter = VU.filter
    ifilter = VU.ifilter