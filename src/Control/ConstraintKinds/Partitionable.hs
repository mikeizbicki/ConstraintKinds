{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Control.ConstraintKinds.Partitionable
    where

import GHC.Prim

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as G
-- import qualified Prelude as P
-- import Prelude (($),fromIntegral,(+),(-),(/),(*))

class Partitionable t where
    type PartitionableConstraint t x :: Constraint
    type PartitionableConstraint t x = ()

    partition :: (PartitionableConstraint t a) => Int -> t a -> [t a]

instance Partitionable [] where
    partition n xs = [map snd $ filter (\(i,x)->i `mod` n==j) ixs | j<-[0..n-1]]
        where
            ixs = addIndex 0 xs
            addIndex i [] = []
            addIndex i (x:xs) = (i,x):(addIndex (i+1) xs)
                

instance Partitionable V.Vector where
    partition n vec = go 0
        where
            go i = if i>=V.length vec
                then []
                else (V.slice i len vec):(go $ i+lenmax)
                where
                    len = if i+lenmax >= V.length vec
                        then (V.length vec)-i
                        else lenmax
                    lenmax = ceiling $ (fromIntegral $ V.length vec) / (fromIntegral n :: Rational)

instance Partitionable VU.Vector where
    type PartitionableConstraint VU.Vector x = VU.Unbox x
    partition n vec = go 0
        where
            go i = if i>=VU.length vec
                then []
                else (VU.slice i len vec):(go $ i+lenmax)
                where
                    len = if i+lenmax >= VU.length vec
                        then (VU.length vec)-i
                        else lenmax
                    lenmax = ceiling $ (fromIntegral $ VU.length vec) / (fromIntegral n)
