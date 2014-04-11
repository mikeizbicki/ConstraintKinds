module Control.ConstraintKinds.Monad.Trace
    where

import Data.Dynamic

import Prelude hiding (Functor, fmap, Monad, (>>=), (>>), return)
import qualified Prelude as Prelude

import Control.ConstraintKinds.Applicative
import Control.ConstraintKinds.Functor
import Control.ConstraintKinds.Monad

-------------------------------------------------------------------------------
-- data types

data Trace a
    = TraceCons Dynamic (Trace a)
    | TraceNil a

instance Functor Trace where
    type FunctorConstraint Trace a = Typeable a

    fmap f (TraceCons d t) = TraceCons d (fmap f t)
    fmap f (TraceNil a) = TraceCons (toDyn a) $ TraceNil (f a)
