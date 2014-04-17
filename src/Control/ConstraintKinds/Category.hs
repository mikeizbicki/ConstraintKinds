module Control.ConstraintKinds.Category
    where

import GHC.Prim
import qualified Prelude as P

import qualified Data.Set as Set

-------------------------------------------------------------------------------
-- 

class Category cat where

    type ValidCategory cat a b :: Constraint
    type ValidCategory cat a b = ()

    id :: ValidCategory cat a a => cat a a

    (.) :: 
        ( ValidCategory cat b c
        , ValidCategory cat a b
        , ValidCategory cat a c
        ) => cat b c -> cat a b -> cat a c

---------------------------------------

class SubCategory cat subcat where
    embed :: ValidCategory subcat a b => subcat a b -> cat a b
    unsafeRestrict :: ValidCategory subcat a b => cat a b -> subcat a b

instance SubCategory a a where
    embed = id
    unsafeRestrict = id

---------------------------------------

class Functor cat f where
    fmap :: ValidCategory cat a b => cat a b -> f a -> f b

class Pointed f where
    point :: a -> f a

pure :: Pointed f => a -> f a
pure = point

return :: Pointed f => a -> f a
return = point

class (Functor cat f, Pointed f) => Applicative cat f where
    (<*>) :: ValidCategory cat a b => f (cat a b) -> f a -> f b 

class TypeMonoid m a where
    join :: m (m a) -> m a

class Applicative cat m => Monad cat m where

    (>>=) :: (TypeMonoid m b, ValidCategory cat a (m b)) => m a -> cat a (m b) -> m b
    a >>= f = join $ fmap f a

-------------------------------------------------------------------------------

instance Category (->) where
    id = P.id
    (.) = (P..)

($) :: (SubCategory (->) subcat, ValidCategory subcat a b) => subcat a b -> a -> b
($) = embed

-------------------------------------------------------------------------------

newtype Constrained (xs :: [* -> Constraint]) a b = Constrained (a -> b)

type family AppConstraints (f :: [* -> Constraint]) (a :: *) :: Constraint
type instance AppConstraints '[] a = ()
type instance AppConstraints (x ': xs) a = (x a, AppConstraints xs a)

instance Category (Constrained xs) where
    type ValidCategory (Constrained xs) a b = (AppConstraints xs a, AppConstraints xs b)
    id = Constrained P.id
    (Constrained f).(Constrained g) = Constrained (f.g)

instance SubCategory (->) (Constrained xs) where
    embed (Constrained f) = f
    unsafeRestrict = Constrained

embedOrd :: 
    ( SubCategory (Constrained '[P.Ord]) subcat
    , ValidCategory subcat a b
    ) => subcat a b -> Constrained '[P.Ord] a b
embedOrd = embed

restrictOrd ::
    ( SubCategory cat (Constrained '[P.Ord])
    , P.Ord a
    , P.Ord b
    ) => cat a b -> Constrained '[P.Ord] a b
restrictOrd = unsafeRestrict

-------------------

instance Functor (Constrained '[P.Ord]) Set.Set where
    fmap f set =  Set.map (embed f) set

instance Applicative (Constrained '[P.Ord]) Set.Set where
    fs <*> xs = Set.unions [ fmap f xs | f <- Set.toList fs ]

instance Monad (Constrained '[P.Ord]) Set.Set where

-------------------------------------------------------------------------------

newtype Monotonic a b = Monotonic (a -> b)

instance Category Monotonic where
    type ValidCategory Monotonic a b = (P.Ord a, P.Ord b)
    id = Monotonic P.id
    (Monotonic f) . (Monotonic g) = Monotonic (f.g)

instance SubCategory (->) Monotonic where
    embed (Monotonic f) = f
    unsafeRestrict = Monotonic

instance SubCategory (Constrained '[P.Ord]) Monotonic where
    embed (Monotonic f) = Constrained f
    unsafeRestrict (Constrained f) = Monotonic f

embedMonotonic :: 
    ( SubCategory Monotonic subcat
    , ValidCategory subcat a b
    ) => subcat a b -> Monotonic a b
embedMonotonic = embed

unsafeRestrictMonotonic ::
    ( SubCategory cat Monotonic
    , ValidCategory Monotonic a b
    ) => cat a b -> Monotonic a b
unsafeRestrictMonotonic = unsafeRestrict

-------------------

instance Functor Monotonic Set.Set where
    fmap f set =  Set.mapMonotonic (embed f) set

instance Pointed Set.Set where
    point = Set.singleton

instance Applicative Monotonic Set.Set where
    fs <*> xs = Set.unions [ fmap f xs | f <- Set.toList fs ]

instance P.Ord a => TypeMonoid Set.Set a where
    join set = Set.unions $ Set.toList set

instance Monad Monotonic Set.Set where
