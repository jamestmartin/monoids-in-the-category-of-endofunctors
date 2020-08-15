module Category.Functor where

import Category
import Category.Nat
import Data.Kind (Constraint)
import qualified Prelude (Functor, fmap)

-- | A functor `f` from category `src` to `dest`.
-- | This is *much more general* than Haskell's Functor, which is
-- | 1. An endofunctor, i.e. a functor from a category to itself (`Functor cat cat f`)
-- | 2. An endofunctor in the category of *types*, where cat can *only* be `AnyC :: * -> Constraint`.
class (Category src, Category dest) => Functor (src :: srcKind -> Constraint) (dest :: destKind -> Constraint) (f :: srcKind -> destKind) where
    map :: forall proxy1 proxy2 a b. (src a, src b, dest (f a), dest (f b)) => proxy1 src -> proxy2 dest -> (a ~> b) -> f a ~> f b

-- | A functor from a category to itself.
class Functor cat cat f => Endofunctor cat f
instance Functor cat cat f => Endofunctor cat f

-- | A restricted alias of `map` which does not cause type ambiguity.
(<$>) :: forall r s f a b. (r ~ AnyC, s ~ AnyC, Functor r s f) => (a ~> b) -> f a ~> f b
(<$>) = map (Proxy @r) (Proxy @s)

-- | A contravariant functor.
class (Category src, Category dest) => Contravariant (src :: srcKind -> Constraint) (dest :: destKind -> Constraint) (f :: srcKind -> destKind) where
    contramap :: forall proxy1 proxy2 a b. (src a, src b, dest (f a), dest (f b)) => proxy1 src -> proxy2 dest -> (b ~> a) -> (f a ~> f b)

-- | An invariant functor.
class (Category src, Category dest) => Invariant (src :: srcKind -> Constraint) (dest :: destKind -> Constraint) (f :: srcKind -> destKind) where
    invmap :: forall proxy1 proxy2 a b. (src a, src b, dest (f a), dest (f b)) => proxy1 src -> proxy2 dest -> (a ~> b) -> (b ~> a) -> (f a ~> f b)

{-
instance {-# INCOHERENT #-} Functor src dest f => Invariant src dest f where
    invmap f _ = map f

instance {-# INCOHERENT #-} Contravariant src dest f => Invariant src dest f where
    invmap _ f = contramap f
-}

instance {-# OVERLAPPABLE #-} Prelude.Functor f => Functor r s f where
    map _ _ = Prelude.fmap

instance (Category src, Category dest) => Functor src dest ((->) a) where
    map _ _ f g x = f (g x)

instance (Category src, Category dest) => Functor src dest ((,) a) where
    map _ _ f (x, y) = (x, f y)

instance (Category src, Category dest) => Functor src dest (,) where
    map _ _ f = Nat \(x, y) -> (f x, y)
