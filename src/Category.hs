{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Category where

import Category.Start qualified as Star
import Control.Category (Category, id, (.))
import Data.Dict (Dict (Dict), (:-) (Sub), (\\))
import Data.Kind (Type)

-- | An unenriched hom.
type family (~>) :: i -> i -> Type
type instance (~>) = (->)
type instance (~>) = (:-)

type Dom (a :: i) = (~>) :: i -> i -> Type

-- | An covariant endofunctor in an unenriched category.
class Category cat => CovariantEndo cat f where
    coendomap :: cat a b -> cat (f a) (f b)

instance {-# OVERLAPPABLE #-} Star.Functor f => CovariantEndo (->) f where
    coendomap = Star.fmap

-- | A contravariant endofunctor in an unenriched category.
class Category cat => ContravariantEndo cat f where
    contraendomap :: cat b a -> cat (f a) (f b)

instance Star.Contravariant f => ContravariantEndo (->) f where
    contraendomap = Star.contramap

-- | An invariant endofunctor (if that's even considered a functor) in an unenriched category.
class Category cat => InvariantEndo cat f where
    invendomap :: cat a b -> cat b a -> cat (f a) (f b)

instance {-# OVERLAPPABLE #-} Star.Invariant f => InvariantEndo (->) f where
    invendomap = Star.invmap

-- | A bi-endofunctor in an unenriched category covariant in both arguments.
class Category cat => BiEndo cat f where
    biendomap :: cat a c -> cat b d -> cat (f a b) (f c d)

instance {-# OVERLAPPABLE #-} Star.Bifunctor f => BiEndo (->) f where
    biendomap = Star.bimap

-- | A pro-endofunctor in an unenriched category (contravariant in the left argument, covariant in the right).
class Category cat => ProEndo cat f where
    diendomap :: cat c a -> cat b d -> cat (f a b) (f c d)

instance {-# OVERLAPPABLE #-} Star.Profunctor f => ProEndo (->) f where
    diendomap = Star.dimap

-- | A covariant functor between unenriched categories.
class (Category dom, Category cod) => Covariant dom cod f where
    comap :: dom a b -> cod (f a) (f b)

instance {-# OVERLAPPABLE #-} CovariantEndo cat f => Covariant cat cat f where
    comap = coendomap

-- | A contravariant functor between unenriched categories.
class (Category dom, Category cod) => Contravariant dom cod f where
    contramap :: dom b a -> cod (f a) (f b)

instance {-# OVERLAPPABLE #-} ContravariantEndo cat f => Contravariant cat cat f where
    contramap = contraendomap

-- | An invariant functor (if that's even considered a functor) between unenriched categories.
class (Category dom, Category cod) => Invariant dom cod f where
    invmap :: dom a b -> dom b a -> cod (f a) (f b)

instance {-# OVERLAPPABLE #-} InvariantEndo cat f => Invariant cat cat f where
    invmap = invendomap

-- | A bifunctor in an unenriched category covariant in both arguments.
class (Category dom, Category cod) => Bi dom cod f where
    bimap :: dom a c -> dom b d -> cod (f a b) (f c d)

instance {-# OVERLAPPABLE #-} BiEndo cat f => Bi cat cat f where
    bimap = biendomap

-- | A profunctor in an unenriched category (contravariant in the left argument, covariant in the right).
class (Category dom, Category cod) => Pro dom cod f where
    dimap :: dom c a -> dom b d -> cod (f a b) (f c d)

instance {-# OVERLAPPABLE #-} ProEndo cat f => Pro cat cat f where
    dimap = diendomap

instance Pro (:-) (->) (:-) where
    dimap f g h = g . h . f

class Category cat => Monoidal (cat :: i -> i -> Type) where
    type Unit cat :: i
    type Product cat :: i -> i -> i
    monBi :: proxy cat -> Dict (BiEndo cat (Product cat))
    monIdLI :: a `cat` Product cat a (Unit cat)
    monIdRI :: a `cat` Product cat (Unit cat) a
    monIdLE :: Product cat a (Unit cat) `cat` a
    monIdRE :: Product cat (Unit cat) a `cat` a
    monAssocL :: Product cat (Product cat a b) c `cat` Product cat a (Product cat b c)
    monAssocR :: Product cat a (Product cat b c) `cat` Product cat (Product cat a b) c

instance Monoidal (->) where
    type Unit (->) = ()
    type Product (->) = (,)
    monBi _ = Dict
    monIdLI x = (x, ())
    monIdRI x = ((), x)
    monIdLE (x, ()) = x
    monIdRE ((), x) = x
    monAssocL ((x, y), z) = (x, (y, z))
    monAssocR (x, (y, z)) = ((x, y), z)

-- | A semigroup object in a monoidal unenriched category.
class Monoidal cat => Semigroup cat s where
    append :: Product cat s s `cat` s

instance {-# OVERLAPPABLE #-} Star.Semigroup s => Semigroup (->) s where
    append (x, y) = (Star.<>) x y

-- | A monoid object in a monoidal unenriched category.
class Semigroup cat s => Monoid cat s where
    empty :: Unit cat `cat` s

instance {-# OVERLAPPABLE #-} Star.Monoid s => Monoid (->) s where
    empty () = Star.mempty

-- | An applicative functor.
class (Monoidal cat, CovariantEndo cat f) => Applicative cat f where
    pure :: a `cat` f a
    ap :: Product cat (f (a `cat` b)) (f a) `cat` f b

instance {-# OVERLAPPABLE #-} Star.Applicative f => Applicative (->) f where
    pure = Star.pure
    ap (f, x) = (Star.<*>) f x

-- | A monoid object in the category of endofunctors in a monoidal unenriched category.
class Applicative cat m => Monad cat m where
    join :: m (m a) `cat` m a

instance {-# OVERLAPPABLE #-} Star.Monad m => Monad (->) m where
    join = Star.join

newtype Nat f g = Nat { runNat :: forall a. f a ~> g a }

type instance (~>) = Nat

type D (hom :: (i -> j) -> (i -> j) -> Type) = (~>) :: j -> j -> Type

instance (nat ~ Nat, Category (D nat)) => Category (nat :: (i -> j) -> (i -> j) -> Type) where
    id = Nat id
    Nat f . Nat g = Nat (f . g)

class AnyC a
instance AnyC a

class AnyC2 a b
instance AnyC2 a b
