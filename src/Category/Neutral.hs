{-# LANGUAGE PolyKinds #-}
module Category.Neutral where

import Category.Good qualified as Good
import Data.Dict (Dict (Dict), (:-) (Sub), (\\))
import Data.Kind (Type)

-- | An unenriched hom.
type family (~>) :: i -> i -> Type
type instance (~>) = (->)
type instance (~>) = (:-)

type Dom (a :: i) = (~>) :: i -> i -> Type

-- | An unenriched category.
class Category cat where
    identity :: cat a a
    compose :: cat b c -> cat a b -> cat a c

instance Category (->) where
    identity x = x
    compose f g x = f (g x)

instance Category (:-) where
    identity = Sub Dict
    compose f g = Sub (Dict \\ f \\ g)

-- | An covariant endofunctor in an unenriched category.
class Category cat => CovariantEndo cat f where
    coendomap :: cat a b -> cat (f a) (f b)

instance Good.Covariant f => CovariantEndo (->) f where
    coendomap = Good.comap

-- | A contravariant endofunctor in an unenriched category.
class Category cat => ContravariantEndo cat f where
    contraendomap :: cat b a -> cat (f a) (f b)

instance Good.Contravariant f => ContravariantEndo (->) f where
    contraendomap = Good.contramap

-- | An invariant endofunctor (if that's even considered a functor) in an unenriched category.
class Category cat => InvariantEndo cat f where
    invendomap :: cat a b -> cat b a -> cat (f a) (f b)

instance Good.Invariant f => InvariantEndo (->) f where
    invendomap = Good.invmap

-- | A bi-endofunctor in an unenriched category covariant in both arguments.
class Category cat => BiEndo cat f where
    biendomap :: cat a c -> cat b d -> cat (f a b) (f c d)

instance Good.Bi f => BiEndo (->) f where
    biendomap = Good.bimap

-- | A pro-endofunctor in an unenriched category (contravariant in the left argument, covariant in the right).
class Category cat => ProEndo cat f where
    diendomap :: cat c a -> cat b d -> cat (f a b) (f c d)

instance Good.Pro f => ProEndo (->) f where
    diendomap = Good.dimap

-- | A covariant functor between unenriched categories.
class (Category dom, Category cod) => Covariant dom cod f where
    comap :: dom a b -> cod (f a) (f b)

instance CovariantEndo cat f => Covariant cat cat f where
    comap = coendomap

-- | A contravariant functor between unenriched categories.
class (Category dom, Category cod) => Contravariant dom cod f where
    contramap :: dom b a -> cod (f a) (f b)

instance ContravariantEndo cat f => Contravariant cat cat f where
    contramap = contraendomap

-- | An invariant functor (if that's even considered a functor) between unenriched categories.
class (Category dom, Category cod) => Invariant dom cod f where
    invmap :: dom a b -> dom b a -> cod (f a) (f b)

instance InvariantEndo cat f => Invariant cat cat f where
    invmap = invendomap

-- | A bifunctor in an unenriched category covariant in both arguments.
class (Category dom, Category cod) => Bi dom cod f where
    bimap :: dom a c -> dom b d -> cod (f a b) (f c d)

instance BiEndo cat f => Bi cat cat f where
    bimap = biendomap

-- | A profunctor in an unenriched category (contravariant in the left argument, covariant in the right).
class (Category dom, Category cod) => Pro dom cod f where
    dimap :: dom c a -> dom b d -> cod (f a b) (f c d)

instance ProEndo cat f => Pro cat cat f where
    dimap = diendomap

instance Pro (:-) (->) (:-) where
    dimap f g h = compose g (compose h f)

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

instance Good.Semigroup s => Semigroup (->) s where
    append (x, y) = Good.append x y

-- | A monoid object in a monoidal unenriched category.
class Semigroup cat s => Monoid cat s where
    empty :: Unit cat `cat` s

instance Good.Monoid s => Monoid (->) s where
    empty () = Good.empty

-- | An applicative functor.
class (Monoidal cat, CovariantEndo cat f) => Applicative cat f where
    pure :: a `cat` f a
    ap :: Product cat (f (a `cat` b)) (f a) `cat` f b

instance Good.Applicative f => Applicative (->) f where
    pure = Good.pure
    ap (f, x) = Good.ap f x

-- | A monoid object in the category of endofunctors in a monoidal unenriched category.
class Applicative cat m => Monad cat m where
    join :: m (m a) `cat` m a

instance Good.Monad m => Monad (->) m where
    join = Good.join
