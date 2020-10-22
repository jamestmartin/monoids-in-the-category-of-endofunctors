{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Category
    ( module Category.Base
    , module Category.Functor.Base
    , module Category.Natural
    , Monoidal, Unit, Product, monBi, monIdLI, monIdRI, monIdLE, monIdRE, monAssocL, monAssocR
    , BiEndo, biendomap
    , ProEndo, diendomap
    , Bi, bimap
    , Pro, dimap
    , Semigroup, append
    , Monoid, empty
    , Applicative, pure, ap
    , Monad, join
    ) where

import Category.Base
import Category.Constraint
import Category.Functor.Base
import Category.Natural
import Category.Star qualified as Star
import Data.Kind (Type)

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
class (Monoidal cat, Endofunctor cat f) => Applicative cat f where
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

class AnyC a
instance AnyC a

class AnyC2 a b
instance AnyC2 a b
