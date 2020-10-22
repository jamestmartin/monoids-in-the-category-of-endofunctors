{-# LANGUAGE UndecidableInstances #-}
module Category.Functor.Base
    ( Phantom, phmap
    , Endofunctor, emap, Functor, map
    , ContraEndofunctor, contraemap, ContraFunctor, contramap
    , InvariantEndofunctor, invemap, InvariantFunctor, invmap
    ) where

import Category.Base
import Category.Star qualified as Star
import Data.Kind (Type)

-- | A type constructor ending in Type which varies freely in its argument.
--
-- All Phantom types should also implement Endofunctor and ContraEndofunctor.
class Category cat => Phantom cat f where
    phmap :: f a `cat` f b

-- | An endofunctor in an unenriched category which is covariant in its argument.
--
-- All Endofunctors should also implement InvariantEndofunctor.
class Category cat => Endofunctor cat f where
    emap :: a `cat` b -> f a `cat` f b

instance {-# OVERLAPPABLE #-} Star.Functor f => Endofunctor (->) f where
    emap = Star.fmap

-- | A functor between unenriched categories which is covariant in its argument.
--
-- All Functors should also implement InvariantFunctor.
class (Category dom, Category cod) => Functor dom cod f where
    map :: a `dom` b -> f a `cod` f b

instance {-# OVERLAPPABLE #-} Endofunctor cat f => Functor cat cat f where
    map = emap

-- \ An endofunctor in an unenriched category which is contravariant in its argument.
--
-- All ContraEndofunctors should also implement InvariantEndofunctor.
class Category cat => ContraEndofunctor cat f where
    contraemap :: b `cat` a -> f a `cat` f b

instance {-# OVERLAPPABLE #-} Star.Contravariant f => ContraEndofunctor (->) f where
    contraemap = Star.contramap

-- | A functor between unenriched categories which is contravariant in its argument.
--
-- All ContraFunctors should also implement InvariantFunctor.
class (Category dom, Category cod) => ContraFunctor dom cod f where
    contramap :: b `dom` a -> f a `cod` f b

instance {-# OVERLAPPABLE #-} ContraEndofunctor cat f => ContraFunctor cat cat f where
    contramap = contraemap

-- | An endofunctor-like type in an unenriched category which is parametric in its argument.
class Category cat => InvariantEndofunctor cat f where
    invemap :: a `cat` b -> b `cat` a -> f a `cat` f b

instance {-# OVERLAPPABLE #-} Star.Invariant f => InvariantEndofunctor (->) f where
    invemap = Star.invmap

-- | An endofunctor-like type between unenriched categories which is parametric in its argument.
class (Category dom, Category cod) => InvariantFunctor dom cod f where
    invmap :: a `dom` b -> b `dom` a -> f a `cod` f b

instance {-# OVERLAPPABLE #-} InvariantEndofunctor cat f => InvariantFunctor cat cat f where
    invmap = invemap
