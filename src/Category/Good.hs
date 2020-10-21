module Category.Good where

class Irrelevant f where
    irrmap :: f a -> f b

-- | A covariant endofunctor in the category of types.
class Covariant f where
    comap :: (a -> b) -> (f a -> f b)

-- | A contravariant endofunctor in the category of types.
class Contravariant f where
    contramap :: (b -> a) -> (f a -> f b)

-- | An invariant endofunctor in the category of types.
class Invariant f where
    invmap :: (a -> b) -> (b -> a) -> (f a -> f b)

-- | A bifunctor in the category of types covariant in both arguments.
class Bi f where
    bimap :: (a -> c) -> (b -> d) -> (f a b -> f c d)

instance Bi (,) where
    bimap f g (x, y) = (f x, g y)

-- | A profunctor in the category of types (contravariant in the left argument, covariant in the right).
class Pro f where
    dimap :: (c -> a) -> (b -> d) -> (f a b -> f c d)

instance Pro (->) where
    dimap f g h x = g (h (f x))

-- | An applicative functor.
class Covariant f => Applicative f where
    pure :: a -> f a
    ap :: f (a -> b) -> f a -> f b

-- | A monoid object in the category of endofunctors in the category of types.
class Applicative m => Monad m where
    join :: m (m a) -> m a

-- | A semigroup object in the category of types.
class Semigroup s where
    append :: s -> s -> s

-- | A monoid object in the category of types.
class Semigroup m => Monoid m where
    empty :: m
