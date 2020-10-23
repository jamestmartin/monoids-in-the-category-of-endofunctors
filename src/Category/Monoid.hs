module Category.Monoid where

import Category.Functor
import Category.Monoidal

class TensorProduct f => Semigroup (f :: i -> i -> i) (s :: i) where
    append :: Cod2 f (f s s) s

class Semigroup f m => Monoid f m where
    empty :: proxy f -> Cod2 f (Unit f) m
