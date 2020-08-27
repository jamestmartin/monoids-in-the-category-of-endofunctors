{-# LANGUAGE UndecidableSuperClasses #-}
module Category.Monoid where

import Category
import Category.Functor
import Category.NaturalTransformation
import Category.Semigroup
import qualified Control.Monad
import qualified Prelude

class Semigroup hom obj m => Monoid hom obj m where
    empty :: proxy obj -> Unit obj `hom` m

instance (Semigroup (->) obj m, Prelude.Monoid m) => Monoid (->) obj m where
    empty _ () = Prelude.mempty

instance (hom ~ Nat (->) EveryC (->) EveryC, MonoidalCategory hom (Endofunctor (->) EveryC), Prelude.Monad m) => Monoid hom (Endofunctor (->) EveryC) m where
    empty _ = Nat \(Identity x) -> Prelude.return x
