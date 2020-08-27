{-# LANGUAGE UndecidableSuperClasses #-}
module Category.Semigroup where

import Category
import Category.Functor
import Category.NaturalTransformation
import qualified Control.Monad
import Prelude (uncurry)
import qualified Prelude

class (MonoidalCategory hom obj, obj m) => Semigroup hom obj m where
    append :: proxy obj -> Product obj m m `hom` m

instance {-# INCOHERENT #-} Prelude.Functor f => Functor (->) EveryC (->) EveryC f where
    mapObj _ _ _ = Dict
    map _ _ = Prelude.fmap

instance {-# OVERLAPPABLE #-} (MonoidalCategory (->) obj, obj m, Prelude.Semigroup m) => Semigroup (->) obj m where
    append _ = uncurry (Prelude.<>)

instance {-# OVERLAPPABLE #-} (hom ~ Nat (->) EveryC (->) EveryC, MonoidalCategory hom (Endofunctor (->) EveryC), Prelude.Monad m) => Semigroup hom (Endofunctor (->) EveryC) m where
    append _ = Nat \(ComposeEndofunctor x) -> Control.Monad.join x
