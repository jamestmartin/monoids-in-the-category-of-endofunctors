{-# LANGUAGE AllowAmbiguousTypes #-}
module Category.Monoid where

import Category
import Category.Functor
import Category.Functor.Identity
import Category.Monoidal
import Category.Nat
import Category.Semigroup
import Data.Kind (Constraint)
import qualified Prelude

class Semigroup r m => Monoid r m where
    empty :: Unit ~> m

instance {-# OVERLAPPABLE #-} (Category r, Prelude.Monoid m) => Monoid r m where
    empty _ = Prelude.mempty

instance {-# OVERLAPPABLE #-} (Category (Functor r r), Prelude.Monad m) => Monoid (Functor r r) m where
    empty = Nat \(Identity x) -> Prelude.return x
