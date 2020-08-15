module Category.Semigroup where

import Category
import Category.Functor
import Category.Monoidal
import Category.Nat
import qualified Control.Monad
import Data.Kind (Constraint)
import Prelude (curry, uncurry)
import qualified Prelude

class MonoidalCategory r => Semigroup r m where
    append :: proxy r -> Dom r (Product m m) m

instance {-# OVERLAPPABLE #-} (MonoidalCategory r, Prelude.Semigroup m) => Semigroup r m where
    append _ = uncurry (Prelude.<>)

instance {-# OVERLAPPABLE #-} (Category (Functor r r), Prelude.Monad m) => Semigroup (Functor r r) m where
    append _ = Nat \(Compose x) -> Control.Monad.join x
