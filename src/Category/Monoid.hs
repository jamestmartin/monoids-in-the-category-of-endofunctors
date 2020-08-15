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
    empty :: proxy r -> Unit ~> m

instance {-# OVERLAPPABLE #-} (Category r, Prelude.Monoid m) => Monoid r m where
    empty _ _ = Prelude.mempty

instance {-# OVERLAPPABLE #-} (Category (Functor r r), Prelude.Monad m) => Monoid (Functor r r) m where
    empty _ = Nat \(Identity x) -> Prelude.return x

class MonoidalCategory cat => Comonoid cat m where
    duplicate :: proxy cat -> Dom cat m (Product m m)
    extract :: proxy cat -> Dom cat m Unit

instance MonoidalCategory cat => Comonoid cat (m :: *) where
    duplicate _ x = (x, x)
    extract _ _ = ()
