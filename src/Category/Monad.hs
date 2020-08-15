module Category.Monad where

import Category
import Category.Applicative
import Category.Functor
import Category.Functor.Identity
import Category.Monoid
import Category.Monoidal
import Category.Nat
import Category.Semigroup
import Prelude (undefined)

class (Category (Functor r r), Monoid (Functor r r) m) => Monad r m
instance (Category (Functor r r), Monoid (Functor r r) m) => Monad r m

return :: forall proxy r m. Monad r m => proxy r -> Dom (Functor r r) Unit m
return _ = empty (Proxy @(Functor r r))

join :: forall proxy r m. Monad r m => proxy r -> Dom (Functor r r) (Product m m) m
join _ = append (Proxy @(Functor r r))

class (Category (Endofunctor cat), Comonoid (Endofunctor cat) w) => Comonad cat w
instance (Category (Endofunctor cat), Comonoid (Endofunctor cat) w) => Comonad cat w
