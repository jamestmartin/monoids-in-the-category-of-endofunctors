{-# LANGUAGE AllowAmbiguousTypes #-}
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

return :: forall r m. Monad r m => Dom (Functor r r) Unit m
return = empty @(Functor r r)

join :: forall r m. Monad r m => Dom (Functor r r) (Product m m) m
join = append @(Functor r r)