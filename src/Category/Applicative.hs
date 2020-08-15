{-# LANGUAGE AllowAmbiguousTypes #-}
module Category.Applicative where

import Category
import Category.Functor
import Category.Functor.Identity
import qualified Prelude

class Functor r s f => Applicative r s f where
    pure :: (r a, s (f a)) => a ~> f a
    ap :: (r a, r b, r (a ~> b), s (f (a ~> b)), s (f a), s (f b)) => f (a ~> b) -> f a ~> f b

instance {-# OVERLAPPABLE #-} Prelude.Applicative f => Applicative r s f where
    pure = Prelude.pure
    ap = (Prelude.<*>)

instance Applicative r s Identity where
    pure = Identity
    ap (Identity f) x = f <$> x
