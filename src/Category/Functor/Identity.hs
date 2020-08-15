module Category.Functor.Identity where

import Category
import Category.Functor

newtype Identity a = Identity { getIdentity :: a }

instance (Category src, Category dest) => Functor src dest Identity where
    map _ _ f (Identity x) = Identity (f x)
