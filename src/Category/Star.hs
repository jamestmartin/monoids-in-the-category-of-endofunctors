-- | Re-export category typeclasses which are specialized to the category of types.
module Category.Star
    ( module Control.Applicative
    , module Control.Monad
    , module Data.Bifunctor
    , module Data.Functor
    , module Data.Functor.Contravariant
    , module Data.Functor.Invariant
    , module Data.Monoid
    , module Data.Profunctor
    , module Data.Semigroup
    ) where

import Control.Applicative        (Applicative, pure, (<*>))
import Control.Monad              (Monad, join)
import Data.Bifunctor             (Bifunctor, bimap)
import Data.Functor               (Functor, fmap)
import Data.Functor.Contravariant (Contravariant, contramap)
import Data.Functor.Invariant     (Invariant, invmap)
import Data.Monoid                (Monoid, mempty)
import Data.Profunctor            (Profunctor, dimap)
import Data.Semigroup             (Semigroup, (<>))
