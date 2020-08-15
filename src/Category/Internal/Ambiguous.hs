{-# LANGUAGE AllowAmbiguousTypes #-}
module Category.Internal.Ambiguous where

import Category.Internal.Hom
import Data.Kind (Constraint)

class Category (r :: k -> Constraint) where
    identity :: forall a. r a => Dom r a a
    compose :: forall a b c. (r a, r b, r c) => Dom r b c -> Dom r a b -> Dom r a c
