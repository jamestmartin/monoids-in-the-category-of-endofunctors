module Category.Semigroup where

import Category.Functor
import Category.Monoidal
import Data.Kind (Constraint, Type)
import Data.Maybe (Maybe (Nothing, Just))

type Semigroup :: (i -> i -> Type) -> (i -> i -> i) -> i -> Constraint
class Monoidal morph prod => Semigroup morph prod s where
    append :: morph (prod s s) s

instance Semigroup (Nat (->) (->)) Compose Maybe where
    append = Nat \(Compose x') -> case x' of
        Nothing -> Nothing
        Just Nothing -> Nothing
        Just (Just x) -> Just x

-- TODO: More semigroup/monoid instances. `Either` has two, can I accomodate both?
