module Category.Semigroup (Semigroup, append) where

import Category.Functor
import Category.Monoidal
import Data.Kind (Constraint, Type)
import Data.Maybe (Maybe (Nothing, Just))

type Semigroup :: (i -> i -> Type) -> (i -> i -> i) -> i -> Constraint
class TensorProduct morph prod => Semigroup morph prod s where
    append :: morph (prod s s) s

instance Semigroup (Nat (->) (->)) Compose Maybe where
    append = Nat \_ (Compose x') -> case x' of
        Nothing -> Nothing
        Just Nothing -> Nothing
        Just (Just x) -> Just x

-- TODO: More semigroup/monoid instances. `Either` has two, can I accomodate both?
