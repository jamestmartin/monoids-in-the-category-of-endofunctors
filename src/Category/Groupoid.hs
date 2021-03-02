module Category.Groupoid (Groupoid, inv) where

import Category.Base
import Data.Kind (Constraint, Type)

type Groupoid :: (i -> i -> Type) -> Constraint
class Category morph => Groupoid morph where
    inv :: morph a b -> morph b a
