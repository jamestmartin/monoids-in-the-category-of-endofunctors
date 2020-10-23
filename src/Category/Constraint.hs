module Category.Constraint
    ( module Data.Dict
    , (:-) (Sub)
    , (\\)
    ) where

import Category.Base
import Data.Dict

newtype a :- b = Sub (a => Dict b)

(\\) :: a => (b => c) -> (a :- b) -> c
r \\ Sub Dict = r

instance Semigroupoid (:-) where
    f . g = Sub (Dict \\ f \\ g)

instance Category (:-) where
    id = Sub Dict
