{-# LANGUAGE RankNTypes #-}
module Category.Constraint
    ( module Data.Dict
    , (:-) (Sub)
    , (\\)
    ) where

import Category.Base
import Data.Dict

newtype a :- b = Sub (a => Dict b)

type instance (~>) = (:-)

(\\) :: a => (b => c) -> (a :- b) -> c
r \\ Sub Dict = r

instance Category (:-) where
    id = Sub Dict
    f . g = Sub (Dict \\ f \\ g)
