{-# LANGUAGE RankNTypes #-}
module Data.Dict where

import Control.Category (Category, id, (.))

data Dict c where
    Dict :: c => Dict c

newtype a :- b = Sub (a => Dict b)

(\\) :: a => (b => c) -> (a :- b) -> c
r \\ Sub Dict = r

instance Category (:-) where
    id = Sub Dict
    f . g = Sub (Dict \\ f \\ g)
