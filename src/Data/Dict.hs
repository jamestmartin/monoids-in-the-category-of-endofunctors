{-# LANGUAGE RankNTypes #-}
module Data.Dict where

data Dict c where
    Dict :: c => Dict c

newtype a :- b = Sub (a => Dict b)

(\\) :: a => (b => c) -> (a :- b) -> c
r \\ Sub Dict = r
