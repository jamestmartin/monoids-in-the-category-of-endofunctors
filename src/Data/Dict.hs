module Data.Dict where

data Dict c where
    Dict :: c => Dict c
