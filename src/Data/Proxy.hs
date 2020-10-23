module Data.Proxy where

data Proxy (a :: i) where
    Proxy :: Proxy a
