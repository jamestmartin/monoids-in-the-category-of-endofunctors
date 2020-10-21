{-# LANGUAGE PolyKinds #-}
module Category.Free where

import Category
import Data.Kind (Type)

data FreeCategory (hom :: i -> i -> Type) :: i -> i -> Type where
    Id :: FreeCategory hom a a
    Embed :: !(hom a b) -> FreeCategory hom a b
    Compose :: !(FreeCategory hom b c) -> !(FreeCategory hom a b) -> FreeCategory hom a c

instance Category (FreeCategory hom) where
    identity = Id
    compose = Compose
