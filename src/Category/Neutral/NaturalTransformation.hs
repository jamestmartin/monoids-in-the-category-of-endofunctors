{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Category.Neutral.NaturalTransformation where

import Category.Neutral
import Data.Kind (Type)

-- f , g :: i -> j
-- (~>) :: j -> j -> Type
-- Nat :: (i -> j) -> (i -> j) -> Type
newtype Nat f g = Nat { runNat :: forall a. f a ~> g a }

type instance (~>) = Nat

type D (hom :: (i -> j) -> (i -> j) -> Type) = (~>) :: j -> j -> Type

instance (nat ~ Nat, Category (D nat)) => Category (nat :: (i -> j) -> (i -> j) -> Type) where
    identity = Nat identity
    compose (Nat f) (Nat g) = Nat (compose f g)
