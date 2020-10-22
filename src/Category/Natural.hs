{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
-- | The category of natural transformations.
module Category.Natural where

import Category.Base
import Data.Kind (Type)

newtype Nat f g = Nat { runNat :: forall a. f a ~> g a }

type instance (~>) = Nat

type ChildHom (nat :: (i -> j) -> (i -> j) -> Type) = (~>) :: j -> j -> Type

instance (nat ~ Nat, Category (ChildHom nat)) => Category (nat :: (i -> j) -> (i -> j) -> Type) where
    id = Nat id
    Nat f . Nat g = Nat (f . g)
