-- | The category of natural transformations.
module Category.Nat where

import Category
import Data.Kind (Constraint)

type instance (~>) = Nat

newtype Nat f g = Nat { runNat :: forall a. f a ~> g a }

nat2 :: forall f g. (forall a b. f a b -> g a b) -> f ~> g
nat2 f = Nat (Nat f)

runNat2 = runNat . runNat
runNat3 = runNat . runNat2
runNat4 = runNat . runNat3

class (forall a. c (nat a)) => NatConstraint (c :: j -> Constraint) (nat :: i -> j)
instance (forall a. c (nat a)) => NatConstraint (c :: j -> Constraint) (nat :: i -> j)

instance Category r => Category (NatConstraint r) where
    identity _ = Nat (identity (Proxy @r))
    compose _ (Nat f) (Nat g) = Nat (compose (Proxy @r) f g)
