module Category.Monoid where

import Category.Base
import Category.Functor
import Category.Monoidal
import Category.Semigroup
import Data.Dict
import Data.Kind (Constraint, Type)
import Data.Maybe (Maybe (Just))
import Data.Proxy

type Monoid :: (i -> i -> Type) -> (i -> i -> i) -> i -> Constraint
class Semigroup morph prod m => Monoid morph prod m where
    empty :: proxy morph -> proxy' prod -> morph (Unit morph prod) m

instance Monoid (Nat (->) (->)) Compose Maybe where
    empty _ _ = Nat \(Identity x) -> Just x

(>>=) :: forall f a b. Monoid (Nat (->) (->)) Compose f => f a -> (a -> f b) -> f b
m >>= f = runNat (append @_ @(Nat (->) (->))) m'
    where m' :: Compose f f b
          m' = case obj (empty @_ @_ @_ @f (Proxy @(Nat (->) (->))) (Proxy @Compose)) of Dict -> Compose (map f m)
