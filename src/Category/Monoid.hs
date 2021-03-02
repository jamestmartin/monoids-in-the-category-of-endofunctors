{-# LANGUAGE UndecidableInstances #-}
module Category.Monoid
    ( Monoid, empty
    , (>>=)
    ) where

import Category.Base
import Category.Functor
import Category.Monoidal
import Category.Semigroup
import Data.Kind (Constraint, Type)
import Data.Maybe (Maybe (Just))
import Data.Proxy

type Monoid :: (i -> i -> Type) -> (i -> i -> i) -> i -> Constraint
class Semigroup morph prod m => Monoid morph prod m where
    empty :: proxy morph -> proxy' prod -> morph (Unit morph prod) m

instance Monoid (Nat (->) (->)) Compose Maybe where
    empty _ _ = Nat_ \(Identity x) -> Just x

-- | A monad is a monoid object in the monoidal category of endofunctors and natural transformations between them!
class Monoid (Nat (->) (->)) Compose m => Monad m
instance Monoid (Nat (->) (->)) Compose m => Monad m

(>>=) :: forall f a b. Monoid (Nat (->) (->)) Compose f => f a -> (a -> f b) -> f b
m >>= f = runNat (append @_ @(Nat (->) (->))) id (lemma (Compose (map f m)))
    where lemma :: forall c. (Functor (->) (->) f => c) -> c
          lemma x = case empty (Proxy @(Nat (->) (->))) (Proxy @Compose) :: Nat (->) (->) Identity f of Nat _ -> x
