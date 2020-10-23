module Data.Vec where

import Category
import Category.Functor.Foldable
import Data.Fin
import Data.Kind (Type)
import Data.Nat
import Prelude (($))

data Vec a :: N -> Type where
    VZ :: Vec a 'Z
    VS :: a -> Vec a n -> Vec a ('S n)
data VecF a r :: N -> Type where
    VZF :: VecF a r 'Z
    VSF :: a -> r n -> VecF a r ('S n)
type instance Base (Vec a) = (VecF a)

instance Functor (Vec a) where
    type Dom (Vec a) = (:~:)
    type Cod (Vec a) = (->)
    map Refl x = x

instance Functor Vec where
    type Dom Vec = (->)
    type Cod Vec = Nat (:~:) (->)
    map_ = Sub Dict
    map f = Nat \case
        VZ -> VZ
        (VS x r) -> VS (f x) (runNat (map f) r)

instance Functor (VecF a r) where
    type Dom (VecF a r) = (:~:)
    type Cod (VecF a r) = (->)
    map Refl x = x

instance Functor (VecF a) where
    type Dom (VecF a) = Nat (:~:) (->)
    type Cod (VecF a) = Nat (:~:) (->)
    map_ = Sub Dict
    map (Nat f) = Nat \case
        VZF -> VZF
        (VSF x r) -> VSF x (f r)

instance Functor VecF where
    type Dom VecF = (->)
    type Cod VecF = Nat (Nat (:~:) (->)) (Nat (:~:) (->))
    map_ = Sub Dict
    map f = Nat $ Nat \case
        VZF -> VZF
        (VSF x r) -> VSF (f x) r

newtype Ixr ty r a = Ixr { getIxr :: ty a -> r }

instance Functor (Ixr ty r) where
    type Dom (Ixr ty r) = (:~:)
    type Cod (Ixr ty r) = (->)
    map Refl x = x

indexer :: Nat (:~:) (->) Fin (Ixr (Vec a) a)
indexer = cata $ Nat \case
    FZF -> Ixr \case VS x _ -> x
    (FSF (Ixr r)) -> Ixr \case VS _ xs -> r xs

index :: Fin n -> Vec a n -> a
index = getIxr . runNat indexer
