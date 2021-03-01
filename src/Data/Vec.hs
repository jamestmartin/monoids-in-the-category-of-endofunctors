module Data.Vec where

import Category.Base
import Category.Functor
import Category.Functor.Foldable
import Data.Dict
import Data.Fin
import Data.Identity
import Data.Kind (Type)
import Data.Nat
import Prelude (($))

type Vec :: Type -> N -> Type
data Vec a :: N -> Type where
    VZ :: Vec a 'Z
    VS :: a -> Vec a n -> Vec a ('S n)
type VecF :: Type -> (N -> Type) -> N -> Type
data VecF a r :: N -> Type where
    VZF :: VecF a r 'Z
    VSF :: a -> r n -> VecF a r ('S n)
type instance Base (Vec a) = VecF a

instance Functor (Nat (->) (:~:)) (->) Vec where
    map_ _ _ = Dict
    map f = Nat \case
        VZ -> VZ
        (VS x r) -> VS (f x) (runNat (map @_ @_ @(Nat (->) (:~:)) f) r)

instance Functor (Nat (->) (:~:)) (Nat (->) (:~:)) (VecF a) where
    map_ _ _ = Dict
    map (Nat f) = Nat \case
        VZF -> VZF
        (VSF x r) -> VSF x (f r)

instance Functor (Nat (Nat (->) (:~:)) (Nat (->) (:~:))) (->) VecF where
    map_ _ _ = Dict
    map f = Nat $ Nat \case
        VZF -> VZF
        (VSF x r) -> VSF (f x) r

type Ixr :: (N -> Type) -> Type -> N -> Type
newtype Ixr ty r a = Ixr { getIxr :: ty a -> r }

indexer :: Nat (->) (:~:) Fin (Ixr (Vec a) a)
indexer = cata $ Nat \case
    FZF -> Ixr \case VS x _ -> x
    (FSF (Ixr r)) -> Ixr \case VS _ xs -> r xs

index :: Fin n -> Vec a n -> a
index = getIxr . runNat indexer
