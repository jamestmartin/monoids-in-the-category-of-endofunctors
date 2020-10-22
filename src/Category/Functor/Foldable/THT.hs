{-# LANGUAGE TemplateHaskell #-}
module Category.Functor.Foldable.THT where

import Category.Functor.Foldable.TH
import Data.Kind (Type)

import Category
import Category.Functor.Foldable

data N = Z | S N

makeBaseFunctor ''N 0

data Fin :: N -> Type where
    FZ :: forall n. Fin ('S n)
    FS :: forall n. Fin n -> Fin ('S n)

makeBaseFunctor ''Fin 0
type instance Base Fin = FinF

instance Functor Fin where
    type Dom Fin = (:~:)
    type Cod Fin = (->)
    map Refl x = x

instance Functor (FinF r) where
    type Dom (FinF r) = (:~:)
    type Cod (FinF r) = (->)
    map Refl x = x

instance Functor FinF where
    type Dom FinF = Nat (:~:) (->)
    type Cod FinF = Nat (:~:) (->)
    map :: forall f g. Nat (:~:) (->) f g -> Nat (:~:) (->) (FinF f) (FinF g)
    map (Nat f) = Nat \_ -> \case
        FZF -> FZF
        (FSF r) -> FSF (f Refl r)

instance Recursive Fin where
    project = Nat \_ -> \case
        FZ -> FZF
        (FS r) -> FSF r

instance Corecursive Fin where
    embed = Nat \_ -> \case
        FZF -> FZ
        (FSF r) -> FS r

data Vec a :: N -> Type where
    VZ :: Vec a 'Z
    VS :: a -> Vec a n -> Vec a ('S n)

makeBaseFunctor ''Vec 0
