module Data.Fin where

import Category
import Category.Functor.Foldable
import Data.Kind (Type)
import Data.Nat

data Fin :: N -> Type where
    FZ :: Fin ('S n)
    FS :: Fin n -> Fin ('S n)
data FinF r :: N -> Type where
    FZF :: FinF r ('S n)
    FSF :: r n -> FinF r ('S n)
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
    map_ = Sub Dict
    map :: forall f g. Nat (:~:) (->) f g -> Nat (:~:) (->) (FinF f) (FinF g)
    map (Nat f) = Nat \case
        FZF -> FZF
        (FSF r) -> FSF (f r)

instance Recursive Fin where
    project = Nat \case
        FZ -> FZF
        (FS r) -> FSF r

instance Corecursive Fin where
    embed = Nat \case
        FZF -> FZ
        (FSF r) -> FS r

fin2nat :: Nat (:~:) (->) Fin (Const (:~:) N)
fin2nat = cata (Nat alg)
    where alg :: FinF (Const (:~:) N) n -> Const (:~:) N n
          alg FZF = Const Z
          alg (FSF (Const n)) = Const (S n)
