module Data.Fin
    ( Fin (FZ, FS)
    , FinF (FZF, FSF)
    , fin2nat
    ) where

import Category.Functor
import Category.Functor.Foldable
import Data.Identity
import Data.Kind (Type)
import Data.Nat

data Fin :: N -> Type where
    FZ :: Fin ('S n)
    FS :: Fin n -> Fin ('S n)
data FinF r :: N -> Type where
    FZF :: FinF r ('S n)
    FSF :: r n -> FinF r ('S n)
type instance Base Fin = FinF

instance Functor (Nat (->) (:~:)) (Nat (->) (:~:)) FinF where
    map :: forall f g. Nat (->) (:~:) f g -> Nat (->) (:~:) (FinF f) (FinF g)
    map (Nat_ f) = Nat_ \case
        FZF -> FZF
        (FSF r) -> FSF (f r)

instance Recursive (Nat (->) (:~:)) Fin where
    project = Nat_ \case
        FZ -> FZF
        (FS r) -> FSF r

instance Corecursive (Nat (->) (:~:)) Fin where
    embed = Nat_ \case
        FZF -> FZ
        (FSF r) -> FS r

fin2nat :: Nat (->) (:~:) Fin (Const N)
fin2nat = cata (Nat_ alg)
    where alg :: FinF (Const N) n -> Const N n
          alg FZF = Const Z
          alg (FSF (Const n)) = Const (S n)
