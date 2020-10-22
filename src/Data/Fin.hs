module Data.Fin where

import Category
import Category.Functor.Foldable
import Data.Nat

data Fin n where
    FZ :: Fin ('S n)
    FS :: Fin n -> Fin ('S n)
data FinF r n where
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

fin2nat :: Nat (:~:) (->) Fin (Const N)
fin2nat = cata (Nat \_ -> alg)
    where alg :: FinF (Const N) n -> Const N n
          alg FZF = Const Z
          alg (FSF (Const n)) = Const (S n)
