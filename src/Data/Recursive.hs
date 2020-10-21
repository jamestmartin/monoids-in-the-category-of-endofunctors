{-# LANGUAGE PolyKinds #-}
module Data.Recursive where

import Category
import Category.Good qualified as Good
import Data.Kind (Type)
import Data.Maybe (Maybe (Nothing, Just))

type family Base (t :: i) :: i -> i

class (Category cat, CovariantEndo cat (Base t)) => Recursive cat (t :: i) where
    project :: cat t (Base t t)

class (Category cat, CovariantEndo cat (Base t)) => Corecursive cat (t :: i) where
    embed :: cat (Base t t) t

data N = Z | S N

type instance Base N = Maybe

instance Good.Covariant Maybe where
    comap _ Nothing  = Nothing
    comap f (Just x) = Just (f x)

instance Recursive (->) N where
    project Z     = Nothing
    project (S n) = Just n

data Fin (n :: N) where
    FZ :: Fin ('S n)
    FS :: Fin n -> Fin ('S n)

data FinF (r :: N -> Type) (n :: N) :: Type where
    FZF :: FinF r ('S n)
    FSF :: r n -> FinF r ('S n)

type instance Base Fin = FinF

instance CovariantEndo Nat FinF where
    coendomap (Nat f) = Nat \case
        FZF -> FZF
        (FSF r) -> FSF (f r)

instance Recursive Nat Fin where
    project = Nat \case
        FZ -> FZF
        (FS n) -> FSF n

data Vec (a :: Type) (n :: N) :: Type where
    VZ :: Vec a 'Z
    VS :: a -> Vec a n -> Vec a ('S n)

data VecF (a :: Type) (r :: N -> Type) (n :: N) :: Type where
    VZF :: VecF a r 'Z
    VSF :: a -> r n -> VecF a r ('S n)

type instance Base (Vec a) = VecF a

instance CovariantEndo Nat (VecF a) where
    coendomap (Nat f) = Nat \case
        VZF -> VZF
        (VSF x r) -> VSF x (f r)

instance Recursive Nat (Vec a) where
    project = Nat \case
        VZ -> VZF
        (VS x r) -> VSF x r

type Algebra cat t a = t a `cat` a

cata :: Recursive cat t => Algebra cat (Base t) a -> t `cat` a
cata alg = alg . coendomap (cata alg) . project

newtype Ixr a n = Ixr { getIxr :: Vec a n -> a }

indexer :: Fin ~> Ixr a
indexer = cata (Nat alg)
    where alg :: FinF (Ixr a) n -> Ixr a n
          alg = \case
                  FZF -> Ixr (\case (VS x _) -> x)
                  (FSF (Ixr r)) -> Ixr (\case (VS _ xs) -> r xs)

index :: Vec a n -> Fin n -> a
index xs i = getIxr (runNat indexer i) xs
