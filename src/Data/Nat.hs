module Data.Nat where

import Category
import Category.Functor.Foldable
import Category.Functor.Foldable.TH
import Data.Kind (Type)
import Data.Maybe (Maybe (Nothing, Just))
import Quantifier

data N = Z | S N

instance Pi N where
    type PiCat N = (->)
    data Ty N :: N -> Type where
        ZTy :: Ty N 'Z
        STy :: Ty N n -> Ty N ('S n)
    depi ZTy = Z
    depi (STy n) = S (depi n)

class NTyC n where
    nTy :: Ty N n
instance NTyC 'Z where
    nTy = ZTy
instance NTyC n => NTyC ('S n) where
    nTy = STy nTy

instance PiC N where
    type TyC N = NTyC
    depic = nTy

data NTyF r n where
    ZTyF :: NTyF r 'Z
    STyF :: r n -> NTyF r ('S n)

instance Functor (NTyF r) where
    type Dom (NTyF r) = (:~:)
    type Cod (NTyF r) = (->)
    map Refl x = x

instance Functor NTyF where
    type Dom NTyF = Nat (:~:) (->)
    type Cod NTyF = Nat (:~:) (->)
    map (Nat f) = Nat \_ -> \case
        ZTyF -> ZTyF
        (STyF r) -> STyF (f Refl r)

type instance Base N = Maybe

instance Recursive N where
    project Z = Nothing
    project (S n) = Just n

instance Corecursive N where
    embed Nothing = Z
    embed (Just n) = S n

type instance Base (Ty N) = NTyF

instance Recursive (Ty N) where
    project = Nat \_ -> \case
        ZTy -> ZTyF
        (STy r) -> STyF r

instance Corecursive (Ty N) where
    embed = Nat \_ -> \case
        ZTyF -> ZTy
        (STyF r) -> STy r
