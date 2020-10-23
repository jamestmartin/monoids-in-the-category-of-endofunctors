module Data.Nat where

import Category
import Category.Functor.Foldable
import Data.Kind (Type)
import Data.Maybe (Maybe (Nothing, Just))
import Quantifier

data N = Z | S N

inf :: N
inf = S inf

instance Pi N where
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
    map_ = Sub Dict
    map (Nat f) = Nat \case
        ZTyF -> ZTyF
        (STyF r) -> STyF (f r)

type instance Base N = Maybe

instance Recursive N where
    project Z = Nothing
    project (S n) = Just n

instance Corecursive N where
    embed Nothing = Z
    embed (Just n) = S n

type instance Base (Ty N) = NTyF

instance Recursive (Ty N) where
    project = Nat \case
        ZTy -> ZTyF
        (STy r) -> STyF r

instance Corecursive (Ty N) where
    embed = Nat \case
        ZTyF -> ZTy
        (STyF r) -> STy r

type family (:+) (m :: N) (n :: N) :: N where
    'Z   :+ n =          n
    'S m :+ n = 'S (m :+ n)

injective :: forall m n. Ty N m -> ('S m ~ 'S n) :- (m ~ n)
injective ZTy = Sub Dict
injective (STy i) = case injective i of Sub Dict -> Sub Dict

rightZero :: forall m. Ty N m -> Dict ((m :+ 'Z) ~ m)
rightZero ZTy = Dict
rightZero (STy i) = case rightZero i of Dict -> Dict
