-- | The natural numbers and associated types and functions.
module Data.Nat
    ( N (Z, S), inf
    , NTyF (ZTyF, STyF)
    , (:+)
    , injective, rightZero
    ) where

import Category.Functor
import Category.Functor.Foldable
import Data.Dict
import Data.Identity
import Data.Kind (Type)
import Data.Maybe (Maybe (Nothing, Just))
import Quantifier

-- | The (co-)natural numbers.
data N = Z   -- ^ Zero
       | S N -- ^ Successor (@+1@)

-- | Infinity, represented as the fixpoint of the successor function.
inf :: N
inf = S inf

instance Pi N where
    data Ty N :: N -> Type where
        ZTy :: Ty N 'Z
        STy :: Ty N n -> Ty N ('S n)
    depi ZTy = Z
    depi (STy n) = S (depi n)

-- | Disregard this and use @'TyC' N@ instead.
class NTyC n where
    depic_n :: Ty N n
instance NTyC 'Z where
    depic_n = ZTy
instance NTyC n => NTyC ('S n) where
    depic_n = STy depic_n

instance PiC N where
    type TyC N = NTyC
    depic = depic_n

-- | The base functor for @'Ty' N@.
data NTyF r n where
    ZTyF :: NTyF r 'Z
    STyF :: r n -> NTyF r ('S n)

instance Functor (Nat (->) (:~:)) (Nat (->) (:~:)) NTyF where
    map (Nat_ f) = Nat_ \case
        ZTyF -> ZTyF
        (STyF r) -> STyF (f r)

type instance Base N = Maybe

instance Recursive (->) N where
    project Z = Nothing
    project (S n) = Just n

instance Corecursive (->) N where
    embed Nothing = Z
    embed (Just n) = S n

type instance Base (Ty N) = NTyF

instance Recursive (Nat (->) (:~:)) (Ty N) where
    project = Nat_ \case
        ZTy -> ZTyF
        (STy r) -> STyF r

instance Corecursive (Nat (->) (:~:)) (Ty N) where
    embed = Nat_ \case
        ZTyF -> ZTy
        (STyF r) -> STy r

-- | Type-level addition.
type family (:+) (m :: N) (n :: N) :: N where
    'Z   :+ n =          n
    'S m :+ n = 'S (m :+ n)

-- | A proof that the successor function is injective.
injective :: forall m n. Ty N m -> ('S m ~ 'S n) => Dict (m ~ n)
injective ZTy = Dict
injective (STy i) = case injective i of Dict -> Dict

-- | A proof that zero is the right identity of addition.
-- (The constraint solver can prove the left identity on its own.)
rightZero :: forall m. Ty N m -> Dict ((m :+ 'Z) ~ m)
rightZero ZTy = Dict
rightZero (STy i) = case rightZero i of Dict -> Dict
