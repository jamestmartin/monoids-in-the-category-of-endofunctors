module Data.Nat where

import Category
import Category.Functor.Foldable

data N = Z | S N
data NF r = ZF | SF r
type instance Base N = NF

instance Functor NF where
    type Dom NF = (->)
    type Cod NF = (->)
    map _ ZF = ZF
    map f (SF r) = SF (f r)

instance Recursive N where
    project Z = ZF
    project (S n) = SF n

instance Corecursive N where
    embed ZF = Z
    embed (SF n) = S n

data NTy n where
    ZTy :: NTy 'Z
    STy :: NTy n -> NTy ('S n)
data NFTy r n where
    ZFTy :: NFTy r 'Z
    SFTy :: r n -> NFTy r ('S n)
type instance Base NTy = NFTy

instance Functor NTy where
    type Dom NTy = (:~:)
    type Cod NTy = (->)
    map Refl x = x

instance Functor (NFTy r) where
    type Dom (NFTy r) = (:~:)
    type Cod (NFTy r) = (->)
    map Refl x = x

instance Functor NFTy where
    type Dom NFTy = Nat (:~:) (->)
    type Cod NFTy = Nat (:~:) (->)

    map (Nat f) = Nat \_ -> \case
        ZFTy -> ZFTy
        (SFTy r) -> SFTy (f Refl r)

instance Recursive NTy where
    project = Nat \_ -> \case
        ZTy -> ZFTy
        (STy r) -> SFTy r

instance Corecursive NTy where
    embed = Nat \_ -> \case
        ZFTy -> ZTy
        (SFTy r) -> STy r
