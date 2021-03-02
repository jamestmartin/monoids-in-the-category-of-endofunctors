module Data.Identity ((:~:) (Refl)) where

import Category.Base
import Category.Functor
import Category.Groupoid
import Data.Kind (Type)

type (:~:) :: i -> i -> Type
data (:~:) :: i -> i -> Type where
    Refl :: a :~: a

instance Category (:~:) where
    Refl . Refl = Refl

instance NiceCat (:~:) where
    id = Refl

instance Groupoid (:~:) where
    inv Refl = Refl

-- TODO: There are lots, lots more valid functor instances.
instance {-# INCOHERENT #-} Functor (->) (:~:) f where
    map Refl = id

instance {-# INCOHERENT #-} Functor (->) (Yoneda (:~:)) f where
    map (Op Refl) = id

instance {-# INCOHERENT #-} Functor (Nat (->) (:~:)) (:~:) f where
    map Refl = id

instance {-# INCOHERENT #-} Functor (Nat (->) (:~:)) (Yoneda (:~:)) f where
    map (Op Refl) = id
