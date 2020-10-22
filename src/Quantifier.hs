module Quantifier where

import Data.Kind (Constraint, Type)
import Data.Maybe (Maybe (Nothing, Just))

-- | An explicit dependent quantifier.
class Pi (ty :: Type) where
    type PiCat ty :: i -> i -> Type
    data Ty ty :: ty -> Type
    depi :: forall a. Ty ty a -> ty

-- | An implicit dependent quantifier.
class Pi ty => PiC (ty :: Type) where
    type TyC ty :: ty -> Constraint
    depic :: forall a. TyC ty a => Ty ty a

instance Pi a => Pi (Maybe a) where
    data Ty (Maybe a) :: Maybe a -> Type where
        NothingTy :: Ty (Maybe a) 'Nothing
        JustTy :: Ty a x -> Ty (Maybe a) ('Just x)
    depi NothingTy = Nothing
    depi (JustTy x) = Just (depi x)
