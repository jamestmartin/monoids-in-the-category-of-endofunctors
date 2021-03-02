-- The default type signatures for `idL` and `idR` trigger
-- redundant constraints because `NiceCat` requires `Category`
-- but it's forced to require `Category` because that's where it's defined.
-- This seems like it might be a bug.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Category.Base
    ( Category, Obj, idL, idR, (.)
    , NiceCat, id
    , Yoneda (Op), Op, getOp
    ) where

import Data.Kind (Constraint, FUN, Type)

-- | Objects are uniquely identified by their identity arrow.
type Obj :: (i -> i -> j) -> i -> j
type Obj morph a = morph a a

type Category :: forall i. (i -> i -> Type) -> Constraint
class Category morph where
    idL :: morph a b -> Obj morph a
    default idL :: NiceCat morph => morph a b -> Obj morph a
    idL _ = id

    idR :: morph a b -> Obj morph b
    default idR :: NiceCat morph => morph a b -> Obj morph b
    idR _ = id

    -- | Associative composition of morphisms.
    (.) :: morph b c -> morph a b -> morph a c

type NiceCat :: forall i. (i -> i -> Type) -> Constraint
class Category morph => NiceCat morph where
    id :: Obj morph a

instance forall m. Category (FUN m) where
    f . g = \x -> f (g x)

instance forall m. NiceCat (FUN m) where
    id = \x -> x

type Yoneda :: (i -> i -> Type) -> i -> i -> Type
newtype Yoneda morph a b = Op { getOp :: morph b a }

type Op :: (i -> i -> Type) -> i -> i -> Type
type family Op morph where
    Op (Yoneda morph) = morph
    Op morph = Yoneda morph

instance Category morph => Category (Yoneda morph) where
    idL (Op f) = Op (idR f)
    idR (Op f) = Op (idL f)
    Op f . Op g = Op (g . f)

instance NiceCat morph => NiceCat (Yoneda morph) where
    id = Op id
