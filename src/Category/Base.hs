module Category.Base where

import Data.Dict
import Data.Kind (Constraint, FUN, Type)
import GHC.Types (Multiplicity)

-- | A one-parameter typeclass which is always implemented.
type Unconst1 :: i -> Constraint
class Unconst1 a
instance Unconst1 a

type Category :: forall i. (i -> i -> Type) -> Constraint
-- Scoped type variables does not make `i` in scope with standalone kind signatures,
-- so this redundant type annotation is necessary.
class Category (morph :: i -> i -> Type) where
    type Obj morph :: i -> Constraint
    type Obj _morph = Unconst1
    obj :: morph a b -> Dict (Obj morph a, Obj morph b)
    default obj :: forall a b. Obj morph ~ Unconst1 => morph a b -> Dict (Obj morph a, Obj morph b)
    obj _ = Dict

    id :: Obj morph a => morph a a
    -- | Associative composition of morphisms.
    (.) :: morph b c -> morph a b -> morph a c

instance forall (m :: Multiplicity). Category (FUN m) where
    id = \x -> x
    f . g = \x -> f (g x)

type Yoneda :: (i -> i -> Type) -> i -> i -> Type
newtype Yoneda morph a b = Op { getOp :: morph b a }

instance Category morph => Category (Yoneda morph) where
    type Obj (Yoneda morph) = Obj morph
    obj (Op f) = case obj f of Dict -> Dict
    id = Op id
    Op f . Op g = Op (g . f)

type Op :: (i -> i -> Type) -> i -> i -> Type
type family Op morph where
    Op (Yoneda morph) = morph
    Op morph = Yoneda morph
