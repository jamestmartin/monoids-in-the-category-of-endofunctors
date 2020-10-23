module Category.Base where

import Data.Dict (Dict (Dict))
import Data.Kind (Constraint, Type)

class Vacuous (a :: i)
instance Vacuous (a :: i)

class Bottom (a :: i) where
    bottom :: forall b proxy. proxy a -> b

class Semigroupoid (q :: i -> i -> Type) where
    type Obj q :: i -> Constraint
    type Obj _ = Vacuous
    observe :: a `q` b -> Dict (Obj q a, Obj q b)
    default observe :: Obj q ~ Vacuous => a `q` b -> Dict (Obj q a, Obj q b)
    observe _ = Dict
    (.) :: b `q` c -> a `q` b -> a `q` c

instance Semigroupoid (->) where
    (.) f g x = f (g x)

class Semigroupoid q => Category q where
    id :: Obj q a => a `q` a

instance Category (->) where
    id x = x

class Category q => Groupoid q where
    inv :: a `q` b -> b `q` a

newtype Yoneda (q :: i -> i -> Type) (a :: i) (b :: i) = Op { getOp :: b `q` a }

instance Semigroupoid q => Semigroupoid (Yoneda q) where
    type Obj (Yoneda q) = Obj q
    observe (Op f) = case observe f of Dict -> Dict
    Op f . Op g = Op (g . f)

instance Category q => Category (Yoneda q) where
    id = Op id

type family Op (q :: i -> i -> Type) :: i -> i -> Type where
    Op (Yoneda q) = q
    Op q = Yoneda q

type Post f = forall a. f a
type Endo f a = f a a

data (:~:) :: i -> i -> Type where
    Refl :: a :~: a

instance Semigroupoid (:~:) where
    Refl . Refl = Refl

instance Category (:~:) where
    id = Refl

instance Groupoid (:~:) where
    inv Refl = Refl
