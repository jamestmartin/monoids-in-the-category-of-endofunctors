module Category.Base where

import Data.Kind (Type)

type Obj q a = a `q` a

class Semigroupoid (q :: i -> i -> Type) where
    (.) :: b `q` c -> a `q` b -> a `q` c

instance Semigroupoid (->) where
    (.) f g x = f (g x)

class Semigroupoid q => Category q where
    src :: a `q` b -> Obj q a
    tgt :: a `q` b -> Obj q b

instance Category (->) where
    src _ x = x
    tgt _ x = x

class Category q => Groupoid q where
    inv :: a `q` b -> b `q` a

newtype Yoneda (q :: i -> i -> Type) (a :: i) (b :: i) = Op { getOp :: b `q` a }

instance Semigroupoid q => Semigroupoid (Yoneda q) where
    Op f . Op g = Op (g . f)

instance Category q => Category (Yoneda q) where
    src (Op f) = Op (tgt f)
    tgt (Op f) = Op (src f)

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
    src _ = Refl
    tgt _ = Refl

instance Groupoid (:~:) where
    inv Refl = Refl

data Unit a b = Unit

instance Semigroupoid Unit where
    Unit . Unit = Unit

instance Category Unit where
    src _ = Unit
    tgt _ = Unit

instance Groupoid Unit where
    inv _ = Unit
