module Category.Monoidal
    ( TensorProduct, Unit, unitObj, prodObj, prodIL, prodIR, prodEL, prodER, prodAL, prodAR
    , prodIL_, prodIR_, prodEL_, prodER_, prodAL_, prodAR_
    , Compose (Compose), getCompose
    , Identity (Identity), getIdentity
    ) where

import Category.Base
import Category.Functor
import Category.Product
import Data.Either (Either (Left, Right))
import Data.Kind (Constraint, FUN, Type)
import Data.Void (Void)

-- | A category is monoidal if it has a product and a unit for that product.
-- A category can have multiple tensor products and be monoidal in multiple ways,
-- including the category of types itself,
-- so instead of using a @Monoidal@ typeclass, we use a @TensorProduct@ typeclass.
type TensorProduct :: (i -> i -> Type) -> (i -> i -> i) -> Constraint
class Bifunctor morph morph morph prod => TensorProduct (morph :: i -> i -> Type) prod where
    type Unit morph prod :: i
    -- | The unit is an object.
    unitObj :: proxy prod -> Obj morph (Unit morph prod)
    default unitObj :: NiceCat morph => proxy prod -> Obj morph (Unit morph prod)
    unitObj _ = id

    -- | Given two objects, their product is also an object.
    prodObj :: Obj morph a -> Obj morph b -> Obj morph (prod a b)
    default prodObj :: NiceCat morph => Obj morph a -> Obj morph b -> Obj morph (prod a b)
    prodObj _ _ = id

    -- | Introduce a product with the value injected into the left side.
    prodIL :: Obj morph a -> morph a (prod a (Unit morph prod))
    -- | Introduce a product with the value injected into the right side.
    prodIR :: Obj morph a -> morph a (prod (Unit morph prod) a)
    -- | Eliminate a product with the value projected from the left side.
    prodEL :: Obj morph a -> morph (prod a (Unit morph prod)) a
    -- | Eliminate a product with the value projected from the right side.
    prodER :: Obj morph a -> morph (prod (Unit morph prod) a) a
    -- | Reassociate a product, nesting it to the left.
    prodAL :: Obj morph a -> Obj morph b -> Obj morph c -> morph (prod a (prod b c)) (prod (prod a b) c)
    -- | Reassociate a product, nesting it to the right.
    prodAR :: Obj morph a -> Obj morph b -> Obj morph c -> morph (prod (prod a b) c) (prod a (prod b c))

prodIL_ :: (NiceCat morph, TensorProduct morph prod) => morph a (prod a (Unit morph prod))
prodIL_ = prodIL id

prodIR_ :: (NiceCat morph, TensorProduct morph prod) => morph a (prod (Unit morph prod) a)
prodIR_ = prodIR id

prodEL_ :: (NiceCat morph, TensorProduct morph prod) => morph (prod a (Unit morph prod)) a
prodEL_ = prodEL id

prodER_ :: (NiceCat morph, TensorProduct morph prod) => morph (prod (Unit morph prod) a) a
prodER_ = prodER id

prodAL_ :: (NiceCat morph, TensorProduct morph prod) => morph (prod a (prod b c)) (prod (prod a b) c)
prodAL_ = prodAL id id id

prodAR_ :: (NiceCat morph, TensorProduct morph prod) => morph (prod (prod a b) c) (prod a (prod b c))
prodAR_ = prodAR id id id

instance TensorProduct (FUN m) (,) where
    type Unit (FUN m) (,) = ()
    prodIL _ = \x -> (x, ())
    prodIR _ = \x -> ((), x)
    prodEL _ = \(x, ()) -> x
    prodER _ = \((), x) -> x
    prodAL _ _ _ = \(x, (y, z)) -> ((x, y), z)
    prodAR _ _ _ = \((x, y), z) -> (x, (y, z))

absurd :: Void %1-> a
absurd = \case{}

instance TensorProduct (FUN m) Either where
    type Unit (FUN m) Either = Void
    prodIL _ = Left
    prodIR _ = Right
    prodEL _ = \case
        Left x -> x
        Right x -> absurd x
    prodER _ = \case
        Left x -> absurd x
        Right x -> x
    prodAL _ _ _ = \case
        Left x -> Left (Left x)
        Right (Left x) -> Left (Right x)
        Right (Right x) -> Right x
    prodAR _ _ _ = \case
        Left (Left x) -> Left x
        Left (Right x) -> Right (Left x)
        Right x -> Right (Right x)

data Compose f g x = (Functor (->) (->) f, Functor (->) (->) g) => Compose { getCompose :: !(f (g x)) }
newtype Identity x = Identity { getIdentity :: x }

instance Functor (FUN m) (FUN m) Identity where
    map f = \(Identity x) -> Identity (f x)

instance Functor (Nat (Nat (->) (->)) (Nat (->) (->))) (Nat (->) (->)) Compose where
    map (Nat f) = Nat \(Nat _) -> Nat_ \(Compose x) -> Compose (f id x)

instance Functor (Nat (->) (->)) (Nat (->) (->)) (Compose f) where
    map (Nat f) = Nat_ \(Compose x) -> Compose (map (f id) x)

instance Functor (->) (->) (Compose (f :: Type -> Type) g) where
    map f = \(Compose x) -> Compose (map @_ @_ @_ @(->) (map f) x)

instance TensorProduct (Nat (->) (->)) Compose where
    type Unit (Nat (->) (->)) Compose = Identity
    unitObj _ = natId
    prodObj _ _ = natId
    prodIL (Nat _) = Nat_ \x -> Compose (map Identity x)
    prodIR (Nat _) = Nat_ \x -> Compose (Identity x)
    prodEL (Nat _) = Nat_ \(Compose x) -> map getIdentity x
    prodER (Nat _) = Nat_ \(Compose (Identity x)) -> x
    prodAL (Nat _) (Nat _) (Nat _) = Nat_ \(Compose x) -> Compose (Compose (map getCompose x))
    prodAR (Nat _) (Nat _) (Nat _) = Nat_ \(Compose (Compose x)) -> Compose (map Compose x)
