module Category.Monoidal where

import Category.Base
import Category.Functor
import Data.Dict
import Data.Either (Either (Left, Right))
import Data.Kind (Constraint, FUN, Type)
import Data.Void (Void)

-- FIXME: Rename this.
-- | A category is monoidal if it has a product and a unit for that product.
-- A category can have multiple tensor products and be monoidal in multiple ways,
-- including the category of types itself,
-- so instead of using a @Monoidal@ typeclass, we use a @TensorProduct@ typeclass.
-- Only we actually don't, because I haven't had a chance to rename it yet.
type Monoidal :: (i -> i -> Type) -> (i -> i -> i) -> Constraint
class Endo Bifunctor morph prod => Monoidal (morph :: i -> i -> Type) prod where
    -- FIXME: Fix all of these garbage names. `uil`? `pal`? seriously?
    type Unit morph prod :: i
    prodObj :: (Obj morph a, Obj morph b) => proxy morph -> proxy' prod -> proxy'' a -> proxy''' b -> Dict (Obj morph (prod a b))
    default prodObj :: Obj morph ~ Unconst1 => proxy morph -> proxy' prod -> proxy'' a -> proxy''' b -> Dict (Obj morph (prod a b))
    prodObj _ _ _ _ = Dict
    unitObj :: proxy morph -> proxy' prod -> Dict (Obj morph (Unit morph prod))
    default unitObj :: Obj morph ~ Unconst1 => proxy morph -> proxy' prod -> Dict (Obj morph (Unit morph prod))
    unitObj _ _ = Dict
    uil :: Obj morph a => morph a (prod (Unit morph prod) a)
    uir :: Obj morph a => morph a (prod a (Unit morph prod))
    uel :: Obj morph a => morph (prod (Unit morph prod) a) a
    uer :: Obj morph a => morph (prod a (Unit morph prod)) a
    pal :: (Obj morph a, Obj morph b, Obj morph c) => morph (prod a (prod b c)) (prod (prod a b) c)
    par :: (Obj morph a, Obj morph b, Obj morph c) => morph (prod (prod a b) c) (prod a (prod b c))

instance Monoidal (FUN m) (,) where
    type Unit (FUN m) (,) = ()
    uil = \x -> ((), x)
    uir = \x -> (x, ())
    uel = \((), x) -> x
    uer = \(x, ()) -> x
    pal = \(x, (y, z)) -> ((x, y), z)
    par = \((x, y), z) -> (x, (y, z))

instance Monoidal (FUN m) Either where
    type Unit (FUN m) Either = Void
    uil = Right
    uir = Left
    uel (Left x) = (\case{}) x
    uel (Right x) = x
    uer (Left x) = x
    uer (Right x) = (\case{}) x
    pal (Left x) = Left (Left x)
    pal (Right (Left x)) = Left (Right x)
    pal (Right (Right x)) = Right x
    par (Left (Left x)) = Left x
    par (Left (Right x)) = Right (Left x)
    par (Right x) = Right (Right x)

data Compose f g x = (Functor (->) (->) f, Functor (->) (->) g) => Compose { getCompose :: !(f (g x)) }
newtype Identity x = Identity { getIdentity :: x }

instance Functor (FUN m) (FUN m) Identity where
    map f = \(Identity x) -> Identity (f x)

instance Functor (->) (->) (Compose f g) where
    map f = \(Compose x) -> Compose (map @_ @_ @(->) @(->) (map f) x)

instance Functor (Nat (Nat (->) (->)) (Nat (->) (->))) (Nat (->) (->)) Compose where
    map_ _ _ = Dict
    map (Nat f) = Nat (Nat \(Compose x) -> Compose (f x))

instance Functor (Nat (->) (->)) (Nat (->) (->)) (Compose f) where
    map_ _ _ = Dict
    map (Nat f) = Nat \(Compose x) -> Compose (map f x)

instance Monoidal (Nat (->) (->)) Compose where
    type Unit (Nat (->) (->)) Compose = Identity
    prodObj _ _ _ _ = Dict
    unitObj _ _ = Dict
    uil = Nat \x -> Compose (Identity x)
    uir = Nat \x -> Compose (map Identity x)
    uel = Nat \(Compose (Identity x)) -> x
    uer = Nat \(Compose x) -> map getIdentity x
    pal = Nat \(Compose x) -> Compose (Compose (map getCompose x))
    par = Nat \(Compose (Compose x)) -> Compose (map Compose x)
