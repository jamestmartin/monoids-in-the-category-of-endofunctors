{-# LANGUAGE AllowAmbiguousTypes #-}
module Category.Monoidal where

import Category
import Category.Constraint
import Category.Functor
import Category.Functor.Identity
import Category.Nat
import Data.Kind (Constraint)
import Prelude (($), undefined)
import Unsafe.Coerce

class Category cat => MonoidalCategory (cat :: i -> Constraint) where
    type Product :: i -> i -> i
    type Unit :: i

    assocL :: (cat a, cat b, cat c) => Dom cat (Product a (Product b c)) (Product (Product a b) c)
    assocR :: (cat a, cat b, cat c) => Dom cat (Product (Product a b) c) (Product a (Product b c))

    idLI :: cat a => Dom cat a (Product a Unit)
    idLE :: cat a => Dom cat (Product a Unit) a
    idRI :: cat a => Dom cat a (Product Unit a)
    idRE :: cat a => Dom cat (Product Unit a) a

instance (Dom cat ~ (->)) => MonoidalCategory cat where
    type Product = (,)
    type Unit = ()

    assocL (x, (y, z)) = ((x, y), z)
    assocR ((x, y), z) = (x, (y, z))

    idLI x = (x, ())
    idLE (x, ()) = x
    idRI x = ((), x)
    idRE ((), x) = x

-- r :: (i -> k) -> Constraint
instance (Category cat, Dom cat ~ Nat) => MonoidalCategory (cat :: (* -> *) -> Constraint) where
    -- FIXME: This instance can be broken by GADTs!
    -- You need a functor, contrafunctor, or invariant functor instance for this to be valid,
    -- but we can't express the category of functors yet so that'll need to be done later.
    type Product = Compose
    type Unit = Identity

    assocL = Nat unsafeCoerce
    assocR = Nat unsafeCoerce

    idLI = Nat unsafeCoerce
    idLE = Nat unsafeCoerce
    idRI = Nat unsafeCoerce
    idRE = Nat unsafeCoerce

class (a, b) => ProductC a b
instance (a, b) => ProductC a b

class (a i, b i) => ProductCI a b i
instance (a i, b i) => ProductCI a b i

class UnitC
instance UnitC

instance (Dom cat ~ (:-)) => MonoidalCategory cat where
    type Product = ProductC
    type Unit = NoC

    assocL = Sub Dict
    assocR = Sub Dict

    idLI = Sub Dict
    idLE = Sub Dict
    idRI = Sub Dict
    idRE = Sub Dict
