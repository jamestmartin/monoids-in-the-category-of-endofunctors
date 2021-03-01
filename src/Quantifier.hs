{-# LANGUAGE UndecidableInstances #-}
-- | Dependent quantification is when a function can use the function's argument
-- both in to compute the result and as part of a type signature.
--
-- Consider this function:
--
-- > replicate :: forall a. pi (n :: Nat) -> a -> Vec a n`
-- > replicate (S n) x = x : replicate n x
--
-- The argument @n@ determines both the type of the result (@Vec a n@)
-- and the number of times @x@ is replicated (via recursion).
--
-- GHC Haskell does not currently natively support dependent types,
-- so instead we have to approximate them through "singletons"
-- (not to be confused with the singleton pattern from OOP).
--
-- The purpose of this module is to add a typeclass for singletons
-- to make approximating dependent quantifiers in Haskell
-- more consistent and general.
--
-- Unfortunately, the techique used here requires a lot of boilerplate,
-- but it's the best option we currently have.
--
-- This technique was first described in [Conor McBride's Hasochism paper](https://personal.cis.strath.ac.uk/conor.mcbride/pub/hasochism.pdf);
-- the idea to make a `Pi` typeclass was my own, but I'm sure it's been done before.
module Quantifier where

import Data.Kind (Constraint, Type)
import Data.Maybe (Maybe (Nothing, Just))

-- | An explicit non-dependent universal quantifier.
type Forall :: (a -> Type) -> Type
newtype Forall f = Forall { runForall :: forall proxy x. proxy x -> f x }

-- | A typeclass faciliating explicit dependent quantifiers.
class Pi (ty :: Type) where
    -- | The type family @`Ty` ty :: ty -> `Type`@ is isomorphic to @ty@,
    -- with each value @x :: ty@ corresponding with a singleton type @`Ty` ty x@.
    data Ty ty :: ty -> Type
    -- | Takes the unique value of @'Ty' ty x@ and returns the corresponding value of @x@ in @ty@.
    depi :: forall x. Ty ty x -> ty

-- | A typeclass faciliating implicit dependent quantifiers.
class Pi ty => PiC (ty :: Type) where
    -- | Because @'Ty' ty x@ has a unique value, we can define a typeclass @'TyC' ty x@
    -- with a unique instances which gives us that unique value.
    -- Thus, if the type inference system can infer the value of @x@ on the type level,
    -- we can get @x@ on the value level as well.
    type TyC ty :: ty -> Constraint
    -- | Get the unique value of the singleton type @'Ty' ty x@.
    depic :: forall x. TyC ty x => Ty ty x

instance Pi a => Pi (Maybe a) where
    data Ty (Maybe a) :: Maybe a -> Type where
        NothingTy :: Ty (Maybe a) 'Nothing
        JustTy :: Ty a x -> Ty (Maybe a) ('Just x)
    depi NothingTy = Nothing
    depi (JustTy x) = Just (depi x)

-- | The typeclass @'TyC' ('Maybe' a)@, which must be defined as its own typeclass
-- because associated typeclasses aren't a thing.
class MaybeTyC (x :: Maybe a) where
    depic_maybe :: Ty (Maybe a) x

instance MaybeTyC 'Nothing where
    depic_maybe = NothingTy

instance (PiC a, TyC a x) => MaybeTyC ('Just x) where
    depic_maybe = JustTy depic

instance PiC a => PiC (Maybe a) where
    type TyC (Maybe a) = MaybeTyC
    depic = depic_maybe
