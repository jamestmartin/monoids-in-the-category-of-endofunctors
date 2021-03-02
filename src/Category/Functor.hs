{-# LANGUAGE UndecidableInstances #-}
module Category.Functor
    ( Functor, map
    , Endo, Endofunctor, endomap
    , Contravariant, contramap
    , Bifunctor, bimap, first, second
    , Profunctor, dimap, lmap, rmap
    , Nat (Nat), runNat, pattern Nat_, natId
    , Const (Const), getConst
    ) where

import Category.Base
import Data.Either (Either (Left, Right))
import Data.Kind (Constraint, FUN, Type)
import Data.Maybe (Maybe (Nothing, Just))
import Data.Proxy

type Functor :: (j -> j -> Type) -> (i -> i -> Type) -> (i -> j) -> Constraint
class (Category dest, Category src) => Functor dest src f where
    map :: src a b -> dest (f a) (f b)

type Endo f a = f a a
type Endofunctor :: (i -> i -> Type) -> (i -> i) -> Constraint
class Endo Functor morph f => Endofunctor morph f where
instance Endo Functor morph f => Endofunctor morph f
endomap :: Endofunctor morph f => morph a b -> morph (f a) (f b)
endomap = map

class Contravariant dest src f where
    contramap :: src b a -> dest (f a) (f b)

instance {-# INCOHERENT #-} Functor dest (Yoneda src) f => Contravariant dest src f where
    contramap f = map (Op f)

instance {-# INCOHERENT #-} Functor dest src f => Contravariant dest (Yoneda src) f where
    contramap (Op f) = map f

type Bifunctor :: (j -> j -> Type) -> (i -> i -> Type) -> (i -> i -> j) -> Constraint
class (Functor (Nat dest src) src f, forall x. Functor dest src (f x)) => Bifunctor dest src f
instance (Functor (Nat dest src) src f, forall x. Functor dest src (f x)) => Bifunctor dest src f
bimap :: Bifunctor dest src f => src a c -> src b d -> dest (f a b) (f c d)
bimap f g = runNat (map f) (idR g) . map g
-- FIXME: A NiceCat dependency should not be necessary here,
--   this most likely means that my definition of Bifunctor is inadequate.
first :: forall dest src f a b c. (Bifunctor dest src f, NiceCat src) => src a b -> dest (f a c) (f b c)
first f = runNat (map f) (id :: Obj src c)
second :: Bifunctor dest src f => src b c -> dest (f a b) (f a c)
second g = map g

type Profunctor :: (j -> j -> Type) -> (i -> i -> Type) -> (i -> i -> j) -> Constraint
class (Functor (Nat dest src) (Yoneda src) f, forall x. Functor dest src (f x)) => Profunctor dest src f
instance (Functor (Nat dest src) (Yoneda src) f, forall x. Functor dest src (f x)) => Profunctor dest src f
dimap :: Profunctor dest src f => src a b -> src c d -> dest (f b c) (f a d)
dimap f g = runNat (map (Op f)) (idR g) . map g
lmap :: forall dest src f a b c. (Profunctor dest src f, NiceCat src) => src a b -> dest (f b c) (f a c)
lmap f = runNat (map (Op f)) (id :: Obj src c)
rmap :: Profunctor dest src f => src b c -> dest (f a b) (f a c)
rmap f = map f

type Nat :: (j -> j -> Type) -> (i -> i -> Type) -> (i -> j) -> (i -> j) -> Type
data Nat dest src f g = (Functor dest src f, Functor dest src g) => Nat { runNat :: !(forall a. Obj src a -> dest (f a) (g a)) }

type Nat_' :: (j -> j -> Type) -> (i -> i -> Type) -> (i -> j) -> (i -> j) -> Type
data Nat_' dest src f g = (Functor dest src f, Functor dest src g) => Nat_' !(forall a. dest (f a) (g a))

nat_' :: NiceCat src => Nat dest src f g -> Nat_' dest src f g
nat_' (Nat f) = Nat_' (f id)

pattern Nat_ :: forall dest src f g. NiceCat src => (Functor dest src f, Functor dest src g) => (forall a. dest (f a) (g a)) -> Nat dest src f g
{-# COMPLETE Nat_ #-}
pattern Nat_ f <- (nat_' -> Nat_' f)
    where Nat_ f = Nat \_ -> f

instance Category (Nat dest src) where
    idL (Nat _) = Nat map
    idR (Nat _) = Nat map
    Nat f . Nat g = Nat \x -> (f x . g x)

natId :: Functor dest src f => Obj (Nat dest src) f
natId = Nat map

instance (forall f. Functor dest src f) => NiceCat (Nat dest src) where
    id = natId

instance Functor (->) (FUN m) (FUN m a) where
    map f = \g -> f . g

instance Functor (Nat (->) (FUN m)) (Yoneda (FUN m)) (FUN m) where
    map (Op f) = Nat \_ g -> g . f

instance Functor (Nat (FUN m) (FUN m)) (FUN m) (,) where
    map f = Nat \_ (x, y) -> (f x, y)

instance Functor (FUN m) (FUN m) ((,) a) where
    map f = \(x, y) -> (x, f y)

instance Functor (Nat (FUN m) (FUN m)) (FUN m) Either where
    map f = Nat \_ -> \case
        Left y -> Left (f y)
        Right x -> Right x

instance Functor (FUN m) (FUN m) (Either a) where
    map f = \case
        Left y -> Left y
        Right x -> Right (f x)

instance Functor (FUN m) (FUN m) Maybe where
    map f = \case
        Nothing -> Nothing
        Just x -> Just (f x)

instance {-# INCOHERENT #-} Category src => Functor (FUN m) src Proxy where
    map _ = \Proxy -> Proxy

type Const :: Type -> i -> Type
newtype Const a b = Const { getConst :: a }

instance Functor (Nat (->) (->)) (->) Const where
    map f = Nat \_ (Const x) -> Const (f x)

instance {-# INCOHERENT #-} Category src => Functor (->) src (Const a) where
    map _ = \(Const x) -> Const x
