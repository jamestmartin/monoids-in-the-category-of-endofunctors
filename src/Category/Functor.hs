{-# LANGUAGE UndecidableInstances #-}
module Category.Functor where

import Category.Base
import Data.Dict
import Data.Either (Either (Left, Right))
import Data.Kind (Constraint, FUN, Type)
import Data.Maybe (Maybe (Nothing, Just))
import Data.Proxy

type Functor :: (j -> j -> Type) -> (i -> i -> Type) -> (i -> j) -> Constraint
class (Category dest, Category src) => Functor dest src f where
    map_ :: Obj src a => proxy dest -> proxy' src -> Dict (Obj dest (f a))
    default map_ :: forall proxy proxy' a. Obj dest ~ Unconst1 => proxy dest -> proxy' src -> Dict (Obj dest (f a))
    map_ _ _ = Dict
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

-- FIXME:
--   I'm not convinced this definition is sufficient.
--   Don't feel like explaining why. Hopefully it won't be too long before I fix it.
--   I'd hate to forget~
type Bifunctor :: (j -> j -> Type) -> (i -> i -> Type) -> (i -> i -> j) -> Constraint
class (Functor (Nat dest src) src f, forall x. Functor dest src (f x)) => Bifunctor dest src f
instance (Functor (Nat dest src) src f, forall x. Functor dest src (f x)) => Bifunctor dest src f
bimap :: forall dest src f a b c d. Bifunctor dest src f => src a c -> src b d -> dest (f a b) (f c d)
bimap f g = case obj g of Dict -> runNat @_ @_ @dest @src (map f) . map g
--first :: forall dest src f a b c d. Bifunctor dest src f => src a b -> dest (f a c) (f b c)
--first f = runNat @_ @_ @dest @src (map f)
second :: Bifunctor dest src f => src b c -> dest (f a b) (f a c)
second g = map g

type Profunctor :: (j -> j -> Type) -> (i -> i -> Type) -> (i -> i -> j) -> Constraint
class (Functor (Nat dest src) (Yoneda src) f, forall x. Functor dest src (f x)) => Profunctor dest src f
instance (Functor (Nat dest src) (Yoneda src) f, forall x. Functor dest src (f x)) => Profunctor dest src f
dimap :: forall dest src f a b c d. Profunctor dest src f => src a b -> src c d -> dest (f b c) (f a d)
dimap f g = case obj g of Dict -> runNat @_ @_ @dest @src (map (Op f)) . map g

type Nat :: (j -> j -> Type) -> (i -> i -> Type) -> (i -> j) -> (i -> j) -> Type
data Nat dest src f g = (Functor dest src f, Functor dest src g) => Nat { runNat :: !(forall a. Obj src a => dest (f a) (g a)) }

instance Category dest => Category (Nat dest src) where
    type Obj (Nat dest src) = Functor dest src
    obj (Nat _) = Dict
    id :: forall f. Obj (Nat dest src) f => Nat dest src f f
    id = Nat id'
        where id' :: forall a. Obj src a => dest (f a) (f a)
              id' = case map_ (Proxy @dest) (Proxy @src) :: Dict (Obj dest (f a)) of Dict -> id
    Nat f . Nat g = Nat (f . g)

instance Functor (->) (FUN m) (FUN m a) where
    map f = \g -> f . g

instance Functor (Nat (->) (FUN m)) (Yoneda (FUN m)) (FUN m) where
    map_ _ _ = Dict
    map (Op f) = Nat \g -> g . f

instance Functor (Nat (FUN m) (FUN m)) (FUN m) (,) where
    map_ _ _ = Dict
    map f = Nat \(x, y) -> (f x, y)

instance Functor (FUN m) (FUN m) ((,) a) where
    map f = \(x, y) -> (x, f y)

instance Functor (Nat (FUN m) (FUN m)) (FUN m) Either where
    map_ _ _ = Dict
    map f = Nat \case
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
    map_ _ _ = Dict
    map f = Nat \(Const x) -> Const (f x)

instance {-# INCOHERENT #-} Category src => Functor (->) src (Const a) where
    map _ = \(Const x) -> Const x
