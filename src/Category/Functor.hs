{-# LANGUAGE UndecidableInstances #-}
module Category.Functor where

import Category.Base
import Data.Kind (Type)
import Data.Maybe (Maybe (Nothing, Just))
import Quantifier

class (Category (Dom f), Category (Cod f)) => Functor (f :: i -> j) where
    type Dom f :: i -> i -> Type
    type Cod f :: j -> j -> Type
    map :: Dom f a b -> Cod f (f a) (f b)

class (Functor f, Dom f ~ q, Cod f ~ r) => FunctorOf q r f
instance (Functor f, Dom f ~ q, Cod f ~ r) => FunctorOf q r f

class (Functor f, Dom f ~ Cod f) => Endofunctor f
instance (Functor f, Dom f ~ Cod f) => Endofunctor f

data Nat (q :: i -> i -> Type) (r :: j -> j -> Type) (f :: i -> j) (g :: i -> j) :: Type where
    Nat :: (FunctorOf q r f, FunctorOf q r g) => (forall a. Obj q a -> r (f a) (g a)) -> Nat q r f g

runNat :: Nat q r f g -> forall a. Obj q a -> r (f a) (g a)
runNat (Nat f) = f

instance Semigroupoid r => Semigroupoid (Nat q r) where
    (.) (Nat f) (Nat g) = Nat \p -> (f p . g p)

instance Category r => Category (Nat q r) where
    src (Nat f) = Nat (src . f)
    tgt (Nat f) = Nat (tgt . f)

instance Functor (->) where
    type Dom (->) = Op (->)
    type Cod (->) = Nat (->) (->)
    map (Op f) = Nat \_ -> (. f)

instance Functor ((->) a) where
    type Dom ((->) a) = (->)
    type Cod ((->) a) = (->)
    map f g = f . g

instance Functor (,) where
    type Dom (,) = (->)
    type Cod (,) = Nat (->) (->)
    map f = Nat \_ (x, y) -> (f x, y)

instance Functor ((,) a) where
    type Dom ((,) a) = (->)
    type Cod ((,) a) = (->)
    map f (x, y) = (x, f y)

--
type family NatDom (f :: (i -> j) -> (i -> j) -> Type) :: (i -> i -> Type) where
    NatDom (Nat p _) = p

type family NatCod (f :: (i -> j) -> (i -> j) -> Type) :: (j -> j -> Type) where
    NatCod (Nat _ q) = q

type Dom2 p = NatDom (Cod p)
type Cod2 p = NatCod (Cod p)

class (Functor p, Cod p ~ Nat (Dom2 p) (Cod2 p), Category (Dom2 p), Category (Cod2 p)) => Bifunctor (p :: i -> j -> k)
instance (Functor p, Cod p ~ Nat (Dom2 p) (Cod2 p), Category (Dom2 p), Category (Cod2 p)) => Bifunctor (p :: i -> j -> k)

newtype Const (cat :: i -> i -> Type) (a :: Type) (b :: i) = Const { getConst :: a }

instance Category cat => Functor (Const cat a) where
    -- FIXME
    type Dom (Const cat a) = cat
    type Cod (Const cat a) = (->)
    map _ (Const x) = Const x

instance Category cat => Functor (Const cat) where
    type Dom (Const cat) = (->)
    type Cod (Const cat) = Nat cat (->)
    map f = Nat \_ -> \case (Const x) -> Const (f x)

instance Functor Maybe where
    type Dom Maybe = (->)
    type Cod Maybe = (->)
    map _ Nothing = Nothing
    map f (Just x) = Just (f x)

instance {-# OVERLAPPABLE #-} Pi ty => Functor (Ty ty) where
    type Dom (Ty ty) = (:~:)
    type Cod (Ty ty) = (->)
    map Refl x = x
