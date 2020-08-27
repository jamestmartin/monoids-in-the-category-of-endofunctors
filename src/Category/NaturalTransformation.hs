module Category.NaturalTransformation where

import Category
import Category.Functor
import Data.Kind (Constraint)
import Prelude (($), undefined)

data Nat domHom domObj codHom codObj f g where
    Nat :: (Functor domHom domObj codHom codObj f, Functor domHom domObj codHom codObj g) => (forall a. domObj a => f a `codHom` g a) -> Nat domHom domObj codHom codObj f g

instance (Category domHom domObj, Category codHom codObj) => Category (Nat domHom domObj codHom codObj) (ProductCI (Functor domHom domObj codHom codObj) obj) where
    -- This type signature is just boilerplate to get `f` into scope, which is necessary to get the `codObj (f a)` instance.
    identity :: forall f proxy. ProductCI (Functor domHom domObj codHom codObj) obj f => proxy (ProductCI (Functor domHom domObj codHom codObj) obj) -> Nat domHom domObj codHom codObj f f
    identity _ = Nat f
        where f :: forall a. domObj a => f a `codHom` f a
              f = case mapObj (Proxy @domHom) (Proxy @domObj) (Proxy @codHom) :: Dict (codObj (f a)) of Dict -> identity (Proxy @codObj)

    compose :: forall a b c proxy. (ProductCI (Functor domHom domObj codHom codObj) obj a, ProductCI (Functor domHom domObj codHom codObj) obj b, ProductCI (Functor domHom domObj codHom codObj) obj c) => proxy (ProductCI (Functor domHom domObj codHom codObj) obj) -> Nat domHom domObj codHom codObj b c -> Nat domHom domObj codHom codObj a b -> Nat domHom domObj codHom codObj a c
    compose _ (Nat f) (Nat g) = Nat fg
        where fg :: forall x. domObj x => a x `codHom` c x
              fg = case mapObj' (Proxy @a) of
                  Dict -> case mapObj' (Proxy @b) of
                      Dict -> case mapObj' (Proxy @c) of
                          Dict -> compose (Proxy @codObj) f g
                where mapObj' :: forall f proxy. Functor domHom domObj codHom codObj f => proxy f -> Dict (codObj (f x))
                      mapObj' _ = mapObj (Proxy @domHom) (Proxy @domObj) (Proxy @codHom) :: Dict (codObj (f x))

data ComposeEndofunctor hom obj f g x where
    ComposeEndofunctor :: { getComposeEndofunctor :: (Endofunctor hom obj f, Endofunctor hom obj g, obj x) => f (g x) } -> ComposeEndofunctor hom obj f g x

instance {-# INCOHERENT #-} (Category (->) obj, Endofunctor (->) obj f, Endofunctor (->) obj g, forall a. obj (ComposeEndofunctor (->) obj f g a)) => Functor (->) obj (->) obj (ComposeEndofunctor (->) obj f g) where
    mapObj _ _ _ = Dict

    map :: forall a b proxy proxy'. (obj a, obj b) => proxy obj -> proxy' obj -> (a -> b) -> ComposeEndofunctor (->) obj f g a -> ComposeEndofunctor (->) obj f g b
    map _ _ f (ComposeEndofunctor x) = ComposeEndofunctor $ case mapObj' (Proxy @a) of
        Dict -> case mapObj' (Proxy @b) of
            Dict -> endomap (Proxy @obj) (endomap (Proxy @obj) f) x
      where mapObj' :: forall x proxy. (Endofunctor (->) obj f, obj x) => proxy x -> Dict (obj (g x))
            mapObj' _ = mapObj (Proxy @(->)) (Proxy @obj) (Proxy @(->))

instance Category hom obj => MonoidalObjects (Endofunctor hom (obj :: * -> Constraint)) where
    type Unit (Endofunctor hom obj) = Identity
    type Product (Endofunctor hom obj) = ComposeEndofunctor hom obj

instance ContainsMonoidalObjects (Endofunctor (->) (EveryC :: * -> Constraint)) where
    unitObj = Dict
    productObjFactor = Dict
    -- FIXME: Implement this
    productObjDistribute = undefined

instance (obj ~ EveryC, hom ~ Nat (->) obj (->) obj, Category hom (Endofunctor (->) obj)) => MonoidalCategory hom (Endofunctor (->) obj) where
    idLI _ = Nat \x -> ComposeEndofunctor (endomap (Proxy @obj) Identity x)
    idLE _ = Nat \(ComposeEndofunctor x) -> endomap (Proxy @obj) getIdentity x
    idRI _ = Nat \x -> ComposeEndofunctor (Identity x)
    idRE _ = Nat \(ComposeEndofunctor (Identity x)) -> x

    assocL _ = Nat \(ComposeEndofunctor x) -> ComposeEndofunctor (ComposeEndofunctor (endomap (Proxy @obj) getComposeEndofunctor x))
    -- FIXME: Finish implementing this
    assocR _ = Nat \(ComposeEndofunctor (ComposeEndofunctor x)) -> ComposeEndofunctor (endomap (Proxy @obj) ComposeEndofunctor undefined)
