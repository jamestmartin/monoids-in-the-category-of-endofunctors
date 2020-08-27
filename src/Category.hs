{-# LANGUAGE UndecidableSuperClasses #-}
module Category where

import Data.Kind (Constraint)

data Dict c where
    Dict :: c => Dict c

data Proxy a where
    Proxy :: Proxy a

newtype Identity a = Identity { getIdentity :: a }

class Category (hom :: k -> k -> *) (obj :: k -> Constraint) where
    identity :: obj a => proxy obj -> a `hom` a
    compose :: (obj a, obj b, obj c) => proxy obj -> b `hom` c -> a `hom` b -> a `hom` c

instance Category (->) obj where
    identity _ x = x
    compose _ f g x = f (g x)

class MonoidalObjects (obj :: k -> Constraint) where
    type Unit obj :: k
    type Product obj :: k -> k -> k

instance MonoidalObjects (obj :: * -> Constraint) where
    type Unit obj = ()
    type Product obj = (,)

class MonoidalObjects obj => ContainsMonoidalObjects obj where
    unitObj :: Dict (obj (Unit obj))
    productObjFactor :: (obj a, obj b) => Dict (obj (Product obj a b))
    productObjDistribute :: obj (Product obj a b) => Dict (obj a, obj b)

class EveryC a
instance EveryC a

instance ContainsMonoidalObjects (EveryC :: * -> Constraint) where
    unitObj = Dict
    productObjFactor = Dict
    productObjDistribute = Dict

class (Category hom obj, ContainsMonoidalObjects obj) => MonoidalCategory (hom :: k -> k -> *) (obj :: k -> Constraint) where
    idLI :: obj a => proxy obj -> a `hom` Product obj a (Unit obj)
    idLE :: obj a => proxy obj -> Product obj a (Unit obj) `hom` a
    idRI :: obj a => proxy obj -> a `hom` Product obj (Unit obj) a
    idRE :: obj a => proxy obj -> Product obj (Unit obj) a `hom` a

    assocL :: (obj a, obj b, obj c) => proxy obj -> Product obj a (Product obj b c) `hom` Product obj (Product obj a b) c
    assocR :: (obj a, obj b, obj c) => proxy obj -> Product obj (Product obj a b) c `hom` Product obj a (Product obj b c)

instance ContainsMonoidalObjects obj => MonoidalCategory (->) obj where
    idLI _ x = (x, ())
    idLE _ (x, ()) = x
    idRI _ x = ((), x)
    idRE _ ((), x) = x

    assocL _ (x, (y, z)) = ((x, y), z)
    assocR _ ((x, y), z) = (x, (y, z))

class (MonoidalCategory overHom overObj) => EnrichedCategory overHom (overObj :: k -> Constraint) (hom :: j -> j -> k) (obj :: j -> Constraint) where
    homObj :: (obj a, obj b) => proxy overHom -> proxy' obj -> Dict (overObj (a `hom` b))
    enrichedIdentity :: obj a => proxy overObj -> proxy' obj -> Unit overObj `overHom` (a `hom` a)
    enrichedCompose :: (obj a, obj b, obj c) => proxy overObj -> proxy' obj -> Product overObj (b `hom` c) (a `hom` b) `overHom` (a `hom` c)

instance Category hom obj => EnrichedCategory (->) EveryC hom obj where
    homObj _ _ = Dict
    enrichedIdentity _ px () = identity px
    enrichedCompose _ px (f, g) = compose px f g

class (c a, d a) => ProductCI c d a
instance (c a, d a) => ProductCI c d a
