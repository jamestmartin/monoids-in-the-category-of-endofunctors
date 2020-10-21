{-# LANGUAGE PolyKinds #-}
module Category.Evil where

import Category.Good qualified as Good
import Category.Neutral qualified as Neutral
import Data.Dict (Dict (Dict), (:-) (Sub))
import Data.Kind (Constraint, Type)

class Category (hom :: i -> i -> Type) (obj :: i -> Constraint) where
    identity :: obj a => proxy obj -> hom a a
    compose :: (obj a, obj b, obj c) => proxy obj -> hom b c -> hom a b -> hom a c

instance Neutral.Category hom => Category hom obj where
    identity _ = Neutral.identity
    compose _ = Neutral.compose

class MonoidalObjects k where
    type Unit obj :: k
    type Product obj :: k -> k -> k

instance MonoidalObjects Type where
    type Unit Type = ()
    type Product Type = (,)

class MonoidalObjects i => ContainsMonoidalObjects (obj :: i -> Constraint) where
    monObjUnit :: Dict (obj (Unit i))
    monObjFactor :: (obj a, obj b) => Dict (obj (Product i a b))
    monObjDistribute :: obj (Product i a b) => Dict (obj a, obj b)

class (Category hom obj, ContainsMonoidalObjects obj) => Monoidal hom (obj :: i -> Constraint) where
    --monBi :: proxy cat -> Dict (BiEndo cat (Product cat))
    monIdLI :: obj a => proxy obj -> a `hom` Product i a (Unit i)
    monIdRI :: obj a => proxy obj -> a `hom` Product i (Unit i) a
    monIdLE :: obj a => proxy obj -> Product i a (Unit i) `hom` a
    monIdRE :: obj a => proxy obj -> Product i (Unit i) a `hom` a
    monAssocL :: (obj a, obj b, obj c) => proxy obj -> Product i (Product i a b) c `hom` Product i a (Product i b c)
    monAssocR :: (obj a, obj b, obj c) => proxy obj -> Product i a (Product i b c) `hom` Product i (Product i a b) c

instance (Neutral.Monoidal hom, ContainsMonoidalObjects obj, Neutral.Unit hom ~ Unit i, Neutral.Product hom ~ Product i) => Monoidal hom (obj :: i -> Constraint) where
    monIdLI _ = Neutral.monIdLI
    monIdRI _ = Neutral.monIdRI
    monIdLE _ = Neutral.monIdLE
    monIdRE _ = Neutral.monIdRE
    monAssocL _ = Neutral.monAssocL
    monAssocR _ = Neutral.monAssocR

class Category hom obj => Endofunctor hom obj f where
    endomapObj :: obj a => proxy hom -> Dict (obj (f a))

class Endofunctor hom obj f => CovariantEndo hom obj f where
    coendomap :: (obj a, obj b) => proxy obj -> hom a b -> hom (f a) (f b)

instance (Endofunctor hom obj f, Neutral.CovariantEndo hom f) => CovariantEndo hom obj f where
    coendomap _ = Neutral.coendomap

class (Category domHom domObj, Category codHom codObj) => Functor domHom domObj codHom codObj f where
    mapObj :: domObj a => proxy domHom -> proxy' domObj -> proxy'' codHom -> Dict (codObj (f a))

instance Endofunctor hom obj f => Functor hom obj hom obj f where
    mapObj _ _ = endomapObj

class Functor domHom domObj codHom codObj f => Covariant domHom domObj codHom codObj f where
    comap :: (domObj a, domObj b) => proxy domObj -> proxy' codObj -> domHom a b -> codHom (f a) (f b)

instance (Functor domHom domObj codHom codObj f, Neutral.Covariant domHom codHom f) => Covariant domHom domObj codHom codObj f where
    comap _ _ = Neutral.comap

class (Monoidal domHom domObj, Monoidal codHom codObj, Covariant domHom domObj codHom codObj f) => Applicative domHom domObj codHom codObj (f :: i -> j) where
    unit :: proxy domHom -> proxy' domObj -> proxy'' codHom -> proxy''' codObj -> codHom (Unit j) (f (Unit i))
    zip :: proxy domHom -> proxy' domObj -> proxy'' codObj -> codHom (Product j (f a) (f b)) (f (Product i a b))
