{-# LANGUAGE UndecidableInstances #-}
module Category.Functor where

import Category
import qualified Prelude

class (Category domHom domObj, Category codHom codObj) => Functor domHom domObj codHom codObj f where
    mapObj :: domObj a => proxy domHom -> proxy' domObj -> proxy'' codHom -> Dict (codObj (f a))
    map :: (domObj a, domObj b) => proxy domObj -> proxy' codObj -> a `domHom` b -> f a `codHom` f b

instance (Category (->) obj, forall a. obj (Identity a)) => Functor (->) obj (->) obj Identity where
    mapObj _ _ _ = Dict
    map _ _ f (Identity x) = Identity (f x)

class (Functor hom obj hom obj f) => Endofunctor hom obj f where
    endomapObj :: obj a => proxy hom -> proxy' obj -> Dict (obj (f a))
    endomap :: (obj a, obj b) => proxy obj -> a `hom` b -> f a `hom` f b

instance (Functor hom obj hom obj f) => Endofunctor hom obj f where
    endomapObj pxh pxo = mapObj pxh pxo pxh
    endomap px = map px px
