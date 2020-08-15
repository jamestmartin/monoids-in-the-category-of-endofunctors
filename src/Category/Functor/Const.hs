module Category.Functor.Const where

import Category
import Category.Functor
import Category.Nat
import Prelude (($))

newtype Const f a = Const { getConst :: f }

reconst :: Const f a -> Const f b
reconst = Const . getConst

instance (Category src, Category dest) => Functor src dest Const where
    map _ _ f = Nat $ Const . f . getConst

instance (Category src, Category dest) => Functor src dest (Const f) where
    map _ _ _ = Const . getConst

instance (Category src, Category dest) => Contravariant src dest (Const f) where
    contramap _ _ _ = Const . getConst

newtype Const2 f a b = Const2 { getConst2 :: f }

instance (Category src, Category dest) => Functor src dest Const2 where
    map _ _ f = nat2 $ Const2 . f . getConst2

instance (Category src, Category dest) => Functor src dest (Const2 f) where
    map _ _ f = Nat $ Const2 . getConst2

instance (Category src, Category dest) => Functor src dest (Const2 f a) where
    map _ _ f = Const2 . getConst2

instance (Category src, Category dest) => Contravariant src dest (Const2 f) where
    contramap _ _ _ = Nat $ Const2 . getConst2

instance (Category src, Category dest) => Contravariant src dest (Const2 f a) where
    contramap _ _ _ = Const2 . getConst2
