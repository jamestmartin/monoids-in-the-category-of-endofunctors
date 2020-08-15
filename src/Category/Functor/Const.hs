module Category.Functor.Const where

import Category
import Category.Functor
import Category.Nat
import Prelude (($))

newtype Const f a = Const { getConst :: f }

reconst :: Const f a -> Const f b
reconst = Const . getConst

instance (Category src, Category dest) => Functor src dest Const where
    map f = Nat $ Const . f . getConst

instance {-# OVERLAPPABLE #-} (Category src, Category dest) => Functor src dest (Const f) where
    map _ = Const . getConst

instance {-# OVERLAPPABLE #-} (Category src, Category dest) => Contravariant src dest (Const f) where
    contramap _ = Const . getConst

newtype Const2 f a b = Const2 { getConst2 :: f }

instance (Category src, Category dest) => Functor src dest Const2 where
    map f = nat2 $ Const2 . f . getConst2

instance {-# OVERLAPPABLE #-} (Category src, Category dest) => Functor src dest (Const2 f) where
    map f = Nat $ Const2 . getConst2

instance {-# OVERLAPPABLE #-} (Category src, Category dest) => Functor src dest (Const2 f a) where
    map f = Const2 . getConst2

instance {-# OVERLAPPABLE #-} (Category src, Category dest) => Contravariant src dest (Const2 f) where
    contramap _ = Nat $ Const2 . getConst2

instance {-# OVERLAPPABLE #-} (Category src, Category dest) => Contravariant src dest (Const2 f a) where
    contramap _ = Const2 . getConst2
