-- | Monoidal categories.
module Category.Monoidal where

import Category.Base
import Category.Constraint
import Category.Functor
import Data.Either (Either (Left, Right))
import Data.Void (Void, absurd)

-- | A category is monoidal if it has a product and a unit for that product.
-- A category can have multiple tensor products and be monoidal in multiple ways,
-- including the category of types itself,
-- so instead of using a @Monoidal@ typeclass, we use a @TensorProduct@ typeclass.
class Bifunctor f => TensorProduct (f :: i -> i -> i) where
    -- | The identity object of the product.
    type Unit f :: i
    -- | The identity object is actually an object in the category.
    unitObj :: proxy f -> Dict (Obj (Dom f) (Unit f))

    -- | 'Unit' is the identity of the product; introduce a value on the left of a product.
    unitLI  :: Dom f a (f a (Unit f))
    -- | 'Unit' is the identity of the product; introduce a value on the right of a product.
    unitRI  :: Dom f a (f (Unit f) a)
    -- | 'Unit' is the identity of the product; eliminate a product, projecting a value on the right.
    unitLE  :: Dom f (f a (Unit f)) a
    -- | 'Unit' is the identity of the product; eliminate a product, projecting a value on the left.
    unitRE  :: Dom f (f (Unit f) a) a

    -- | The product is associative; associate to the left.
    assocL  :: Dom f (f a (f b c)) (f (f a b) c)
    -- | The product is associative; associate to the right.
    assocR  :: Dom f (f (f a b) c) (f a (f b c))

instance TensorProduct (,) where
    type Unit (,) = ()
    unitObj _ = Dict

    unitLI x = (x, ())
    unitRI x = ((), x)
    unitLE (x, _) = x
    unitRE (_, x) = x

    assocL (x, (y, z)) = ((x, y), z)
    assocR ((x, y), z) = (x, (y, z))

instance TensorProduct Either where
    type Unit Either = Void
    unitObj _ = Dict

    unitLI = Left
    unitRI = Right
    unitLE (Left x) = x
    unitLE (Right x) = absurd x
    unitRE (Left x) = absurd x
    unitRE (Right x) = x

    assocL (Left x) = Left (Left x)
    assocL (Right (Left x)) = Left (Right x)
    assocL (Right (Right x)) = Right x
    assocR (Left (Left x)) = Left x
    assocR (Left (Right x)) = Right (Left x)
    assocR (Right x) = Right (Right x)
