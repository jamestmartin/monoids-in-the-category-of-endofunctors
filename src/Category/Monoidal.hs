module Category.Monoidal where

import Category.Base
import Category.Constraint
import Category.Functor
import Data.Either (Either (Left, Right))
import Data.Void (Void, absurd)

class Bifunctor f => TensorProduct (f :: i -> i -> i) where
    type Unit f :: i
    unitObj :: proxy f -> Dict (Obj (Dom f) (Unit f))

    unitLI  :: Dom f a (f a (Unit f))
    unitRI  :: Dom f a (f (Unit f) a)
    unitLE  :: Dom f (f a (Unit f)) a
    unitRE  :: Dom f (f (Unit f) a) a

    assocL  :: Dom f (f a (f b c)) (f (f a b) c)
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
