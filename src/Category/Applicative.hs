module Category.Applicative where

import Category
import Category.Functor
import Category.Semigroup ()
import qualified Prelude

class (MonoidalCategory domHom domObj, MonoidalCategory codHom codObj, Functor domHom domObj codHom codObj f) => Applicative domHom domObj codHom codObj f where
    unit :: proxy domHom -> proxy' domObj -> proxy'' codHom -> proxy''' codObj -> f (Unit domObj)
    zip :: proxy domHom -> proxy' domObj -> proxy'' codObj -> proxy''' codObj -> Product codObj (f a) (f b) `codHom` f (Product domObj a b)

instance {-# INCOHERENT #-} Prelude.Applicative f => Applicative (->) EveryC (->) EveryC f where
    unit _ _ _ _ = Prelude.pure ()
    zip _ _ _ _ (x, y) = Prelude.fmap (,) x Prelude.<*> y
