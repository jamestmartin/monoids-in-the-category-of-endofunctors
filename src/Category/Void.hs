module Category.Void where

import Category.Base
import Data.Proxy
import Data.Void qualified as V

data Void (a :: V.Void) (b :: V.Void)

instance Semigroupoid Void where
    type Obj Void = Bottom
    observe = \case {}
    (.) = \case {}

instance Category Void where
    id :: forall a. Bottom a => Void a a
    id = bottom (Proxy @a)
