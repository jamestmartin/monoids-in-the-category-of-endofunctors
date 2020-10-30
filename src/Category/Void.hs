-- | The `Void` category contains no objects and no morphisms.
module Category.Void where

import Category.Base
import Data.Proxy

-- | The type of no morphisms.
data Void (a :: i) (b :: i)

-- | A typeclass which is not implemented for anything.
-- It's possible to write an implementation for this because Haskell isn't total,
-- but you really, really shouldn't.
class Bottom (a :: i) where
    bottom :: forall b proxy. proxy a -> b

instance Semigroupoid Void where
    -- No values are objects.
    type Obj Void = Bottom
    observe = \case {}
    (.) = \case {}

instance Category Void where
    id :: forall a. Bottom a => Void a a
    id = bottom (Proxy @a)
