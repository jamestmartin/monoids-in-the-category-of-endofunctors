-- | The 'Unit' category contains exactly one object and one morphism.
module Category.Unit where

import Category.Base

-- | A category with exactly one morphism.
data Unit (a :: ()) (b :: ()) = Unit

instance Semigroupoid Unit where
    _ . _ = Unit

instance Category Unit where
    id = Unit

instance Groupoid Unit where
    inv _ = Unit
