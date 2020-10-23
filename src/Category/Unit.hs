module Category.Unit where

import Category.Base

data Unit (a :: ()) (b :: ()) = Unit

instance Semigroupoid Unit where
    _ . _ = Unit

instance Category Unit where
    id = Unit

instance Groupoid Unit where
    inv _ = Unit
