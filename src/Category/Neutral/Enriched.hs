module Category.Neutral.Enriched where

import Category.Neutral qualified as Neutral

class Neutral.Monoidal under => Category under cat where
    identity :: Neutral.Unit under `under` cat a a
    compose :: Neutral.Product under (cat b c) (cat a b) `under` cat a c

instance Neutral.Category cat => Category (->) cat where
    identity () = Neutral.identity
    compose (f, g) = Neutral.compose f g
