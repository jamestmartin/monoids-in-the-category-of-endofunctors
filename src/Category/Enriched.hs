module Category.Enriched
    ( Enriched, eid, ecomp
    ) where

import Category.Base
import Category.Monoidal
import Data.Kind (Constraint, FUN, Type)
import GHC.Types (Multiplicity (One))

type Enriched :: (j -> j -> Type) -> (j -> j -> j) -> (i -> i -> j) -> Constraint
class TensorProduct over prod => Enriched over prod morph where
    eid :: proxy prod -> over (Unit over prod) (Obj morph a)
    ecomp :: over (prod (morph b c) (morph a b)) (morph a c)

instance NiceCat morph => Enriched (->) (,) morph where
    eid _ = \() -> id
    ecomp = \(f, g) -> f . g

instance Enriched (FUN 'One) (,) (FUN 'One) where
    eid _ = \() -> id
    ecomp = \(f, g) -> \x -> f (g x)
