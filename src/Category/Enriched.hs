module Category.Enriched where

import Category.Base
import Category.Monoidal
import Data.Dict
import Data.Kind (Constraint, FUN, Type)
import GHC.Types (Multiplicity (One))

class Monoidal over prod => Enriched (over :: j -> j -> Type) prod (morph :: i -> i -> j) where
    type EObj over prod morph :: i -> Constraint
    type EObj _over _prod _morph = Unconst1
    eobj :: proxy prod -> over (Unit over prod) (morph a b) -> Dict (EObj over prod morph a, EObj over prod morph b)
    default eobj :: forall proxy a b. EObj over prod morph ~ Unconst1 => proxy prod -> over (Unit over prod) (morph a b) -> Dict (EObj over prod morph a, EObj over prod morph b)
    eobj _ _ = Dict

    eid :: EObj over prod morph a => proxy prod -> over (Unit over prod) (morph a a)
    ecomp :: over (prod (morph b c) (morph a b)) (morph a c)

instance Category morph => Enriched (->) (,) morph where
    type EObj (->) (,) morph = Obj morph
    eobj _ f = obj (f ())
    eid _ = \() -> id
    ecomp = \(f, g) -> f . g

instance Enriched (FUN 'One) (,) (FUN 'One) where
    eid _ = \() -> id
    ecomp = \(f, g) -> \x -> f (g x)
