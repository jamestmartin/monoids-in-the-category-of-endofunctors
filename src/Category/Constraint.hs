{-# LANGUAGE UndecidableSuperClasses #-}
module Category.Constraint
    ( (:-) (Sub), (\\)
    , ProdC
    ) where

import Category.Base
import Category.Functor
import Category.Monoidal
import Data.Dict
import Data.Kind (Constraint, Type)

type (:-) :: Constraint -> Constraint -> Type
data (:-) c d = Sub (c => Dict d)

(\\) :: a => (b => c) -> (a :- b) -> c
r \\ Sub Dict = r

instance Category (:-) where
    f . g = Sub (Dict \\ f \\ g)

instance NiceCat (:-) where
    id = Sub Dict

instance Functor (Nat (->) (:-)) (Yoneda (:-)) (:-) where
    map (Op (Sub f)) = Nat \_ (Sub g) -> Sub case f of Dict -> case g of Dict -> Dict

instance Functor (->) (:-) ((:-) a) where
    map = (.)

instance Functor (->) (:-) Dict where
    map f = \Dict -> case f of Sub Dict -> Dict

class (c, d) => ProdC c d
instance (c, d) => ProdC c d
-- Note that, to my understanding,
-- it is impossible to define disjunction in the category of constraints,
-- and as far as I know this is the only way in which entailment is monoidal
-- (up to isomorphism), although I haven't seriously thought about it at all.

instance Functor (Nat (:-) (:-)) (:-) ProdC where
    map (Sub f) = Nat_ (Sub case f of Dict -> Dict)

instance Functor (:-) (:-) (ProdC a) where
    map (Sub f) = Sub case f of Dict -> Dict

instance TensorProduct (:-) ProdC where
    type Unit (:-) ProdC = ()
    prodIL _ = Sub Dict
    prodIR _ = Sub Dict
    prodEL _ = Sub Dict
    prodER _ = Sub Dict
    prodAL _ _ _ = Sub Dict
    prodAR _ _ _ = Sub Dict
