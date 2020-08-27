module Category.Constraint where

import Category
import Prelude (($))

newtype a :- b = Sub (a => Dict b)

(\\) :: a => (b => r) -> (a :- b) -> r
r \\ Sub Dict = r

instance Category (:-) obj where
    identity _ = Sub Dict
    compose _ f g = Sub $ Dict \\ f \\ g
