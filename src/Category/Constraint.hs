-- | The category of constraints.
module Category.Constraint where
  
import Category
import Prelude (($))
  
newtype a :- b = Sub (a => Dict b)

type instance (~>) = (:-)

data Dict p where
    Dict :: p => Dict p

(\\) :: a => (b => r) -> (a :- b) -> r
r \\ Sub Dict = r

instance Dom r ~ (:-) => Category r where
    identity = Sub Dict
    compose f g = Sub $ Dict \\ f \\ g