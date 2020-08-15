module Category where

import Data.Kind (Constraint)

type family Hom :: i -> i -> k
type instance Hom = (~>)

type family (~>) :: i -> i -> *

type Dom (a :: i -> j) = (~>) :: i -> i -> *

data Proxy :: k -> * where
    Proxy :: Proxy k

class Category (r :: k -> Constraint) where
    identity :: forall proxy a. r a => proxy r -> Dom r a a
    compose :: forall proxy a b c. (r a, r b, r c) => proxy r -> Dom r b c -> Dom r a b -> Dom r a c

-- | A restricted alias of `identity` which does not cause type ambiguity.
id :: forall r a. (r ~ AnyC, Category r) => Dom r a a
id = identity (Proxy @r)

-- | A restricted alias of `compose` which does not cause type ambiguity.
(.) :: forall r a b c. (r ~ AnyC, Category r) => Dom r b c -> Dom r a b -> Dom r a c
(.) = compose (Proxy @r)

type instance (~>) = (->)

-- | The category of data types.
instance Dom r ~ (->) => Category r where
    identity _ x = x
    compose _ f g x = f (g x)

-- Everything below is miscellaneous crap that doesn't have a better home.

class NoC
instance NoC

class AnyC a
instance AnyC a

class (f (g a)) => ComposeC f g a
instance (f (g a)) => ComposeC f g a

newtype Compose (f :: j -> *) (g :: i -> j) (a :: i) = Compose { getCompose :: f (g a) }

class (forall a. c a) => LimC c
instance (forall a. c a) => LimC c

class (LimC (ComposeC f g)) => Post f g
instance (LimC (ComposeC f g)) => Post f g

newtype Lim f = Lim { getLim :: forall a. f a }
