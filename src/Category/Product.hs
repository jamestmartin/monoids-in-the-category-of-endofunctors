{-# LANGUAGE UndecidableInstances, UndecidableSuperClasses #-}
module Category.Product
    ( Pi1, Pi2, pairEta
    , Product (Product)
    , Uncurry, Uncurry' (Uncurry), getUncurry, UncurryN (UncurryN), getUncurryN
    , Unc, uncurry, ununcurry
    , Bifunctor, bimap_, bimap, first, second
    , Profunctor, dimap
    ) where

import Category.Base
import Category.Functor
import Data.Dict
import Data.Kind (Constraint, FUN, Type)
import Data.Proxy
import Unsafe.Coerce (unsafeCoerce)

-- | The first projection of the type-level tuple.
type Pi1 :: (i, j) -> i
type family Pi1 xy where
    Pi1 '(x, _) = x

-- | The second projection of the type-level tuple.
type Pi2 :: (i, j) -> j
type family Pi2 xy where
    Pi2 '(_, y) = y

-- | Eta expansion for the pair type *on the type level*. **This does not hold on the value level.**
--
-- This is not provable by GHC's constraint solver, but it is safe to assume.
-- Maybe. Hopefully.
pairEta :: proxy x -> Dict (x ~ '(Pi1 x, Pi2 x))
pairEta = unsafeCoerce (Dict :: Dict ())

-- | The product category of two categories `c` and `d` is the category
-- whose objects are pairs of objects from `c` and `d` and whose arrows
-- are pairs of arrows from `c` and `d`.
type Product :: (i -> i -> Type) -> (j -> j -> Type) -> (i, j) -> (i, j) -> Type
data Product c d a b = Product !(c (Pi1 a) (Pi1 b)) !(d (Pi2 a) (Pi2 b))

instance (Category c, Category d) => Category (Product c d) where
    idL (Product f g) = Product (idL f) (idL g)
    idR (Product f g) = Product (idR f) (idR g)
    Product f1 g1 . Product f2 g2 = Product (f1 . f2) (g1 . g2)

instance (NiceCat c, NiceCat d) => NiceCat (Product c d) where
    id = Product id id

type Uncurry :: (a -> b -> c) -> (a, b) -> c
type family Uncurry

type Uncurry' :: (a -> b -> Type) -> (a, b) -> Type
newtype Uncurry' f ab = Uncurry { getUncurry :: f (Pi1 ab) (Pi2 ab) }
type instance Uncurry = Uncurry'

type UncurryN :: (a -> b -> c -> Type) -> (a, b) -> c -> Type
newtype UncurryN f ab x = UncurryN { getUncurryN :: f (Pi1 ab) (Pi2 ab) x }
type instance Uncurry = UncurryN

instance (Category c, Functor (->) c (f a b)) => Functor (->) c (UncurryN f '(a, b)) where
    map f (UncurryN x) = UncurryN (map f x)

type Unc :: (c -> c -> Type) -> Constraint
class Category cat => Unc cat where
    uncurry :: Obj cat (f a b) -> cat (f a b) (Uncurry f '(a, b))
    ununcurry :: Obj cat (f a b) -> cat (Uncurry f '(a, b)) (f a b)

instance Unc (FUN m) where
    uncurry _ = Uncurry
    ununcurry _ (Uncurry x) = x

instance Unc (Nat (->) (->)) where
    uncurry (Nat _) = Nat_ UncurryN
    ununcurry (Nat _) = Nat_ \(UncurryN x) -> x

-- | A bifunctor is a functor whose domain is the product category.
type Bifunctor :: (k -> k -> Type) -> (i -> i -> Type) -> (j -> j -> Type) -> (i -> j -> k) -> Constraint
class (Unc cod, Category dom1, Category dom2, Functor cod (Product dom1 dom2) (Uncurry f)) => Bifunctor cod dom1 dom2 f where
    bimap_ :: forall a b. Obj dom1 a -> Obj dom2 b -> Obj cod (f a b)

bimap :: forall cod dom1 dom2 f a b c d. Bifunctor cod dom1 dom2 f => dom1 a c -> dom2 b d -> cod (f a b) (f c d)
bimap f g = ununcurry (bimap_ (idR f) (idR g)) . map (Product f g) . uncurry (bimap_ (idL f) (idL g))

first :: forall cod dom f a b c. (NiceCat dom, Bifunctor cod dom dom f) => dom a b -> cod (f a c) (f b c)
first f = bimap f (id :: dom c c)

second :: forall cod dom f a b c. (NiceCat dom, Bifunctor cod dom dom f) => dom b c -> cod (f a b) (f a c)
second g = bimap (id :: dom a a) g

instance (Unc cod, Functor (Nat cod dom2) dom1 f, forall x. Functor cod dom2 (f x), uncurry ~ Uncurry) => Functor cod (Product dom1 dom2) (uncurry f) where
    {-# INLINABLE map #-}
    map :: forall a b. Product dom1 dom2 a b -> cod (uncurry f a) (uncurry f b)
    map (Product f g) = lemma (uncurry (map (idR g)) . runNat (map f) (idR g) . map g . ununcurry (map (idL g)))
        where lemma :: ((a ~ '(Pi1 a, Pi2 a), b ~ '(Pi1 b, Pi2 b)) => c) -> c
              lemma x = case pairEta (Proxy @a) of Dict -> case pairEta (Proxy @b) of Dict -> x
instance (Unc cod, Category dom1, Functor (Nat cod dom2) dom1 f, forall x. Functor cod dom2 (f x)) => Bifunctor cod dom1 dom2 f where
    bimap_ a b = runNat (map a) b . map b

type Profunctor :: (k -> k -> Type) -> (i -> i -> Type) -> (j -> j -> Type) -> (i -> j -> k) -> Constraint
class Bifunctor cod (Yoneda dom1) dom2 f => Profunctor cod dom1 dom2 f
instance Bifunctor cod (Yoneda dom1) dom2 f => Profunctor cod dom1 dom2 f

dimap :: Profunctor cod dom1 dom2 f => dom1 c a -> dom2 b d -> cod (f a b) (f c d)
dimap f g = bimap (Op f) g
