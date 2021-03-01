module Category.Functor.Foldable where

import Category.Base
import Category.Functor

type family Base (t :: i) :: i -> i

class Endofunctor morph (Base t) => Recursive morph t where
    project :: morph t (Base t t)

class Endofunctor morph (Base t) => Corecursive morph t where
    embed :: morph (Base t t) t

type Algebra morph f a = morph (f a) a

cata :: Recursive morph t => Algebra morph (Base t) a -> morph t a
cata alg = alg . map (cata alg) . project

type Coalgebra morph f a = morph a (f a)

ana :: Corecursive morph t => Coalgebra morph (Base t) a -> morph a t
ana coalg = embed . map (ana coalg) . coalg
