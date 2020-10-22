module Category.Functor.Foldable where

import Category.Base
import Category.Functor

type family Base (t :: i) :: i -> i

class Endofunctor (Base t) => Recursive t where
    project :: Dom (Base t) t (Base t t)

class Endofunctor (Base t) => Corecursive t where
    embed :: Dom (Base t) (Base t t) t

type Algebra f a = Dom f (f a) a

cata :: Recursive t => Algebra (Base t) a -> Dom (Base t) t a
cata alg = alg . map (cata alg) . project

type Coalgebra f a = Dom f a (f a)

ana :: Corecursive t => Coalgebra (Base t) a -> Dom (Base t) a t
ana coalg = embed . map (ana coalg) . coalg
