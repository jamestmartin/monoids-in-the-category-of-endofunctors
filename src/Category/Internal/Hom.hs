module Category.Internal.Hom where

type family Hom :: i -> i -> k
type instance Hom = (~>)

type family (~>) :: i -> i -> *

type Arr  (f :: i)                = ((~>) :: i -> i -> *)
type Cod  (f :: i -> j)           = ((~>) :: j -> j -> *)
type Cod2 (f :: i -> j -> k)      = ((~>) :: k -> k -> *)
type Dom  (f :: i -> j)           = ((~>) :: i -> i -> *)
type Dom2 (f :: i -> j -> k)      = ((~>) :: j -> j -> *)
type Dom3 (f :: i -> j -> k -> l) = ((~>) :: k -> k -> *)