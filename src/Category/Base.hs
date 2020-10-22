{-# LANGUAGE ExplicitNamespaces #-}
module Category.Base
    ( module Control.Category
    , type (~>)
    , Post
    ) where

import Control.Category (Category, id, (.))
import Data.Kind (Type)

-- | An unenriched hom.
type family (~>) :: i -> i -> Type
type instance (~>) = (->)

type Post f = forall a. f a
