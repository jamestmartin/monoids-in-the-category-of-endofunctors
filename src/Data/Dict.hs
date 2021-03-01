module Data.Dict where

import Data.Kind (Constraint, Type)

-- | Constraint instances can be stored as values with GADTs.
type Dict :: Constraint -> Type
data Dict c =
    -- | You can only use this type constructor if an instance of @c@ is in scope;
    -- pattern matching against it brings an instance of @c@ into scope.
    c => Dict
