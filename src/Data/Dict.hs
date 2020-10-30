module Data.Dict where

-- | Constraint instances can be stored as values with GADTs.
data Dict c where
    -- | You can only use this type constructor if an instance of @c@ is in scope;
    -- pattern matching against it brings an instance of @c@ into scope.
    Dict :: c => Dict c
