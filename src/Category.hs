-- | A category theory prelude which re-exports other modules.
-- Please refer to their respective haddocks.
--
-- This does not export *every* category module;
-- only those which are likely to be relevant to every program.
-- For example, "Category.Functor.Foldable" is not included here.
module Category
    ( module Category.Base
    , module Category.Constraint
    , module Category.Functor
    ) where

import Category.Base
import Category.Constraint
import Category.Functor
