-- | GHC's @TypeApplications@ extension is a poor choice for explicit type arguments
-- because the order of arguments can (and does) vary between GHC versions,
-- and doesn't let you control which arguments are implicit and which are explicit.
--
-- 'Proxy' provides a better alternative for passing explicit type arguments.
module Data.Proxy where

import Data.Kind (Type)

-- | A type family of singletons used for explicit type arguments without TypeApplications.
type Proxy :: i -> Type
data Proxy a where
    -- | You can specify the type used with @Proxy :: Proxy a@,
    -- or @Proxy \@a@ using TypeApplications.
    --
    -- Why even bother with the proxy if I'm going to recommend TypeApplications anyway?
    -- Because you get explicit control of argument order, and don't /require/ TypeApplications.
    Proxy :: Proxy a
