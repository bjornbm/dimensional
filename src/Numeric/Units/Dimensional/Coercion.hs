{- |
    Copyright  : Copyright (C) 2006-2014 Bjorn Buckwalter
    License    : BSD3

    Maintainer : bjorn@buckwalter.se
    Stability  : Experimental
    Portability: GHC only?

Re-exports the raw 'Quantity' constructor from the Numeric.Units.Dimensional.Internal module, along with 'Data.Coerce.coerce',
for convenience in converting between raw representations and dimensional values.

Note that use of these constructs requires the user to verify the dimensional safety of the conversion,
because the coercion doesn't explicitly mention the unit of the representation.

Note that the haddock documentation doesn't mention the 'Quantity' constructor because it is a part of the
'Dimensional' associated data family, but it is exported by this module.

-}

module Numeric.Units.Dimensional.Coercion
(
  coerce, Dimensional(Quantity), unQuantity
)
where

import Data.Coerce (coerce)
import Numeric.Units.Dimensional.Internal (Quantity, Dimensional(Quantity))

-- | Unwraps a `Quantity`, yielding its underlying representation.
--
-- This is a type-restricted version of `coerce`.
unQuantity :: Quantity d a -> a
unQuantity = coerce
