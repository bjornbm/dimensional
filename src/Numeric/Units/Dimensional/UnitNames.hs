{-# LANGUAGE PatternSynonyms #-}

{- |
   Copyright  : Copyright (C) 2006-2015 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn@buckwalter.se
   Stability  : Stable
   Portability: GHC only

This module provides types and functions for manipulating unit names.

Please note that the details of the name representation may be less stable than the other APIs
provided by this package, as new features using them are still being developed.

-}
module Numeric.Units.Dimensional.UnitNames
(
  -- * Data Types
  UnitName, NameAtom, PrefixName, Metricality(..),
  -- * Construction of Unit Names
  atom, applyPrefix, (*), (/), (^), product, reduce, grouped,
  -- * Standard Names
  baseUnitName,
  -- ** Names for the Base Units
  nOne, nMeter, nGram, nKilogram, nSecond, nAmpere, nKelvin, nMole, nCandela,
  -- ** Names for the SI Metric Prefixes
  deka, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta,
  deci, centi, milli, micro, nano, pico, femto, atto, zepto, yocto,
  -- * Convenience Type Synonyms for Unit Name Transformations
  UnitNameTransformer, UnitNameTransformer2,
  -- * Forgetting Unwanted Phantom Types
  weaken, strengthen, relax
)
where

import Numeric.Units.Dimensional.UnitNames.Internal
import Numeric.Units.Dimensional.Variants
import Prelude hiding ((*), (/), (^), product)
