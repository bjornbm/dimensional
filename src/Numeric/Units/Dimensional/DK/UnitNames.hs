{-# LANGUAGE PatternSynonyms #-}
module Numeric.Units.Dimensional.DK.UnitNames
(
  UnitName, NameAtom, PrefixName,
  atom, applyPrefix, (*), (/), (^), product, reduce, grouped,
  nOne, nMeter, nGram, nKilogram, nSecond, nAmpere, nKelvin, nMole, nCandela,
  deka, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta,
  deci, centi, milli, micro, nano, pico, femto, atto, zepto, yocto,
  baseUnitNames,
  UnitNameTransformer, UnitNameTransformer2,
  weaken, strengthen, strengthenIfNeeded
)
where

import Numeric.Units.Dimensional.DK.UnitNames.Internal
import Prelude hiding ((*), (/), (^), product)