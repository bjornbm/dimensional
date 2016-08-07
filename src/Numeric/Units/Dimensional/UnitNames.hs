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
  UnitName, NameAtom, Prefix, PrefixName, Metricality(..),
  -- * Construction of Unit Names
  atom, applyPrefix, (*), (/), (^), product, reduce, grouped,
  -- * Standard Names
  baseUnitName, nOne,
  -- * Inspecting Prefixes
  prefixName, scaleExponent, scaleFactor,
  -- * Sets of Prefixes
  PrefixSet, prefixSet, unPrefixSet, filterPrefixSet, selectPrefix, siPrefixes, majorSiPrefixes,
  -- * Convenience Type Synonyms for Unit Name Transformations
  UnitNameTransformer, UnitNameTransformer2,
  -- * Forgetting Unwanted Phantom Types
  weaken, strengthen, relax,
  name_en, abbreviation_en, asAtomic
)
where

import Numeric.Units.Dimensional.UnitNames.Internal
import Numeric.Units.Dimensional.Variants
import Prelude hiding ((*), (/), (^), product)

scaleFactor :: Prefix -> Rational
scaleFactor p = 10 ^^ (scaleExponent p)
