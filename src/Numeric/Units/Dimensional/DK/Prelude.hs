{- |
   Copyright  : Copyright (C) 2006-2015 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn@buckwalter.se
   Stability  : Stable
   Portability: GHC only

= Summary

This module supplies a convenient set of imports for working with the dimensional-dk package, including aliases for common 'Quantity's and 'Dimension's,
and a comprehensive set of SI units and units accepted for use with the SI.

It re-exports the "Prelude", hiding arithmetic functions whose names collide with the dimensionally-typed versions supplied by this package.

-}
module Numeric.Units.Dimensional.DK.Prelude
    ( module Numeric.Units.Dimensional.DK
    , module Numeric.Units.Dimensional.DK.Quantities
    , module Numeric.Units.Dimensional.DK.SIUnits
    , module Numeric.NumType.DK.Integers
    , module Data.Foldable
    , module Prelude
    ) where

import Numeric.Units.Dimensional.DK hiding
    ( dmap
    , removeAngles, coerceAngles
    )

import Numeric.Units.Dimensional.DK.Quantities

import Numeric.Units.Dimensional.DK.SIUnits

import Numeric.NumType.DK.Integers
    ( neg5, neg4, neg3, neg2, neg1, zero, pos1, pos2, pos3, pos4, pos5
    )  -- Used in exponents.

import Data.Foldable
    ( product, minimum, maximum )

import Prelude hiding
    ( (+), (-), (*), (/), (^), (**)
    , abs, negate, pi, exp, log, sqrt
    , sin, cos, tan, asin, acos, atan, atan2
    , sinh, cosh, tanh, asinh, acosh, atanh
    , sum, product, minimum, maximum
    )  -- Hide definitions overridden by 'Numeric.Dimensional'.
