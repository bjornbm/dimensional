{- |
    Copyright  : Copyright (C) 2006-2018 Bjorn Buckwalter
    License    : BSD3

    Maintainer : bjorn@buckwalter.se
    Stability  : Stable

Defines convenience functions for inspecting and manipulating quantities with 'RealFloat'
floating-point representations.

The dimensionally-typed versions of functions from Patrick Perry's @ieee754@ package
copy that package's API as closely as possible, by permission. In turn they are based on
the @tango@ math library for the D language.

-}

{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.Units.Dimensional.Float
(
  -- * Lifted Predicates from 'RealFloat'
  isDenormalized, isInfinite, isNaN, isNegativeZero
  -- * Convenience Functions
, isFiniteNumber, scaleFloat
  -- * Lifted Functions from "Numeric.IEEE"
  -- ** Values
, infinity, minNormal, maxFinite, epsilon, nan
  -- ** Arithmetic
, predIEEE, succIEEE, bisectIEEE, copySign
  -- ** NaN with Payload
, nanWithPayload, nanPayload, F.maxNaNPayload
  -- ** Comparisons
, identicalIEEE, minNum, maxNum, minNaN, maxNaN
)
where

import Control.Applicative
import Data.Word (Word64)
import Prelude (RealFloat)
import qualified Prelude as P
import Numeric.IEEE (IEEE)
import qualified Numeric.IEEE as F
import Numeric.Units.Dimensional.Internal (liftQ, liftQ2)
import Numeric.Units.Dimensional.Prelude hiding (RealFloat(..))
import Numeric.Units.Dimensional.Coercion

-- $setup
-- >>> :set -XExtendedDefaultRules
-- >>> :set -XNegativeLiterals

-- | 'True' if the representation of the argument is too small to be represented in normalized format.
isDenormalized :: RealFloat a => Quantity d a -> Bool
isDenormalized = P.isDenormalized . unQuantity

-- | 'True' if the representation of the argument is a number and is not infinite.
--
-- >>> isFiniteNumber (_1 / _0)
-- False
--
-- >>> isFiniteNumber (_0 / _0)
-- False
--
-- >>> isFiniteNumber (_3 / _2)
-- True
isFiniteNumber :: RealFloat a => Quantity d a -> Bool
isFiniteNumber = not . liftA2 (||) isNaN isInfinite

-- | 'True' if the representation of the argument is an IEEE infinity or negative infinity.
--
-- >>> isInfinite (_1 / _0)
-- True
--
-- >>> isInfinite (42 *~ micro farad)
-- False
isInfinite :: RealFloat a => Quantity d a -> Bool
isInfinite = P.isInfinite . unQuantity

-- | 'True' if the representation of the argument is an IEEE "not-a-number" (NaN) value.
--
-- >>> isNaN _3
-- False
--
-- >>> isNaN (_1 / _0)
-- False
--
-- >>> isNaN (asin _4)
-- True
isNaN :: RealFloat a => Quantity d a -> Bool
isNaN = P.isNaN . unQuantity

-- | 'True' if the representation of the argument is an IEEE negative zero.
--
-- >>> isNegativeZero _0
-- False
--
-- >>> isNegativeZero $ (-1e-200 *~ one) * (1e-200 *~ one)
-- True
isNegativeZero :: RealFloat a => Quantity d a -> Bool
isNegativeZero = P.isNegativeZero . unQuantity

-- | Multiplies a floating-point quantity by an integer power of the radix of the representation type.
--
-- Use 'P.floatRadix' to determine the radix.
--
-- >>> let x = 3 *~ meter
-- >>> scaleFloat 3 x
-- 24.0 m
scaleFloat :: RealFloat a => Int -> Quantity d a -> Quantity d a
scaleFloat x = Quantity . P.scaleFloat x . unQuantity

-- | An infinite floating-point quantity.
infinity :: IEEE a => Quantity d a
infinity = Quantity $ F.infinity

-- | The smallest representable positive quantity whose representation is normalized.
minNormal :: IEEE a => Quantity d a
minNormal = Quantity $ F.minNormal

-- | The largest representable finite floating-point quantity.
maxFinite :: IEEE a => Quantity d a
maxFinite = Quantity $ F.maxFinite

-- | The smallest positive value @x@ such that @_1 + x@ is representable.
epsilon :: IEEE a => Dimensionless a
epsilon = Quantity $ F.epsilon

-- | @copySign x y@ returns the quantity @x@ with its sign changed to match that of @y@.
copySign :: IEEE a => Quantity d a -> Quantity d a -> Quantity d a
copySign = liftQ2 F.copySign

-- | Return 'True' if two floating-point quantities are /exactly/ (bitwise) equal.
identicalIEEE :: IEEE a => Quantity d a -> Quantity d a -> Bool
identicalIEEE (Quantity x) (Quantity y) = F.identicalIEEE x y

-- | Return the next largest representable floating-point quantity (@Infinity@ and @NaN@ are unchanged).
succIEEE :: IEEE a => Quantity d a -> Quantity d a
succIEEE = liftQ F.succIEEE

-- | Return the next smallest representable floating-point quantity (@Infinity@ and @NaN@ are unchanged).
predIEEE :: IEEE a => Quantity d a -> Quantity d a
predIEEE = liftQ F.predIEEE

-- | Given two floating-point quantities with the same sign, return the quantity whose representation is halfway
-- between their representations on the IEEE number line. If the signs of the values differ or either is @NaN@,
-- the value is undefined.
bisectIEEE :: IEEE a => Quantity d a -> Quantity d a -> Quantity d a
bisectIEEE (Quantity x) (Quantity y) = Quantity $ F.bisectIEEE x y

-- | Default @NaN@ quantity.
nan :: IEEE a => Quantity d a
nan = Quantity $ F.nan

-- | Quiet @NaN@ quantity with a positive integer payload.
-- Payload must be less than 'maxNaNPayload' of the representation type.
--
-- Beware that while some platforms allow using 0 as a payload, this behavior is not portable.
nanWithPayload :: IEEE a => Word64 -> Quantity d a
nanWithPayload = Quantity . F.nanWithPayload

-- | The payload stored in a @NaN@ quantity. Undefined if the argument is not @NaN@.
nanPayload :: IEEE a => Quantity d a -> Word64
nanPayload = F.nanPayload . unQuantity

-- | Return the minimum of two quantities; if one value is @NaN@, return the other. Prefer the first if both values are @NaN@.
minNum :: RealFloat a => Quantity d a -> Quantity d a -> Quantity d a
minNum = liftQ2 F.minNum

-- | Return the maximum of two quantities; if one value is @NaN@, return the other. Prefer the first if both values are @NaN@.
maxNum :: RealFloat a => Quantity d a -> Quantity d a -> Quantity d a
maxNum = liftQ2 F.maxNum

-- | Return the minimum of two quantities; if one value is @NaN@, return it. Prefer the first if both values are @NaN@.
minNaN :: RealFloat a => Quantity d a -> Quantity d a -> Quantity d a
minNaN = liftQ2 F.minNaN

-- | Return the maximum of two quantities; if one value is @NaN@, return it. Prefer the first if both values are @NaN@.
maxNaN :: RealFloat a => Quantity d a -> Quantity d a -> Quantity d a
maxNaN = liftQ2 F.maxNaN
