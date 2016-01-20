{- |
    Copyright  : Copyright (C) 2006-2016 Bjorn Buckwalter
    License    : BSD3

    Maintainer : bjorn@buckwalter.se
    Stability  : Stable

Defines convenience functions for inspecting and manipulating quantities with 'RealFloat'
floating-point representations.
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.Units.Dimensional.Float
(
  -- * Lifted Predicates from 'RealFloat'
  isDenormalized, isInfinite, isNaN, isNegativeZero
  -- * Convenience Functions
, isFiniteNumber, scaleFloat
)
where

import Control.Applicative
import Prelude (RealFloat)
import qualified Prelude as P
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
