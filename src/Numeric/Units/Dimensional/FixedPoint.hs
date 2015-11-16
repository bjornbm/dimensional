{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Numeric.Units.Dimensional.FixedPoint
(
  -- * Dimensional Arithmetic
  (*~), (/~),
  (+), (-), negate, abs,
  -- ** Transcendental Functions
  -- *** Via 'Double'
  expD, logD, sinD, cosD, tanD, asinD, acosD, atanD, sinhD, coshD, tanhD, asinhD, acoshD, atanhD,
  -- *** Via arbitary 'Floating' type
  expVia, logVia, sinVia, cosVia, tanVia, asinVia, acosVia, atanVia, sinhVia, coshVia, tanhVia, asinhVia, acoshVia, atanhVia,
  -- * Commonly Used Type Synonyms
  type Q, type Angle8, type Angle16, type Angle32
)
where

import Numeric.Units.Dimensional.Prelude hiding ((+), (-), abs, negate, (*~), (/~))
import qualified Prelude as P
import Data.ExactPi
import qualified Data.ExactPi.TypeLevel as E
import Data.Int
import Data.Proxy
import qualified GHC.TypeLits as N
import Numeric.Units.Dimensional.Internal

-- | A dimensionless number with `n` fractional bits, using a representation of type `a`.
type Q n a = SQuantity (E.One E./ (E.ExactNatural (2 N.^ n))) DOne a

-- | A single-turn angle represented as a signed 8-bit integer.
type Angle8  = SQuantity (E.Pi E./ (E.ExactNatural (2 N.^ 8)))  DPlaneAngle Int8

-- | A single-turn angle represented as a signed 16-bit integer.
type Angle16 = SQuantity (E.Pi E./ (E.ExactNatural (2 N.^ 15))) DPlaneAngle Int16

-- | A single-turn angle represented as a signed 32-bit integer.
type Angle32 = SQuantity (E.Pi E./ (E.ExactNatural (2 N.^ 31))) DPlaneAngle Int32

-- | Adds two possibly scaled 'SQuantity's, preserving any scale factor.
--
-- Use in conjunction with 'changeRepRound' to combine quantities with differing scale factors.
(+) :: (Num a) => SQuantity s d a -> SQuantity s d a -> SQuantity s d a
(+) = liftUntyped2Q (P.+)

-- | Subtracts one possibly scaled 'SQuantity' from another, preserving any scale factor.
--
-- Use in conjunction with 'changeRepRound' to combine quantities with differing scale factors.
(-) :: (Num a) => SQuantity s d a -> SQuantity s d a -> SQuantity s d a
(-) = liftUntyped2Q (P.-)

-- | Takes the absolute value of a possibly scaled 'SQuantity', preserving any scale factor.
abs :: (Num a) => SQuantity s d a -> SQuantity s d a
abs = liftUntypedQ (P.abs)

-- | Negates the value of a possibly scaled 'SQuantity', preserving any scale factor.
negate :: (Num a) => SQuantity s d a -> SQuantity s d a
negate = liftUntypedQ (P.negate)

expD, logD, sinD, cosD, tanD, asinD, acosD, atanD, sinhD, coshD, tanhD, asinhD, acoshD, atanhD
  :: forall s1 s2 a b.(Real a, Integral a, Integral b, E.MinCtxt s1 Double, E.MinCtxt s2 Double) => SQuantity s1 DOne a -> SQuantity s2 DOne b
expD = expVia (Proxy :: Proxy P.Double)
logD = logVia (Proxy :: Proxy P.Double)
sinD = sinVia (Proxy :: Proxy P.Double)
cosD = cosVia (Proxy :: Proxy P.Double)
tanD = tanVia (Proxy :: Proxy P.Double)
asinD = asinVia (Proxy :: Proxy P.Double)
acosD = acosVia (Proxy :: Proxy P.Double)
atanD = atanVia (Proxy :: Proxy P.Double)
sinhD = sinhVia (Proxy :: Proxy P.Double)
coshD = coshVia (Proxy :: Proxy P.Double)
tanhD = tanhVia (Proxy :: Proxy P.Double)
asinhD = asinhVia (Proxy :: Proxy P.Double)
acoshD = acoshVia (Proxy :: Proxy P.Double)
atanhD = atanhVia (Proxy :: Proxy P.Double)

expVia, logVia, sinVia, cosVia, tanVia, asinVia, acosVia, atanVia, sinhVia, coshVia, tanhVia, asinhVia, acoshVia, atanhVia
  :: forall s1 s2 a b c.(Real a, Integral a, RealFrac b, Floating b, Integral c, E.MinCtxt s1 b, E.MinCtxt s2 b) => Proxy b -> SQuantity s1 DOne a -> SQuantity s2 DOne c
expVia = liftDimensionlessVia P.exp
logVia = liftDimensionlessVia P.log
sinVia = liftDimensionlessPeriodicVia (2 P.* P.pi) P.sin
cosVia = liftDimensionlessPeriodicVia (2 P.* P.pi) P.cos
tanVia = liftDimensionlessPeriodicVia P.pi P.tan
asinVia = liftDimensionlessVia P.asin
acosVia = liftDimensionlessVia P.acos
atanVia = liftDimensionlessVia P.atan
sinhVia = liftDimensionlessPeriodicVia (2 P.* P.pi) P.sinh
coshVia = liftDimensionlessPeriodicVia (2 P.* P.pi) P.cosh
tanhVia = liftDimensionlessPeriodicVia P.pi P.tanh
asinhVia = liftDimensionlessVia P.asinh
acoshVia = liftDimensionlessVia P.acosh
atanhVia = liftDimensionlessVia P.atanh

-- | Lift a function on dimensionless values of a specified intermediate type to operate on possibly scaled dimensionless.
liftDimensionlessVia :: forall s1 s2 a b c.(Real a, RealFrac b, Integral c, E.MinCtxt s1 b, E.MinCtxt s2 b) => (b -> b) -> Proxy b -> SQuantity s1 DOne a -> SQuantity s2 DOne c
liftDimensionlessVia f _ = (*~ siUnit) . (f :: b -> b) . (/~ siUnit)

-- | Lift a periodic function on dimensionless values of a specified intermediate type to operate on possibly scaled dimensionless.
--
-- If the scale factor of the input type is an exact integer divisor of the function's period, the argument
-- will be clamped via an integer `mod` operation prior to applying the function to avoid errors introduced by a floating point modulus.
liftDimensionlessPeriodicVia :: forall s1 s2 a b c.(Real a, Integral a, RealFrac b, Floating b, Integral c, E.MinCtxt s1 b, E.MinCtxt s2 b) => ExactPi -> (forall d.Floating d => d -> d) -> Proxy b -> SQuantity s1 DOne a -> SQuantity s2 DOne c
liftDimensionlessPeriodicVia p f proxy | Just p'' <- p', p'' /= 0 = (liftDimensionlessVia f proxy) . dmap (`mod` p'')
                                       | otherwise = liftDimensionlessVia f proxy
  where
    p' :: Maybe a
    p' = fmap fromInteger . toExactInteger . recip . (P./ p) . E.exactPiVal $ (Proxy :: Proxy s1)

-- | Forms a possibly scaled 'SQuantity' by multipliying a number and a unit.
(*~) :: forall s m d a b.(RealFrac a, Integral b, E.MinCtxt s a) => a -> Unit m d a -> SQuantity s d b
x *~ (Unit' _ _ y) = Quantity' . round $ (x P.* y P./ s)
  where
    s = E.injMin (Proxy :: Proxy s)

-- | Divides a possibly scaled 'SQuantity' by a 'Unit' of the same physical dimension, obtaining the
-- numerical value of the quantity expressed in that unit.
(/~) :: forall s m d a b.(Real a, Fractional b,  E.MinCtxt s b) => SQuantity s d a -> Unit m d b -> b
(Quantity' x) /~ (Unit' _ _ y) = ((realToFrac x) P.* s P./ y)
  where
    s = E.injMin (Proxy :: Proxy s)
