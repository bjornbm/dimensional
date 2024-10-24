{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{- |
    Copyright  : Copyright (C) 2006-2018 Bjorn Buckwalter
    License    : BSD3

    Maintainer : bjorn@buckwalter.se
    Stability  : Experimental
    Portability: GHC only?

Defines types for manipulation of quantities with fixed point representations.
-}
module Numeric.Units.Dimensional.FixedPoint
(
  -- * Types
  -- $types
  Dimensional,
  Unit, Quantity, SQuantity,
  Metricality(..),
  -- * Physical Dimensions
  Dimension (Dim),
  -- ** Dimension Arithmetic
  type (*), type (/), type (^), NRoot, Recip,
  -- ** Term Level Representation of Dimensions
  Dimension' (Dim'), HasDimension(..), KnownDimension,
  -- * Dimensional Arithmetic
  (*~), (/~),
  (*), (/), (+), (-),
  negate, abs,
  -- ** Transcendental Functions
  -- *** Via 'Double'
  expD, logD, sinD, cosD, tanD, asinD, acosD, atanD, sinhD, coshD, tanhD, asinhD, acoshD, atanhD, atan2D,
  -- *** Via arbitary 'Floating' type
  expVia, logVia, sinVia, cosVia, tanVia, asinVia, acosVia, atanVia, sinhVia, coshVia, tanhVia, asinhVia, acoshVia, atanhVia, atan2Via,
  -- ** Operations on Collections
  (*~~), (/~~), sum, mean, -- dimensionlessLength, nFromTo,
  -- ** Conversion Between Representations
  rescale, rescaleFinite, rescaleD, rescaleVia, KnownVariant(dmap), changeRep, changeRepRound, changeRepApproximate,
  -- * Dimension Synonyms
  DOne, DLength, DMass, DTime, DElectricCurrent, DThermodynamicTemperature, DAmountOfSubstance, DLuminousIntensity,
  -- * Quantity Synonyms
  Dimensionless, Length, Mass, Time, ElectricCurrent, ThermodynamicTemperature, AmountOfSubstance, LuminousIntensity,
  -- * Constants
  _0, epsilon,
  -- $possibly-imprecise-constants
  _1, _2, _3, _4, _5, _6, _7, _8, _9, pi, tau,
  -- * Constructing Units
  siUnit, one, mkUnitR, mkUnitQ, mkUnitZ,
  -- * Unit Metadata
  name, exactValue, weaken, strengthen, exactify,
  -- * Commonly Used Type Synonyms
  -- $synonyms
  type Q, type QScale, type Angle8, type Angle16, type Angle32
)
where

import Data.Bits
import Data.ExactPi
import qualified Data.ExactPi.TypeLevel as E
import Data.Int
import Data.Proxy
import qualified Data.Foldable as F
import Data.Ratio
import qualified GHC.TypeLits as N
import Numeric.Units.Dimensional.Coercion
import Numeric.Units.Dimensional.Internal
import Numeric.Units.Dimensional.Prelude hiding ((*~), (/~), (+), (-), recip, negate, abs, (*~~), (/~~), sum, mean, _0, _1, _2, _3, _4, _5, _6, _7, _8, _9, pi, tau, changeRep)
import Numeric.Units.Dimensional.Variants hiding (type (*), type (/))
import qualified Numeric.Units.Dimensional.UnitNames as Name
import qualified Prelude as P

{- $types

We provide access to the same 'Dimensional', 'Unit', and 'Quantity' types as are exposed by "Numeric.Units.Dimensional", but additionally
offer the 'SQuantity' type to represent scaled quantities. Fixed-point quantities are quantities backed by integers, it is frequently
necessary to scale those integers into a range appropriate for the physical problem at hand.

-}

{-

Arithmetic Operators and Functions

We will reuse the operators and function names from the Prelude.
To prevent unpleasant surprises we give operators the same fixity
as the Prelude.

-}

--infixr 8  ^, ^/, **
infixl 6  +, -

-- | Adds two possibly scaled 'SQuantity's, preserving any scale factor.
--
-- Use in conjunction with 'changeRepRound' to combine quantities with differing scale factors.
(+) :: (Num a) => SQuantity s d a -> SQuantity s d a -> SQuantity s d a
(+) = liftQ2 (P.+)

-- | Subtracts one possibly scaled 'SQuantity' from another, preserving any scale factor.
--
-- Use in conjunction with 'changeRepRound' to combine quantities with differing scale factors.
(-) :: (Num a) => SQuantity s d a -> SQuantity s d a -> SQuantity s d a
(-) = liftQ2 (P.-)

-- | Takes the absolute value of a possibly scaled 'SQuantity', preserving any scale factor.
abs :: (Num a) => SQuantity s d a -> SQuantity s d a
abs = liftQ (P.abs)

-- | Negates the value of a possibly scaled 'SQuantity', preserving any scale factor.
negate :: (Num a) => SQuantity s d a -> SQuantity s d a
negate = liftQ (P.negate)

infixl 7  *~~, /~~

-- | Applies '*~' to all values in a functor.
(*~~) :: (Functor f, RealFrac a, Integral b, E.MinCtxt s a) => f a -> Unit m d a -> f (SQuantity s d b)
xs *~~ u = fmap (*~ u) xs

-- | Applies '/~' to all values in a functor.
(/~~) :: (Functor f, Real a, Fractional b, E.MinCtxt s b) => f (SQuantity s d a) -> Unit m d b -> f b
xs /~~ u = fmap (/~ u) xs

-- | The sum of all elements in a list.
sum :: (Num a, F.Foldable f) => f (SQuantity s d a) -> SQuantity s d a
sum = F.foldr (+) _0

-- | The arithmetic mean of all elements in a list.
mean :: (Fractional a, F.Foldable f) => f (SQuantity s d a) -> SQuantity s d a
mean = reduce . F.foldr accumulate (_0, 0 :: Int)
  where
    reduce (s, n) = dmap (P./ fromIntegral n) s
    accumulate val (accum, count) = (accum + val, count P.+ 1)

expD, logD, sinD, cosD, tanD, asinD, acosD, atanD, sinhD, coshD, tanhD, asinhD, acoshD, atanhD
  :: (Integral a, Integral b, E.MinCtxt s1 Double, E.MinCtxt s2 Double) => SQuantity s1 DOne a -> SQuantity s2 DOne b
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

-- | The standard two argument arctangent function.
-- Since it interprets its two arguments in comparison with one another, the input may have any dimension.
atan2D :: (Integral a, Integral b, E.MinCtxt s1 Double, E.MinCtxt s2 Double, E.MinCtxt s3 Double) => SQuantity s1 DOne a -> SQuantity s2 DOne a -> SQuantity s3 DOne b
atan2D = atan2Via (Proxy :: Proxy P.Double)

expVia, logVia, sinVia, cosVia, tanVia, asinVia, acosVia, atanVia, sinhVia, coshVia, tanhVia, asinhVia, acoshVia, atanhVia
  :: (Integral a, RealFrac b, Floating b, Integral c, E.MinCtxt s1 b, E.MinCtxt s2 b) => Proxy b -> SQuantity s1 DOne a -> SQuantity s2 DOne c
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

-- | The standard two argument arctangent function.
-- Since it interprets its two arguments in comparison with one another, the input may have any dimension.
atan2Via :: forall s1 s2 s3 a b c d.(Integral a, RealFloat b, Integral c, E.MinCtxt s1 b, E.MinCtxt s2 b, E.MinCtxt s3 b, KnownDimension d) => Proxy b -> SQuantity s1 d a -> SQuantity s2 d a -> SQuantity s3 DOne c
atan2Via _ y x = (*~ siUnit) $ (P.atan2 :: b -> b -> b) (y /~ siUnit) (x /~ siUnit)

-- | Lift a function on dimensionless values of a specified intermediate type to operate on possibly scaled dimensionless values.
liftDimensionlessVia :: forall s1 s2 a b c.(Real a, RealFrac b, Integral c, E.MinCtxt s1 b, E.MinCtxt s2 b) => (b -> b) -> Proxy b -> SQuantity s1 DOne a -> SQuantity s2 DOne c
liftDimensionlessVia f _ = (*~ siUnit) . (f :: b -> b) . (/~ siUnit)

-- | Lift a periodic function on dimensionless values of a specified intermediate type to operate on possibly scaled dimensionless values.
--
-- If the scale factor of the input type is an exact integer divisor of the function's period, the argument
-- will be clamped via an integer `mod` operation prior to applying the function to avoid errors introduced by a floating point modulus.
liftDimensionlessPeriodicVia :: forall s1 s2 a b c.(Integral a, RealFrac b, Floating b, Integral c, E.MinCtxt s1 b, E.MinCtxt s2 b) => ExactPi -> (forall d.Floating d => d -> d) -> Proxy b -> SQuantity s1 DOne a -> SQuantity s2 DOne c
liftDimensionlessPeriodicVia p f proxy | Just p'' <- p', p'' /= 0 = (liftDimensionlessVia f proxy) . dmap (`mod` p'')
                                       | otherwise = liftDimensionlessVia f proxy
  where
    p' :: Maybe a
    p' = fmap fromInteger . toExactInteger . P.recip . (P./ p) . E.exactPiVal $ (Proxy :: Proxy s1)

{-
We give '*~' and '/~' the same fixity as '*' and '/' defined below.
Note that this necessitates the use of parenthesis when composing
units using '*' and '/', e.g. "1 *~ (meter / second)".
-}

infixl 7  *~, /~

-- | Forms a possibly scaled 'SQuantity' by multipliying a number and a unit.
(*~) :: forall s m d a b.(RealFrac a, Integral b, E.MinCtxt s a) => a -> Unit m d a -> SQuantity s d b
x *~ (Unit _ _ y) = Quantity . round $ (x P.* y P./ s)
  where
    s = E.injMin (Proxy :: Proxy s)

-- | Divides a possibly scaled 'SQuantity' by a 'Unit' of the same physical dimension, obtaining the
-- numerical value of the quantity expressed in that unit.
(/~) :: forall s m d a b.(Real a, Fractional b,  E.MinCtxt s b) => SQuantity s d a -> Unit m d b -> b
(Quantity x) /~ (Unit _ _ y) = ((realToFrac x) P.* s P./ y)
  where
    s = E.injMin (Proxy :: Proxy s)

{-

Rescaling Operations

-}

-- | Rescales a fixed point quantity, accomodating changes both in its scale factor and its representation type.
--
-- Note that this uses an arbitrary precision representation of 'pi', which may be quite slow.
rescale :: forall a b d s1 s2.(Integral a, Integral b, E.KnownExactPi s1, E.KnownExactPi s2) => SQuantity s1 d a -> SQuantity s2 d b
rescale | Just s' <- toExactInteger s           = viaInteger (P.* s')
        | Just s' <- toExactInteger (P.recip s) = viaInteger (`P.quot` s')
        | Just q  <- toExactRational s          = viaInteger $ timesRational q
        | otherwise                             = viaInteger $ \x -> fixedPoint (fmap (($ x) . timesRational) (rationalApproximations s))
  where
    s = (s1' P./ s2')
    s1' = E.exactPiVal (Proxy :: Proxy s1)
    s2' = E.exactPiVal (Proxy :: Proxy s2)
    timesRational :: Rational -> Integer -> Integer
    timesRational q = (`P.quot` denominator q) . (P.* numerator q)

-- | Rescales a fixed point quantity, accomodating changes both in its scale factor and its representation type.
--
-- Expected to outperform `rescale` when a `FiniteBits` context is available for the source and destination representation types.
rescaleFinite :: (Integral a, FiniteBits a, Integral b, FiniteBits b, E.KnownExactPi s1, E.KnownExactPi s2) => SQuantity s1 d a -> SQuantity s2 d b
rescaleFinite = rescale -- It should be possible to do this more quickly, since we have a priori knowledge of how well we need to approximate the result

-- | Approximately rescales a fixed point quantity, accomodating changes both in its scale factor and its representation type.
--
-- Uses approximate arithmetic by way of an intermediate `Floating` type, to which a proxy must be supplied.
rescaleVia :: forall a b c d s1 s2.(Integral a, RealFrac b, Floating b, Integral c, E.KnownExactPi s1, E.KnownExactPi s2) => Proxy b -> SQuantity s1 d a -> SQuantity s2 d c
rescaleVia _ = viaIntermediate (P.* s)
  where
    s = approximateValue (s1' P./ s2') :: b
    s1' = E.exactPiVal $ (Proxy :: Proxy s1)
    s2' = E.exactPiVal $ (Proxy :: Proxy s2)

-- | Approximately rescales a fixed point quantity, accomodating changes both in its scale factor and its representation type.
--
-- Uses approximate arithmetic by way of an intermediate `Double` representation.
rescaleD :: (Integral a, Integral b, E.KnownExactPi s1, E.KnownExactPi s2) => SQuantity s1 d a -> SQuantity s2 d b
rescaleD = rescaleVia (Proxy :: Proxy Double)

-- Note that this does not respect scaling factors at all.
viaInteger :: (Integral a, Integral b) => (P.Integer -> P.Integer) -> SQuantity s1 d a -> SQuantity s2 d b
viaInteger f = Quantity . fromInteger . f . fromIntegral . unQuantity

-- Note that this does not respect scaling factors at all.
viaIntermediate :: (Integral a, RealFrac b, Integral c) => (b -> b) -> SQuantity s1 d a -> SQuantity s2 d c
viaIntermediate f = Quantity . round . f . fromIntegral . unQuantity

fixedPoint :: (Eq a) => [a] -> a
fixedPoint []                     = error "Fixed point of empty list."
fixedPoint [x]                    = x
fixedPoint (x1:x2:xs) | x1 == x2  = x1
                      | otherwise = fixedPoint (x2:xs)

{-

Changes of Representation

-}

-- | Convenient conversion between numerical types while retaining dimensional information.
changeRep :: forall v1 v2 d a b.
            (KnownVariant v1, KnownVariant v2,
             CompatibleVariants v1 v2,
             E.MinCtxt (ScaleFactor v1 E./ ScaleFactor v2) b,
             Real a, Fractional b)
          => Dimensional v1 d a -> Dimensional v2 d b
changeRep = liftD (P.* s) ((P.* s') . realToFrac) Name.weaken
  where
    p :: Proxy (ScaleFactor v1 E./ ScaleFactor v2)
    p = Proxy
    s = E.exactPiVal p
    s' = E.injMin p

-- | Convenient conversion to types with `Integral` representations using `round`.
changeRepRound :: forall v1 v2 d a b.
                 (KnownVariant v1, KnownVariant v2,
                  CompatibleVariants v1 v2,
                  E.MinCtxt (ScaleFactor v1 E./ ScaleFactor v2) a,
                  RealFrac a, Integral b)
               => Dimensional v1 d a -> Dimensional v2 d b
changeRepRound = liftD (P.* s) (round . (P.* s')) Name.weaken
  where
    p :: Proxy (ScaleFactor v1 E./ ScaleFactor v2)
    p = Proxy
    s = E.exactPiVal p
    s' = E.injMin p

{-

Useful Constant Values

-}

{- $possibly-imprecise-constants

Note that, other than '_0' and 'epsilon', these constants may not be exactly representable with certain scale factors.

-}

-- | The constant for zero is polymorphic, allowing
-- it to express zero 'Length' or 'Capacitance' or 'Velocity' etc, in addition
-- to the 'Dimensionless' value zero.
_0 :: Num a => SQuantity s d a
_0 = Quantity 0

_1, _2, _3, _4, _5, _6, _7, _8, _9 :: (Integral a, E.KnownExactPi s) => SQuantity s DOne a
_1 = rescale (epsilon :: SQuantity E.One DOne Integer)
_2 = rescale (epsilon :: SQuantity (E.ExactNatural 2) DOne Integer)
_3 = rescale (epsilon :: SQuantity (E.ExactNatural 3) DOne Integer)
_4 = rescale (epsilon :: SQuantity (E.ExactNatural 4) DOne Integer)
_5 = rescale (epsilon :: SQuantity (E.ExactNatural 5) DOne Integer)
_6 = rescale (epsilon :: SQuantity (E.ExactNatural 6) DOne Integer)
_7 = rescale (epsilon :: SQuantity (E.ExactNatural 7) DOne Integer)
_8 = rescale (epsilon :: SQuantity (E.ExactNatural 8) DOne Integer)
_9 = rescale (epsilon :: SQuantity (E.ExactNatural 9) DOne Integer)

pi :: (Integral a, E.KnownExactPi s) => SQuantity s DOne a
pi = rescale (epsilon :: SQuantity E.Pi DOne Integer)

-- | Twice 'pi'.
--
-- For background on 'tau' see [The Tau Manifesto](https://tauday.com/tau-manifesto) (but also
-- feel free to review [The Pi Manifesto](https://web.archive.org/web/20200926221249/http://www.thepimanifesto.com/)).
tau :: (Integral a, E.KnownExactPi s) => SQuantity s DOne a
tau = rescale (epsilon :: SQuantity (E.ExactNatural 2 E.* E.Pi) DOne Integer)

-- | The smallest positive representable value in a given fixed-point scaled quantity type.
epsilon :: (Integral a) => SQuantity s d a
epsilon = Quantity 1

{- $synonyms

These type synonyms for commonly used fixed-point types are provided for convenience.

-}

-- | A binary scale factor.
type QScale n = (E.One E./ (E.ExactNatural (2 N.^ n)))

-- | A dimensionless number with `n` fractional bits, using a representation of type `a`.
type Q n a = SQuantity (QScale n) DOne a

-- | A single-turn angle represented as a signed 8-bit integer.
type Angle8  = SQuantity (E.Pi E.* (QScale 7))  DPlaneAngle Int8

-- | A single-turn angle represented as a signed 16-bit integer.
type Angle16 = SQuantity (E.Pi E.* (QScale 15)) DPlaneAngle Int16

-- | A single-turn angle represented as a signed 32-bit integer.
type Angle32 = SQuantity (E.Pi E.* (QScale 31)) DPlaneAngle Int32
