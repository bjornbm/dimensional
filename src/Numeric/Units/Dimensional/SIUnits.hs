{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE RankNTypes #-}

{- |
   Copyright  : Copyright (C) 2006-2015 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn@buckwalter.se
   Stability  : Stable
   Portability: GHC only

= Summary

This module defines the SI prefixes, the SI base units and the SI
derived units. It also defines the units outside of the SI that are
accepted for use with the SI. Any chapters, sections or tables
referenced are from <#note1 [1]> unless otherwise specified.

= References

1. #note1# http://physics.nist.gov/Pubs/SP811/
2. #note2# http://en.wikipedia.org/wiki/Minute_of_arc
3. #note3# http://en.wikipedia.org/wiki/Astronomical_unit

-}

module Numeric.Units.Dimensional.SIUnits
(
  -- * SI Base Units
  -- $base-units
  metre, meter, gram, second, ampere, kelvin, mole, candela,
  -- * SI Derived Units
  -- $derived-units
  radian, steradian, hertz, newton, pascal, joule, watt, coulomb, volt, farad, ohm, siemens, weber, tesla, henry, lumen, lux,
  -- ** Celsius Temperature
  -- $celsius
  degreeCelsius, fromDegreeCelsiusAbsolute, toDegreeCelsiusAbsolute,
  -- ** Units Admitted for Reasons of Safeguarding Human Health
  -- $health
  becquerel, gray, sievert, katal,
  -- * Units Accepted for Use with the SI
  -- $accepted-units
  minute, hour, day,
  hectare, litre, liter, tonne, metricTon,
  -- ** Units of Plane Angle
  -- $arc-units
  degree, arcminute, arcsecond,
  -- $arc-units-alternate
  degreeOfArc, minuteOfArc, secondOfArc,
  -- ** Units Formerly Defined By Experiment
  -- $values-obtained-experimentally
  astronomicalUnit,
  -- * SI Prefixes
  -- $multiples
  deka, deca, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta,
  -- $submultiples
  deci, centi, milli, micro, nano, pico, femto, atto, zepto, yocto,
  -- $reified-prefixes
  Prefix, applyPrefix, applyOptionalPrefix, siPrefixes, appropriatePrefix, withAppropriatePrefix, appropriatePrefix', withAppropriatePrefix'
)
where

import Control.Monad (join)
import Data.Ratio
import Data.List (sortBy, find)
import Data.Ord (comparing, Down(..))
import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.Quantities
import Numeric.Units.Dimensional.UnitNames (Prefix, siPrefixes, scaleExponent)
import qualified Numeric.Units.Dimensional.UnitNames as N
import Numeric.Units.Dimensional.UnitNames.Internal (ucum, ucumMetric)
import qualified Numeric.Units.Dimensional.UnitNames.Internal as I
import Numeric.NumType.DK.Integers ( pos3 )
import Prelude ( Eq(..), ($), (.), Num, Fractional, Floating, RealFrac(..), Maybe(..), otherwise, error, Ord(..), fst, snd, Int, Bool, fmap, mod, (&&))
import qualified Prelude

{- $multiples
Prefixes are used to form decimal multiples and submultiples of SI
Units as described in section 4.4. We will define the SI prefixes
in terms of the 'prefix' function which applies a scale factor to a
unit.

By defining SI prefixes as functions applied to a 'Unit' we satisfy
section 6.2.6 "Unacceptability of stand-alone prefixes".

We define all SI prefixes from Table 5. Multiples first.
-}

applyMultiple :: (Num a) => Prefix -> Unit 'Metric d a -> Unit 'NonMetric d a
applyMultiple p u | denominator x == 1 = mkUnitZ n' (numerator x) u
                  | otherwise = error "Attempt to apply a submultiple prefix as a multiple."
  where
    n' = N.applyPrefix p (name u)
    x = N.scaleFactor p

deka, deca, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta
  :: Num a => Unit 'Metric d a -> Unit 'NonMetric d a
deka  = applyMultiple I.deka -- International English.
deca  = deka      -- American English.
hecto = applyMultiple I.hecto
kilo  = applyMultiple I.kilo
mega  = applyMultiple I.mega
giga  = applyMultiple I.giga
tera  = applyMultiple I.tera
peta  = applyMultiple I.peta
exa   = applyMultiple I.exa
zetta = applyMultiple I.zetta
yotta = applyMultiple I.yotta

{- $submultiples
Then the submultiples.
-}

-- | Applies a 'Prefix' to a 'Metric' 'Unit', creating a 'NonMetric' unit.
applyPrefix :: (Fractional a) => Prefix -> Unit 'Metric d a -> Unit 'NonMetric d a
applyPrefix p u = mkUnitQ n' x u
  where
    n' = N.applyPrefix p (name u)
    x = N.scaleFactor p

-- | Applies an optional 'Prefix' to a 'Metric' 'Unit', creating a 'NonMetric' unit.
applyOptionalPrefix :: (Fractional a) => Maybe Prefix -> Unit 'Metric d a -> Unit 'NonMetric d a
applyOptionalPrefix Nothing = weaken
applyOptionalPrefix (Just p) = applyPrefix p

deci, centi, milli, micro, nano, pico, femto, atto, zepto, yocto
  :: Fractional a => Unit 'Metric d a -> Unit 'NonMetric d a
deci  = applyPrefix I.deci
centi = applyPrefix I.centi
milli = applyPrefix I.milli
micro = applyPrefix I.micro
nano  = applyPrefix I.nano
pico  = applyPrefix I.pico
femto = applyPrefix I.femto
atto  = applyPrefix I.atto
zepto = applyPrefix I.zepto
yocto = applyPrefix I.yocto

{- $reified-prefixes

We supply an explicit representation of an SI prefix, along with a function to apply one and a
list of all prefixes defined by the SI.

-}

-- | Selects the appropriate 'Prefix' to use with a 'Metric' unit when using it to display
-- a particular 'Quantity', or 'Nothing' if the supplied unit should be used without a prefix.
--
-- The appropriate prefix is defined to be the largest prefix such that the resulting value
-- of the quantity, expressed in the prefixed unit, is greater than or equal to one.
--
-- Note that the supplied prefix need not be 'Metric'. This is intended for use to compute a prefix to insert
-- somewhere in the denominator of a composite (and hence 'NonMetric') unit.
appropriatePrefix :: (Floating a, RealFrac a) => Unit m d a -> Quantity d a -> Maybe Prefix
appropriatePrefix u q = selectPrefix (<= e)
  where
    val = q /~ u
    e = Prelude.floor $ Prelude.logBase 10 val :: Prelude.Int

-- | Selects the appropriate 'Prefix' to use with a 'Metric' unit when using it to display
-- a particular 'Quantity', or 'Nothing' if the supplied unit should be used without a prefix.
--
-- The appropriate prefix is defined to be the largest prefix such that the resulting value
-- of the quantity, expressed in the prefixed unit, is greater than or equal to one. Only those prefixes
-- whose 'scaleExponent' is a multiple of @3@ are considered.
--
-- Note that the supplied prefix need not be 'Metric'. This is intended for use to compute a prefix to insert
-- somewhere in the denominator of a composite (and hence 'NonMetric') unit.
appropriatePrefix' :: (Floating a, RealFrac a) => Unit m d a -> Quantity d a -> Maybe Prefix
appropriatePrefix' u q = selectPrefix (\x -> x `mod` 3 == 0 && x <= e)
  where
    val = q /~ u
    e = Prelude.floor $ Prelude.logBase 10 val :: Prelude.Int

-- Selects the first prefix in the list of prefix candidates whose scale exponent matches the supplied predicate.
selectPrefix :: (Int -> Bool) -> Maybe Prefix
selectPrefix p = join $ fmap snd $ find (p . fst) prefixCandidates

-- This is a list of candidate prefixes and the least scale exponent at which each applies.
prefixCandidates :: [(Int, Maybe Prefix)]
prefixCandidates = sortBy (comparing $ Down . fst) $ (0, Nothing) : fmap (\x -> (scaleExponent x, Just x)) siPrefixes

-- | Constructs a version of a 'Metric' unit, by possibly applying a 'Prefix' to it, appropriate
-- for display of a particular 'Quantity'.
--
-- The appropriate prefix is defined to be the largest prefix such that the resulting value
-- of the quantity, expressed in the prefixed unit, is greater than or equal to one.
withAppropriatePrefix :: (Floating a, RealFrac a) => Unit 'Metric d a -> Quantity d a -> Unit 'NonMetric d a
withAppropriatePrefix u q = applyOptionalPrefix (appropriatePrefix u q) u

-- | Constructs a version of a 'Metric' unit, by possibly applying a 'Prefix' to it, appropriate
-- for display of a particular 'Quantity'.
--
-- The appropriate prefix is defined to be the largest prefix such that the resulting value
-- of the quantity, expressed in the prefixed unit, is greater than or equal to one. Only those prefixes
-- whose 'scaleExponent' is a multiple of @3@ are considered.
withAppropriatePrefix' :: (Floating a, RealFrac a) => Unit 'Metric d a -> Quantity d a -> Unit 'NonMetric d a
withAppropriatePrefix' u q = applyOptionalPrefix (appropriatePrefix' u q) u

{- $base-units
These are the base units from section 4.1. To avoid a
myriad of one-letter functions that would doubtlessly cause clashes
and frustration in users' code we spell out all unit names in full,
as we did for prefixes. We also elect to spell the unit names in
singular form, as allowed by section 9.7 "Other spelling conventions".

We define the SI base units in the order of table 1.
-}

metre, meter :: Num a => Unit 'Metric DLength a
metre = mkUnitZ I.nMeter 1 siUnit -- International English.
meter = metre         -- American English.

{-

For mass the SI base unit is kilogram. For sensible prefixes we
define gram here (see section 6.2.7 "Prefixes and the kilogram").
The drawback is that we are forced to use 'Fractional'.

-}

gram    :: Fractional a => Unit 'Metric DMass a
gram    = mkUnitQ I.nGram 1e-3 siUnit
second  :: Num a => Unit 'Metric DTime a
second  = mkUnitZ I.nSecond 1 siUnit
ampere  :: Num a => Unit 'Metric DElectricCurrent a
ampere  = mkUnitZ I.nAmpere 1 siUnit
kelvin  :: Num a => Unit 'Metric DThermodynamicTemperature a
kelvin  = mkUnitZ I.nKelvin 1 siUnit
mole    :: Num a => Unit 'Metric DAmountOfSubstance a
mole    = mkUnitZ I.nMole 1 siUnit
candela :: Num a => Unit 'Metric DLuminousIntensity a
candela = mkUnitZ I.nCandela 1 siUnit

{- $derived-units
From Table 3, SI derived units with special names and symbols, including the
radian and steradian.
-}

radian :: Num a => Unit 'Metric DPlaneAngle a
radian = mkUnitZ (ucumMetric "rad" "rad" "radian") 1 siUnit -- meter * meter ^ neg1
steradian :: Num a => Unit 'Metric DSolidAngle a
steradian = mkUnitZ (ucumMetric "sr" "sr" "steradian") 1 siUnit -- meter ^ pos2 * meter ^ neg2
hertz :: Num a => Unit 'Metric DFrequency a
hertz = mkUnitZ (ucumMetric "Hz" "Hz" "Hertz") 1 $ siUnit
newton :: Num a => Unit 'Metric DForce a
newton = mkUnitZ (ucumMetric "N" "N" "Newton") 1 $ siUnit
pascal :: Num a => Unit 'Metric DPressure a
pascal = mkUnitZ (ucumMetric "Pa" "Pa" "Pascal") 1 $ siUnit
joule :: Num a => Unit 'Metric DEnergy a
joule = mkUnitZ (ucumMetric "J" "J" "Joule") 1 $ siUnit
watt :: Num a => Unit 'Metric DPower a
watt = mkUnitZ (ucumMetric "W" "W" "Watt") 1 $ siUnit
coulomb :: Num a => Unit 'Metric DElectricCharge a
coulomb = mkUnitZ (ucumMetric "C" "C" "Coulomb") 1 $ siUnit
volt :: Num a => Unit 'Metric DElectricPotential a
volt = mkUnitZ (ucumMetric "V" "V" "Volt") 1 $ siUnit
farad :: Num a => Unit 'Metric DCapacitance a
farad = mkUnitZ (ucumMetric "F" "F" "Farad") 1 $ siUnit
ohm :: Num a => Unit 'Metric DElectricResistance a
ohm = mkUnitZ (ucumMetric "Ohm" "Ω" "Ohm") 1 $ siUnit
siemens :: Num a => Unit 'Metric DElectricConductance a
siemens = mkUnitZ (ucumMetric "S" "S" "Siemens") 1 $ siUnit
weber :: Num a => Unit 'Metric DMagneticFlux a
weber = mkUnitZ (ucumMetric "Wb" "Wb" "Weber") 1 $ siUnit
tesla :: Num a => Unit 'Metric DMagneticFluxDensity a
tesla = mkUnitZ (ucumMetric "T" "T" "Tesla") 1 $ siUnit
henry :: Num a => Unit 'Metric DInductance a
henry = mkUnitZ (ucumMetric "H" "H" "Henry") 1 $ siUnit

{-
We defer the definition of Celcius temperature to another section (would
appear here if we stricly followed table 3).
-}

lumen :: Num a => Unit 'Metric DLuminousFlux a
lumen = mkUnitZ (ucumMetric "lm" "lm" "lumen") 1 $ siUnit
lux :: Num a => Unit 'Metric DIlluminance a
lux = mkUnitZ (ucumMetric "lx" "lx" "lux") 1 $ siUnit

{- $celsius
A problematic area is units which increase proportionally to the
base SI units but cross zero at a different point. An example would
be degrees Celsius (see section 4.2.1.1). The author feels that it
is appropriate to define a unit for use with relative quantities
(taking only into account the proportionality) and complement the
unit with functions for converting absolute values.

The function 'fromDegreeCelsiusAbsolute' should be used in lieu of
"*~ degreeCelsius" when working with absolute temperatures. Similarily,
'toDegreeCelsiusAbsolute' should be used in lieu of "/~ degreeCelsius"
when working with absolute temperatures.
-}

degreeCelsius :: Num a => Unit 'Metric DCelsiusTemperature a
degreeCelsius = kelvin

fromDegreeCelsiusAbsolute :: Floating a => a -> ThermodynamicTemperature a
fromDegreeCelsiusAbsolute x = x *~ degreeCelsius + 273.15 *~ degreeCelsius
toDegreeCelsiusAbsolute :: Floating a => ThermodynamicTemperature a -> a
toDegreeCelsiusAbsolute x = (x - 273.15 *~ degreeCelsius) /~ degreeCelsius

{- $health

The last units from Table 3 are SI derived units with special names and symbols admitted for reasons
of safeguarding human health.
-}

becquerel :: Num a => Unit 'Metric DActivity a
becquerel = mkUnitZ (ucumMetric "Bq" "Bq" "Becquerel") 1 $ siUnit
gray :: Num a => Unit 'Metric DAbsorbedDose a
gray = mkUnitZ (ucumMetric "Gy" "Gy" "Gray") 1 $ siUnit
sievert :: Num a => Unit 'Metric DDoseEquivalent a
sievert = mkUnitZ (ucumMetric "Sv" "Sv" "Sievert") 1 $ siUnit
katal :: Num a => Unit 'Metric DCatalyticActivity a
katal = mkUnitZ (ucumMetric "kat" "kat" "katal") 1 $ siUnit

{- $accepted-units
There are several units that are not strictly part of the SI but
are either permanently or temporarily accepted for use with the SI.
We define the permanently accepted ones in this module.

From Table 6, Units accepted for use with the SI.

We start with time which we grant exclusive rights to 'minute' and
'second'.
-}
minute, hour, day :: Num a => Unit 'NonMetric DTime a
minute = mkUnitZ (ucum "min" "min" "minute") 60 $ second
hour   = mkUnitZ (ucum "h" "h" "hour")       60 $ minute
day    = mkUnitZ (ucum "d" "d" "day")        24 $ hour -- Mean solar day.

{- $arc-units

Since 'minute' and 'second' are already in use for time we use
'arcminute' and 'arcsecond' <#note2 [2]> for plane angle instead.
-}

degree, arcminute, arcsecond :: Floating a => Unit 'NonMetric DPlaneAngle a
degree    = mkUnitR (ucum "deg" "°" "degree")    (Prelude.pi Prelude./ 180) $ radian
arcminute = mkUnitR (ucum "'" "'" "arcminute")   (Prelude.recip 60)         $ degreeOfArc
arcsecond = mkUnitR (ucum "''" "''" "arcsecond") (Prelude.recip 60)         $ minuteOfArc

{- $arc-units-alternate
Alternate (longer) forms of the above. In particular 'degreeOfArc'
can be used if there is a percieved need to disambiguate from e.g.
temperature.
-}

degreeOfArc, minuteOfArc, secondOfArc :: Floating a => Unit 'NonMetric DPlaneAngle a
degreeOfArc = degree
secondOfArc = arcsecond
minuteOfArc = arcminute

hectare :: Fractional a => Unit 'NonMetric DArea a
hectare = square (hecto meter)

litre, liter :: Fractional a => Unit 'Metric DVolume a
litre = mkUnitQ (ucumMetric "L" "L" "litre") 1 $ deci meter ^ pos3 -- International English.
liter = litre             -- American English.

tonne, metricTon :: Num a => Unit 'Metric DMass a
tonne     = mkUnitZ (ucumMetric "t" "t" "tonne") 1000 $ siUnit -- Name in original SI text.
metricTon = tonne                   -- American name.

{- $values-obtained-experimentally
We decline to provide here those units - listed in Table 7 - which,
while accepted for use with the SI, have values which are determined experimentally.
For versioning purposes, those units can be found in "Numeric.Units.Dimensional.NonSI".

However, in 2012 the IAU redefined the astronomical unit as a conventional
unit of length directly tied to the meter, with a length of exactly
149 597 870 700 m and the official abbreviation of au <#note3 [3]>. We therefore include it here.
-}

astronomicalUnit :: Num a => Unit 'NonMetric DLength a
astronomicalUnit = mkUnitZ (ucum "AU" "AU" "astronomical unit") 149597870700 $ meter
