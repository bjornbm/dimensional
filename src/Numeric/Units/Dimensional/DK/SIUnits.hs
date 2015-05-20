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

module Numeric.Units.Dimensional.DK.SIUnits 
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
  -- * Difftime Conversion
  -- $difftime
  fromDiffTime, toDiffTime
)
where

import Numeric.Units.Dimensional.DK
import Numeric.Units.Dimensional.DK.Quantities
import Numeric.Units.Dimensional.DK.UnitNames (PrefixName, applyPrefix, nMeter, nGram, nSecond, nAmpere, nKelvin, nMole, nCandela)
import qualified Numeric.Units.Dimensional.DK.UnitNames as N
import Numeric.Units.Dimensional.DK.UnitNames.Internal (ucum, ucumMetric)
import Numeric.NumType.DK.Integers ( neg1, neg2, pos2, pos3 )
import Prelude ( (.), ($), Real, realToFrac, Num, Fractional, Floating, Integer, Rational, recip)
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

applyMultiple :: (Num a) => PrefixName -> Integer -> Unit 'Metric d a -> Unit 'NonMetric d a
applyMultiple p x u = compositeNum' (applyPrefix p (name u)) x u

deka, deca, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta
  :: Num a => Unit 'Metric d a -> Unit 'NonMetric d a
deka  = applyMultiple N.deka 10 -- International English.
deca  = deka      -- American English.
hecto = applyMultiple N.hecto 100
kilo  = applyMultiple N.kilo 1e3
mega  = applyMultiple N.mega 1e6
giga  = applyMultiple N.giga 1e9
tera  = applyMultiple N.tera 1e12
peta  = applyMultiple N.peta 1e15
exa   = applyMultiple N.exa 1e18
zetta = applyMultiple N.zetta 1e21
yotta = applyMultiple N.yotta 1e24

{- $submultiples
Then the submultiples.
-}

applySubmultiple :: (Fractional a) => PrefixName -> Rational -> Unit 'Metric d a -> Unit 'NonMetric d a
applySubmultiple p x u = compositeFrac' (applyPrefix p (name u)) x u

deci, centi, milli, micro, nano, pico, femto, atto, zepto, yocto
  :: Fractional a => Unit 'Metric d a -> Unit 'NonMetric d a
deci  = applySubmultiple N.deci 0.1
centi = applySubmultiple N.centi 0.01
milli = applySubmultiple N.milli 1e-3
micro = applySubmultiple N.micro 1e-6
nano  = applySubmultiple N.nano 1e-9
pico  = applySubmultiple N.pico 1e-12
femto = applySubmultiple N.femto 1e-15
atto  = applySubmultiple N.atto 1e-18
zepto = applySubmultiple N.zepto 1e-21
yocto = applySubmultiple N.yocto 1e-24

{- $base-units
These are the base units from section 4.1. To avoid a
myriad of one-letter functions that would doubtlessly cause clashes
and frustration in users' code we spell out all unit names in full,
as we did for prefixes. We also elect to spell the unit names in
singular form, as allowed by section 9.7 "Other spelling conventions".

We define the SI base units in the order of table 1.
-}

metre, meter :: Num a => Unit 'Metric DLength a
metre = compositeNum nMeter 1 siUnit -- International English.
meter = metre         -- American English.

{-

For mass the SI base unit is kilogram. For sensible prefixes we
define gram here (see section 6.2.7 "Prefixes and the kilogram").
The drawback is that we are forced to use 'Fractional'.

-}

gram    :: Fractional a => Unit 'Metric DMass a
gram    = compositeFrac nGram 1e-3 siUnit
second  :: Num a => Unit 'Metric DTime a
second  = compositeNum nSecond 1 siUnit
ampere  :: Num a => Unit 'Metric DElectricCurrent a
ampere  = compositeNum nAmpere 1 siUnit
kelvin  :: Num a => Unit 'Metric DThermodynamicTemperature a
kelvin  = compositeNum nKelvin 1 siUnit
mole    :: Num a => Unit 'Metric DAmountOfSubstance a
mole    = compositeNum nMole 1 siUnit
candela :: Num a => Unit 'Metric DLuminousIntensity a
candela = compositeNum nCandela 1 siUnit

{- $derived-units
From Table 3, SI derived units with special names and symbols, including the
radian and steradian.
-}

radian :: Num a => Unit 'Metric DPlaneAngle a
radian = compositeNum (ucumMetric "rad" "rad" "radian") 1 siUnit -- meter * meter ^ neg1
steradian :: Num a => Unit 'Metric DSolidAngle a
steradian = compositeNum (ucumMetric "sr" "sr" "steradian") 1 siUnit -- meter ^ pos2 * meter ^ neg2
hertz :: Num a => Unit 'Metric DFrequency a
hertz = compositeNum (ucumMetric "Hz" "Hz" "Hertz") 1 $ second ^ neg1
newton :: Num a => Unit 'Metric DForce a
newton = compositeNum (ucumMetric "N" "N" "Newton") 1 $ kilo gram * meter * second ^ neg2
pascal :: Num a => Unit 'Metric DPressure a
pascal = compositeNum (ucumMetric "Pa" "Pa" "Pascal") 1 $ newton / meter ^ pos2
joule :: Num a => Unit 'Metric DEnergy a
joule = compositeNum (ucumMetric "J" "J" "Joule") 1 $ newton * meter
watt :: Num a => Unit 'Metric DPower a
watt = compositeNum (ucumMetric "W" "W" "Watt") 1 $ joule / second
coulomb :: Num a => Unit 'Metric DElectricCharge a
coulomb = compositeNum (ucumMetric "C" "C" "Coulomb") 1 $ second * ampere
volt :: Num a => Unit 'Metric DElectricPotential a
volt = compositeNum (ucumMetric "V" "V" "Volt") 1 $ watt / ampere
farad :: Num a => Unit 'Metric DCapacitance a
farad = compositeNum (ucumMetric "F" "F" "Farad") 1 $ coulomb / volt
ohm :: Num a => Unit 'Metric DElectricResistance a
ohm = compositeNum (ucumMetric "Ohm" "Ω" "Ohm") 1 $ volt / ampere
siemens :: Num a => Unit 'Metric DElectricConductance a
siemens = compositeNum (ucumMetric "S" "S" "Siemens") 1 $ ampere / volt
weber :: Num a => Unit 'Metric DMagneticFlux a
weber = compositeNum (ucumMetric "Wb" "Wb" "Weber") 1 $ volt * second
tesla :: Num a => Unit 'Metric DMagneticFluxDensity a
tesla = compositeNum (ucumMetric "T" "T" "Tesla") 1 $ weber / meter ^ pos2
henry :: Num a => Unit 'Metric DInductance a
henry = compositeNum (ucumMetric "H" "H" "Henry") 1 $ weber / ampere

{-
We defer the definition of Celcius temperature to another section (would
appear here if we stricly followed table 3).
-}

lumen :: Num a => Unit 'Metric DLuminousFlux a
lumen = compositeNum (ucumMetric "lm" "lm" "lumen") 1 $ candela * steradian
lux :: Num a => Unit 'Metric DIlluminance a
lux = compositeNum (ucumMetric "lx" "lx" "lux") 1 $ lumen / meter ^ pos2

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
becquerel = compositeNum (ucumMetric "Bq" "Bq" "Becquerel") 1 $ second ^ neg1
gray :: Num a => Unit 'Metric DAbsorbedDose a
gray = compositeNum (ucumMetric "Gy" "Gy" "Gray") 1 $ joule / kilo gram
sievert :: Num a => Unit 'Metric DDoseEquivalent a
sievert = compositeNum (ucumMetric "Sv" "Sv" "Sievert") 1 $ joule / kilo gram
katal :: Num a => Unit 'Metric DCatalyticActivity a
katal = compositeNum (ucumMetric "kat" "kat" "katal") 1 $ mole / second

{- $accepted-units
There are several units that are not strictly part of the SI but
are either permanently or temporarily accepted for use with the SI.
We define the permanently accepted ones in this module.

From Table 6, Units accepted for use with the SI.

We start with time which we grant exclusive rights to 'minute' and
'second'.
-}
minute, hour, day :: Num a => Unit 'NonMetric DTime a
minute = compositeNum (ucum "min" "min" "minute") 60 $ second
hour   = compositeNum (ucum "h" "h" "hour")       60 $ minute
day    = compositeNum (ucum "d" "d" "day")        24 $ hour -- Mean solar day.

{- $arc-units

Since 'minute' and 'second' are already in use for time we use
'arcminute' and 'arcsecond' <#note2 [2]> for plane angle instead.
-}

degree, arcminute, arcsecond :: Floating a => Unit 'NonMetric DPlaneAngle a
degree = composite (ucum "deg" "°" "degree")       (Prelude.pi Prelude./ 180) $ radian
arcminute = composite (ucum "'" "'" "arcminute")   (recip 60)                 $ degreeOfArc
arcsecond = composite (ucum "''" "''" "arcsecond") (recip 60)                 $ minuteOfArc

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
litre = compositeFrac (ucumMetric "L" "L" "litre") 1 $ deci meter ^ pos3 -- International English.
liter = litre             -- American English.

tonne, metricTon :: Num a => Unit 'Metric DMass a
tonne     = compositeNum (ucumMetric "t" "t" "tonne") 1000 $ kilo gram -- Name in original SI text.
metricTon = tonne                   -- American name.

{- $values-obtained-experimentally
We decline to provide here those units - listed in Table 7 - which,
while accepted for use with the SI, have values which are determined experimentally.
For versioning purposes, those units can be found in "Numeric.Units.Dimensional.DK.NonSI".

However, in 2012 the IAU redefined the astronomical unit as a conventional
unit of length directly tied to the meter, with a length of exactly
149,597,870,700 m and the official abbreviation of au <#note3 [3]>. We therefore include it here.
-}

astronomicalUnit :: Floating a => Unit 'NonMetric DLength a
astronomicalUnit = composite (ucum "AU" "AU" "astronomical unit") 149597870700 $ meter

{- $difftime
It is not within the scope of this library to handle the complex
task of date and time arithmetic. It is recommended to use the
'Data.Time' library for handling dates and using 'Time' quantities
only when time differences are involved in calculations with other
quantities. In order to convert between the 'DiffTime' data type
in the 'Data.Time' library and 'Time' quantities we provide the
functions 'fromDiffTime' and 'toDiffTime'.
-}

{-# DEPRECATED fromDiffTime, toDiffTime "These will probably go away." #-}
fromDiffTime :: (Real a, Fractional b) => a -> Time b
fromDiffTime = (*~ second) . realToFrac
toDiffTime :: (Real a, Fractional a, Fractional b) => Time a -> b
toDiffTime = realToFrac . (/~ second)
