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
import Numeric.NumType.DK.Integers ( pos3 )
import Prelude ( (.), Num, Real, realToFrac, Fractional, Floating, recip )
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

deka, deca, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta
  :: Num a => Unit d a -> Unit d a
deka  = prefix 10 -- International English.
deca  = deka      -- American English.
hecto = prefix 100
kilo  = prefix 1000
mega  = kilo . kilo
giga  = kilo . mega
tera  = kilo . giga
peta  = kilo . tera
exa   = kilo . peta
zetta = kilo . exa
yotta = kilo . zetta

{- $submultiples
Then the submultiples.
-}

deci, centi, milli, micro, nano, pico, femto, atto, zepto, yocto
  :: Fractional a => Unit d a -> Unit d a
deci  = prefix 0.1
centi = prefix 0.01
milli = prefix 1e-3
micro = prefix 1e-6
nano  = prefix 1e-9
pico  = prefix 1e-12
femto = prefix 1e-15
atto  = prefix 1e-18
zepto = prefix 1e-21
yocto = prefix 1e-24

{- $base-units
These are the base units from section 4.1. To avoid a
myriad of one-letter functions that would doubtlessly cause clashes
and frustration in users' code we spell out all unit names in full,
as we did for prefixes. We also elect to spell the unit names in
singular form, as allowed by section 9.7 "Other spelling conventions".

We define the SI base units in the order of table 1.
-}

metre, meter :: Num a => Unit DLength a
metre = siUnit -- International English.
meter = metre         -- American English.

{-

For mass the SI base unit is kilogram. For sensible prefixes we
define gram here (see section 6.2.7 "Prefixes and the kilogram").
The drawback is that we are forced to use 'Fractional'.

-}

gram    :: Fractional a => Unit DMass a
gram    = milli siUnit
second  :: Num a => Unit DTime a
second  = siUnit
ampere  :: Num a => Unit DElectricCurrent a
ampere  = siUnit
kelvin  :: Num a => Unit DThermodynamicTemperature a
kelvin  = siUnit
mole    :: Num a => Unit DAmountOfSubstance a
mole    = siUnit
candela :: Num a => Unit DLuminousIntensity a
candela = siUnit

{- $derived-units
From Table 3, SI derived units with special names and symbols, including the
radian and steradian.
-}

radian :: Num a => Unit DPlaneAngle a
radian = one -- meter * meter ^ neg1
steradian :: Num a => Unit DSolidAngle a
steradian = one -- meter ^ pos2 * meter ^ neg2
hertz :: Num a => Unit DFrequency a
hertz = siUnit -- second ^ neg1
newton :: Num a => Unit DForce a
newton = siUnit -- kilo gram * meter * second ^ neg2
pascal :: Num a => Unit DPressure a
pascal = siUnit -- newton / meter ^ pos2
joule :: Num a => Unit DEnergy a
joule = siUnit -- newton * meter
watt :: Num a => Unit DPower a
watt = siUnit -- joule / second
coulomb :: Num a => Unit DElectricCharge a
coulomb = siUnit -- second * ampere
volt :: Num a => Unit DElectricPotential a
volt = siUnit -- watt / ampere
farad :: Num a => Unit DCapacitance a
farad = siUnit -- coulomb / volt
ohm :: Num a => Unit DElectricResistance a
ohm = siUnit -- volt / ampere
siemens :: Num a => Unit DElectricConductance a
siemens = siUnit -- ampere / volt
weber :: Num a => Unit DMagneticFlux a
weber = siUnit -- volt * second
tesla :: Num a => Unit DMagneticFluxDensity a
tesla = siUnit -- weber / meter ^ pos2
henry :: Num a => Unit DInductance a
henry = siUnit -- weber / ampere

{-
We defer the definition of Celcius temperature to another section (would
appear here if we stricly followed table 3).
-}

lumen :: Num a => Unit DLuminousFlux a
lumen = siUnit -- candela * steradian
lux :: Num a => Unit DIlluminance a
lux = siUnit -- lumen / meter ^ pos2

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

degreeCelsius :: Num a => Unit DCelsiusTemperature a
degreeCelsius = kelvin

fromDegreeCelsiusAbsolute :: Fractional a => a -> ThermodynamicTemperature a
fromDegreeCelsiusAbsolute x = x *~ degreeCelsius + 273.15 *~ degreeCelsius
toDegreeCelsiusAbsolute :: Fractional a => ThermodynamicTemperature a -> a
toDegreeCelsiusAbsolute x = (x - 273.15 *~ degreeCelsius) /~ degreeCelsius

{- $health

The last units from Table 3 are SI derived units with special names and symbols admitted for reasons
of safeguarding human health.
-}

becquerel :: Num a => Unit DActivity a
becquerel = siUnit -- second ^ neg1
gray :: Num a => Unit DAbsorbedDose a
gray = siUnit -- joule / kilo gram
sievert :: Num a => Unit DDoseEquivalent a
sievert = siUnit -- joule / kilo gram
katal :: Num a => Unit DCatalyticActivity a
katal = siUnit -- mole / second

{- $accepted-units
There are several units that are not strictly part of the SI but
are either permanently or temporarily accepted for use with the SI.
We define the permanently accepted ones in this module.

From Table 6, Units accepted for use with the SI.

We start with time which we grant exclusive rights to 'minute' and
'second'.
-}
minute, hour, day :: Num a => Unit DTime a
minute = prefix 60 second
hour   = prefix 60 minute
day    = prefix 24 hour -- Mean solar day.

{- $arc-units

Since 'minute' and 'second' are already in use for time we use
'arcminute' and 'arcsecond' <#note2 [2]> for plane angle instead.
-}

degree, arcminute, arcsecond :: Floating a => Unit DPlaneAngle a
degree = prefix (Prelude.pi Prelude./ 180) radian
arcminute = prefix (recip 60) degreeOfArc
arcsecond = prefix (recip 60) minuteOfArc

{- $arc-units-alternate
Alternate (longer) forms of the above. In particular 'degreeOfArc'
can be used if there is a percieved need to disambiguate from e.g.
temperature.
-}

degreeOfArc, minuteOfArc, secondOfArc :: Floating a => Unit DPlaneAngle a
degreeOfArc = degree
secondOfArc = arcsecond
minuteOfArc = arcminute

hectare :: Fractional a => Unit DArea a
hectare = square (hecto meter)

litre, liter :: Fractional a => Unit DVolume a
litre = deci meter ^ pos3 -- International English.
liter = litre             -- American English.

tonne, metricTon :: Fractional a => Unit DMass a
tonne     = prefix 1000 (kilo gram) -- Name in original SI text.
metricTon = tonne                   -- American name.

{- $values-obtained-experimentally
We decline to provide here those units - listed in Table 7 - which,
while accepted for use with the SI, have values which are determined experimentally.
For versioning purposes, those units can be found in "Numeric.Units.Dimensional.DK.NonSI".

However, in 2012 the IAU redefined the astronomical unit as a conventional
unit of length directly tied to the meter, with a length of exactly
149,597,870,700 m and the official abbreviation of au <#note3 [3]>. We therefore include it here.
-}

astronomicalUnit :: Num a => Unit DLength a
astronomicalUnit = prefix 149597870700 meter

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
