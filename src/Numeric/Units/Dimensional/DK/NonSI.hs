{- |
   Copyright  : Copyright (C) 2006-2015 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn@buckwalter.se
   Stability  : Stable
   Portability: GHC only

= Summary

This module defines units that are not part of the SI, with the
exception of those defined in the "Numeric.Units.Dimensional.DK.SIUnits" module (units outside
of the SI accepted for use with the SI).

Any chapters, sections or tables referenced are from <#note1 [1]> unless
otherwise specified.

== Neper, bel, shannon and the like

The units of section 5.1.2 are purposefully (but not permanently)
omitted. In fact the logarithmic units (see section 8.7) are
problematic and it is not clear how to implement them. Perhaps with
a conversion function similar to for degrees Celsius.

= References

1. #note1# http://physics.nist.gov/Pubs/SP811/
2. #note2# http://www.iau.org/science/publications/proceedings_rules/units/
3. #note3# http://en.m.wikipedia.org/wiki/Pressure
4. #note4# http://en.m.wikipedia.org/wiki/Torr

-}

module Numeric.Units.Dimensional.DK.NonSI 
(
  -- * Units Defined By Experiment
  -- $values-obtained-experimentally
  electronVolt, unifiedAtomicMassUnit, dalton,
  -- * Standard Gravity
  -- $standard-gravity
  gee,
  -- * Inch-pound Units
  -- $inch-pound-units
  inch, foot, mil, poundMass, ounce, poundForce, slug, psi, yard, mile, nauticalMile, knot,
  revolution, solid, teaspoon, acre,
  -- * Years
  -- $year
  year, century,
  -- * Pressure Units
  -- $pressure-units
  bar, atmosphere, technicalAtmosphere, mmHg, inHg, torr,
  -- * Radiation Units
  rad,
  -- * Kinematic Viscosity
  stokes,
  -- * Temperature
  -- $temperature
  degreeFahrenheit, degreeRankine,
  -- * Imperial Volumes
  -- $imperial-volumes
  imperialGallon, imperialQuart, imperialPint, imperialCup, imperialGill, imperialFluidOunce,
  -- * US Customary Volumes
  -- $us-customary-volumes
  usGallon, usQuart, usPint, usCup, usGill, usFluidOunce
)
where

import Numeric.Units.Dimensional.DK.Prelude
import qualified Prelude

{- $values-obtained-experimentally

From Table 7, units accepted for use with the SI whose values in SI units are
obtained experimentally.

When <#note1 [1]> was published the electron volt had a standard combined
uncertainity of 0.00000049e-19 J and the unified atomic mass unit
had a combined uncertainty of 0.0000010e-27 kg.

-}

electronVolt :: Fractional a => Unit DEnergy a
electronVolt = prefix 1.60217733e-19 joule
unifiedAtomicMassUnit :: Fractional a => Unit DMass a
unifiedAtomicMassUnit = prefix 1.6605402e-27 (kilo gram)
dalton :: Fractional a => Unit DMass a
dalton = unifiedAtomicMassUnit

{- $standard-gravity
In order to relate e.g. pounds mass to pounds force we define the unit
'gee' equal to the standard gravity g_0: the nominal acceleration of a
body in free fall in a vacuum near the surface of the earth (note that
local values of acceleration due to gravity will differ from the standard
gravity). I.e. g_0 = 1 gee.
-}

gee :: Fractional a => Unit DAcceleration a
gee = prefix 9.80665 meter / second ^ pos2

{- $inch-pound-units
Some US customary (that is, inch-pound) units.
-}

inch, foot, mil :: Fractional a => Unit DLength a
inch = prefix 2.54 (centi meter)
foot = prefix 12 inch     -- 0.3048 m
mil  = prefix 0.001 inch
poundMass, ounce :: Fractional a => Unit DMass a
poundMass = prefix 0.45359237 (kilo gram)
ounce     = prefix (1 Prelude./ 16) poundMass

poundForce :: Fractional a => Unit DForce a
poundForce = poundMass * gee  -- 4.4482 N

{-

The slug is an alternative unit of mass defined in terms of the pound-force.

-}

slug :: Fractional a => Unit DMass a
slug = poundForce * (second^pos2) / foot

{-

Pounds of force per square inch.

-}

psi :: Fractional a => Unit DPressure a
psi = poundForce / inch ^ pos2

{-

= Various other (non inch-pound) units =

-}

yard, mile, nauticalMile :: (Fractional a) => Unit DLength a
yard = prefix 3 foot
mile = prefix 1760 yard
nauticalMile = prefix 1852 meter
knot :: (Fractional a) => Unit DVelocity a
knot = nauticalMile / hour
revolution :: (Floating a) => Unit DPlaneAngle a
revolution = prefix (2 Prelude.* Prelude.pi) radian
solid :: (Floating a) => Unit DSolidAngle a
solid = prefix (4 Prelude.* Prelude.pi) steradian
teaspoon :: (Fractional a) => Unit DVolume a
teaspoon = prefix 5 (milli liter)
acre :: (Fractional a) => Unit DArea a
acre = prefix 43560 (square foot)

{- $year

The IAU recommends <#note2 [2]> that:

  Although there are several different kinds of year (as there are
  several kinds of day), it is best to regard a year as a julian
  year of 365.25 days (31.5576 Ms) unless otherwise specified.

This aligns well with my needs so I'm happy to oblige. We define
the year in terms of seconds in order to avoid a 'Fractional'
constraint, and also provide a Julian century.

-}

year, century :: Num a => Unit DTime a
year    = prefix 31557600 second
century = prefix 100 year

{- $pressure-units
It seems that nearly every area of application has its own customary unit for measuring pressure.
We include some of the common ones here. 'psi' was defined earlier.
-}

-- | The bar is exactly 100,000 'Numeric.Units.Dimensional.DK.SIUnits.pascal'.
--
-- From Wikipedia:
--
--  It is about equal to the atmospheric pressure on Earth at sea level.
bar :: (Fractional a) => Unit DPressure a
bar = prefix 1.0e5 pascal

-- | The "standard atmosphere".
--
-- From Wikipedia <#note3 [3]>:
--
--  The standard atmosphere (atm) is an established constant. It is
--  approximately equal to typical air pressure at earth mean sea
--  level.
atmosphere :: (Fractional a) => Unit DPressure a
atmosphere = prefix 101325 pascal

-- | The "technical atmosphere"
--
-- From Wikipedia:
--
--  A technical atmosphere (symbol: at) is a non-SI unit of pressure equal
--  to one kilogram-force per square centimeter.
technicalAtmosphere :: (Fractional a) => Unit DPressure a
technicalAtmosphere = kilo gram * gee * centi meter ^ neg2

-- | The conventional value for the pressure exerted by a 1 mm high column of mercury.
--
-- Per Wikipedia <#note4 [4]>, one mmHg (millimeter of mercury) is defined as:
--
--  The pressure exerted at the base of a column of fluid exactly 1 mm high,
--  when the density of the fluid is exactly 13.5951 g/cm^3, at a place
--  where the acceleration of gravity is exactly 9.80665 m/s^2.

-- The chosen fluid density approximately corresponds to that of mercury
-- at 0 deg. Under most conditions, 1 mmHg is approximately equal to 1 'torr'.
mmHg :: (Fractional a) => Unit DPressure a
mmHg = prefix 13.5951 gram * centi meter ^ neg3 * milli meter * gee


-- | The conventional value for the pressure exerted by a 1 inch high column of mercury.
--
-- Column inches of mercury are also used to measure pressure, especially in
-- meteorological or aeronautical contexts in the United States.
--
-- This is the conventional value from Appendix B.8 of <#note1 [1]>.
inHg :: (Fractional a) => Unit DPressure a
inHg = prefix 3386.389 pascal

-- | One torr (symbol: Torr) is defined as 1/760 atm, which is approximately equal to 1 'mmHg'.
torr :: (Fractional a) => Unit DPressure a
torr = prefix (1 Prelude./ 760) atmosphere

{- Radiation -}
rad :: (Fractional a) => Unit DAbsorbedDose a
rad = centi gray

{- Kinematic Viscosity -}
stokes :: (Fractional a) => Unit DKinematicViscosity a
stokes = centi meter ^ pos2 / second

{- $temperature 
These units of temperature are relative. For absolute temperatures, see 'Numeric.Units.Dimensional.DK.SIUnits.fromDegreeCelsiusAbsolute'.
-}
degreeFahrenheit :: (Fractional a) => Unit DThermodynamicTemperature a
degreeFahrenheit = prefix (5 Prelude./ 9) degreeCelsius

degreeRankine :: (Fractional a) => Unit DThermodynamicTemperature a
degreeRankine = degreeFahrenheit

{- $imperial-volumes
Per http://en.wikipedia.org/wiki/Imperial_units and http://en.wikipedia.org/wiki/Cup_(unit)#Imperial_cup.
-}

imperialGallon, imperialQuart, imperialPint, imperialCup,
                imperialGill, imperialFluidOunce
                :: (Fractional a) => Unit DVolume a
imperialGallon = prefix 4.54609 liter
imperialQuart  = prefix (1 Prelude./ 4) imperialGallon
imperialPint   = prefix (1 Prelude./ 8) imperialGallon
imperialCup    = prefix 0.5 imperialPint
imperialGill   = prefix (1 Prelude./ 4) imperialPint
imperialFluidOunce = prefix (1 Prelude./ 20) imperialPint

{- $us-customary-volumes
Per http://www.nist.gov/pml/wmd/pubs/upload/2012-hb44-final.pdf page 452 and http://en.wikipedia.org/wiki/United_States_customary_units#Fluid_volume
Note that there exist rarely-used "dry" variants of units with overlapping names.
-}

usGallon, usQuart, usPint, usCup, usGill, usFluidOunce :: (Fractional a) => Unit DVolume a
usGallon = prefix 231 (cubic inch)
usQuart = prefix (1 Prelude./ 4) usGallon
usPint = prefix (1 Prelude./ 8) usGallon
usCup = prefix (1 Prelude./ 2) usPint
usGill = prefix (1 Prelude./ 4) usPint
usFluidOunce = prefix (1 Prelude./ 16) usPint -- sic, does not match factor used in imperial system
