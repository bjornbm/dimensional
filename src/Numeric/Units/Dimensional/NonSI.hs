{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumDecimals #-}

{- |
   Copyright  : Copyright (C) 2006-2015 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn@buckwalter.se
   Stability  : Stable
   Portability: GHC only

= Summary

This module defines units that are not part of the SI, with the
exception of those defined in the "Numeric.Units.Dimensional.SIUnits" module (units outside
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

module Numeric.Units.Dimensional.NonSI 
(
  -- * Units Defined By Experiment
  -- $values-obtained-experimentally
  electronVolt, unifiedAtomicMassUnit, dalton,
  -- * Standard Gravity
  -- $standard-gravity
  gee,
  -- * Inch-pound Units
  -- $inch-pound-units
  inch, foot, mil, poundMass, ounce, poundForce, horsepower,
  slug, psi, yard, mile, nauticalMile, knot,
  revolution, solid, teaspoon, acre,
  -- * Years
  -- $year
  year, century,
  -- * Pressure Units
  -- $pressure-units
  bar, atmosphere, technicalAtmosphere, mmHg, inHg, inHg_UCUM, inHg_NIST, torr,
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

import Data.ExactPi
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.UnitNames.Internal (ucumMetric, ucum, dimensionalAtom)
import qualified Prelude

{- $values-obtained-experimentally

From Table 7, units accepted for use with the SI whose values in SI units are
obtained experimentally.

When <#note1 [1]> was published the electron volt had a standard combined
uncertainity of 0.00000049e-19 J and the unified atomic mass unit
had a combined uncertainty of 0.0000010e-27 kg.

-}

electronVolt :: Floating a => Unit 'Metric DEnergy a
electronVolt = mkUnitR (ucumMetric "eV" "eV" "electron volt") (Approximate 1.60217733e-19) $ joule
unifiedAtomicMassUnit :: Floating a => Unit 'Metric DMass a
unifiedAtomicMassUnit = mkUnitR (ucumMetric "u" "u" "atomic mass unit") (Approximate 1.6605402e-27) $ kilo gram
dalton :: Floating a => Unit 'Metric DMass a
dalton = mkUnitR (ucumMetric "eV" "Da" "Dalton") 1 $ unifiedAtomicMassUnit

{- $standard-gravity
In order to relate e.g. pounds mass to pounds force we define the unit
'gee' equal to the standard gravity g_0: the nominal acceleration of a
body in free fall in a vacuum near the surface of the earth (note that
local values of acceleration due to gravity will differ from the standard
gravity). I.e. g_0 = 1 gee.
-}

gee :: Fractional a => Unit 'Metric DAcceleration a
gee = mkUnitQ (ucumMetric "[g]" "g" "gee") 9.80665 $ meter / second ^ pos2

{- $inch-pound-units
Some US customary (that is, inch-pound) units.
-}

inch, foot, mil :: Fractional a => Unit 'NonMetric DLength a
inch = mkUnitQ (ucum "[in_i]" "in" "inch") 2.54 $ centi meter
foot = mkUnitQ (ucum "[ft_i]" "ft" "foot") 12 $ inch     -- 0.3048 m
mil  = mkUnitQ (ucum "[mil_i]" "mil" "mil") 0.001 $ inch
poundMass, ounce :: Fractional a => Unit 'NonMetric DMass a
poundMass = mkUnitQ (ucum "[lb_av]" "lb" "pound") 0.45359237 $ kilo gram
ounce     = mkUnitQ (ucum "[oz_av]" "oz" "ounce") (1 Prelude./ 16) $ poundMass

-- | The pound-force is equal to the gravitational force exerted on a mass
-- of one avoirdupois pound on the surface of Earth.
--
-- This definition is based on standard gravity (the 'gee') and the
-- international avoirdupois 'poundMass'.
--
-- See <https://en.wikipedia.org/wiki/Pound_%28force%29 here> for further information.
--
-- >>> 1 *~ poundForce
-- 4.4482216152605 m kg s^-2
--
-- >>> 1 *~ poundForce :: Force Rational
-- 8896443230521 % 2000000000000 m kg s^-2
poundForce :: Fractional a => Unit 'NonMetric DForce a
poundForce = mkUnitQ (ucum "[lbf_av]" "lbf" "pound force") 1 $ poundMass * gee

-- | One mechanical horsepower is by definition the power necessary
-- to apply a force of 550 'poundForce' through a distance of one 'foot'
-- per 'second'.
--
-- See <https://en.wikipedia.org/wiki/Horsepower#Mechanical_horsepower here> for further information.
--
-- >>> 1 *~ horsepower
-- 745.6998715822702 m^2 kg s^-3
--
-- >>> 1 *~ horsepower :: Power Rational
-- 37284993579113511 % 50000000000000 m^2 kg s^-3
horsepower :: Fractional a => Unit 'NonMetric DPower a
horsepower = mkUnitQ (ucum "[HP]" "hp" "horsepower") 550 $ foot * poundForce / second

-- | The slug is a unit of mass associated with Imperial units and United States customary units.
-- It is a mass that accelerates by 1 foot per second per second when a force of one pound is exerted on it.
--
-- This definition is based on standard gravity (the 'gee'), the international 'foot', and the international avoirdupois 'poundMass'.
--
-- See <https://en.wikipedia.org/wiki/Slug_%28mass%29 here> for further information.
--
-- >>> 1 *~ slug
-- 14.593902937206364 kg 
--
-- >>> 1 *~ slug :: Mass Rational
-- 8896443230521 % 609600000000 kg
slug :: Fractional a => Unit 'NonMetric DMass a
slug = mkUnitQ (dimensionalAtom "slug" "slug" "slug") 1 $ poundForce * (second^pos2) / foot

-- | One psi is a pressure of one 'poundForce' per 'square' 'inch' of area.
--
-- See <https://en.wikipedia.org/wiki/Pounds_per_square_inch here> for further information.
--
-- >>> 1 *~ psi
-- 6894.757293168362 m^-1 kg s^-2
--
-- >>> 1 *~ psi :: Pressure Rational
-- 8896443230521 % 1290320000 m^-1 kg s^-2
psi :: Fractional a => Unit 'NonMetric DPressure a
psi = mkUnitQ (ucum "[psi]" "psi" "pound per square inch") 1 $ poundForce / inch ^ pos2

{-

= Various other (non inch-pound) units =

-}

yard, mile :: (Fractional a) => Unit 'NonMetric DLength a
yard = mkUnitQ (ucum "[yd_i]" "yd" "yard") 3 $ foot
mile = mkUnitQ (ucum "[mi_i]" "mi" "mile") 5280 $ foot
nauticalMile :: (Num a) => Unit 'NonMetric DLength a
nauticalMile = mkUnitZ (ucum "[nmi_i]" "NM" "nautical mile") 1852 $ meter
knot :: (Fractional a) => Unit 'NonMetric DVelocity a
knot = mkUnitQ (ucum "[kt_i]" "kt" "knot") 1 $ nauticalMile / hour
revolution :: (Floating a) => Unit 'NonMetric DOne a
revolution = mkUnitR (dimensionalAtom "rev" "rev" "revolution") (2 Prelude.* Prelude.pi) $ radian
solid :: (Floating a) => Unit 'NonMetric DOne a
solid = mkUnitR (dimensionalAtom "solid" "solid" "solid") (4 Prelude.* Prelude.pi) $ steradian
teaspoon :: (Fractional a) => Unit 'NonMetric DVolume a
teaspoon = mkUnitQ (ucum "[tsp_m]" "tsp" "teaspoon") 5 $ milli liter
acre :: (Fractional a) => Unit 'NonMetric DArea a
acre = mkUnitQ (ucum "[acr_us]" "ac" "acre") 43560 $ square foot

{- $year

The IAU recommends <#note2 [2]> that:

  Although there are several different kinds of year (as there are
  several kinds of day), it is best to regard a year as a julian
  year of 365.25 days (31.5576 Ms) unless otherwise specified.

We define the year in terms of seconds in order to avoid a 'Fractional'
constraint, and also provide a Julian century.

-}

year, century :: Num a => Unit 'NonMetric DTime a
year    = mkUnitZ (ucum "a_j" "a" "mean Julian year") 31557600 $ second
century = mkUnitZ (dimensionalAtom "c_j" "cen" "mean Julian century") 100 $ year

{- $pressure-units
It seems that nearly every area of application has its own customary unit for measuring pressure.
We include some of the common ones here. 'psi' was defined earlier.
-}

-- | The bar is exactly 100,000 'Numeric.Units.Dimensional.SIUnits.pascal'.
--
-- From Wikipedia:
--
--  It is about equal to the atmospheric pressure on Earth at sea level.
--
-- >>> 1 *~ bar
-- 100000.0 m^-1 kg s^-2
--
-- >>> 1 *~ bar :: Pressure Rational
-- 100000 % 1 m^-1 kg s^-2
bar :: (Num a) => Unit 'Metric DPressure a
bar = mkUnitZ (ucumMetric "bar" "bar" "bar") 1e5 $ pascal

-- | The "standard atmosphere".
--
-- From Wikipedia <#note3 [3]>:
--
--  The standard atmosphere (atm) is an established constant. It is
--  approximately equal to typical air pressure at earth mean sea
--  level.
--
-- >>> 1 *~ atmosphere
-- 101325.0 m^-1 kg s^-2
--
-- >>> 1 *~ atmosphere :: Pressure Rational
-- 101325 % 1 m^-1 kg s^-2
atmosphere :: (Num a) => Unit 'NonMetric DPressure a
atmosphere = mkUnitZ (ucum "atm" "atm" "standard atmosphere") 101325 $ pascal

-- | The "technical atmosphere"
--
-- From Wikipedia:
--
--  A technical atmosphere (symbol: at) is a non-SI unit of pressure equal
--  to one kilogram-force per square centimeter.
--
-- >>> 1 *~ technicalAtmosphere
-- 98066.5 m^-1 kg s^-2
--
-- >>> 1 *~ technicalAtmosphere :: Pressure Rational
-- 196133 % 2 m^-1 kg s^-2
technicalAtmosphere :: (Fractional a) => Unit 'NonMetric DPressure a
technicalAtmosphere = mkUnitQ (ucum "att" "at" "technical atmosphere") 1 $ kilo gram * gee * centi meter ^ neg2

-- | The conventional value for the pressure exerted by a 1 mm high column of mercury.
--
-- Per Wikipedia <#note4 [4]>, one mmHg (millimeter of mercury) is defined as:
--
--  The pressure exerted at the base of a column of fluid exactly 1 mm high,
--  when the density of the fluid is exactly 13.5951 g/cm^3, at a place
--  where the acceleration of gravity is exactly 9.80665 m/s^2.
--
-- The chosen fluid density approximately corresponds to that of mercury
-- at 0 deg. Under most conditions, 1 mmHg is approximately equal to 1 'torr'.
mmHg :: (Floating a) => Unit 'NonMetric DPressure a
mmHg = milli mHg

mHg :: (Floating a) => Unit 'Metric DPressure a
mHg = mkUnitR (ucumMetric "m[Hg]" "m Hg" "meter of mercury") (Approximate 133.3220) $ kilo pascal

-- | The conventional value for the pressure exerted by a 1 inch high column of mercury.
--
-- Column inches of mercury are also used to measure pressure, especially in
-- meteorological or aeronautical contexts in the United States.
--
-- This is the value defined by UCUM. For the value defined by NIST, see 'inHg_NIST'.
inHg :: (Floating a) => Unit 'NonMetric DPressure a
inHg = inHg_UCUM

-- | The conventional value for the pressure exerted by a 1 inch high column of mercury.
--
-- Column inches of mercury are also used to measure pressure, especially in
-- meteorological or aeronautical contexts in the United States.
--
-- This is the value defined by UCUM. For the value defined by NIST, see 'inHg_NIST'.
inHg_UCUM :: (Floating a) => Unit 'NonMetric DPressure a
inHg_UCUM = mkUnitR (ucum "[in_i'Hg]" "in Hg" "inch of mercury") 1 $ mHg * inch / meter

-- | The conventional value for the pressure exerted by a 1 inch high column of mercury.
--
-- Column inches of mercury are also used to measure pressure, especially in
-- meteorological or aeronautical contexts in the United States.
--
-- This is the value defined by NIST. For the value defined by UCUM, see 'inHg_UCUM'.
inHg_NIST :: (Floating a) => Unit 'NonMetric DPressure a
inHg_NIST = mkUnitR (dimensionalAtom "[in_i'Hg_NIST]" "in Hg" "inch of mercury") 3.386389 $ pascal

-- | One torr (symbol: Torr) is defined as 1/760 atm, which is approximately equal to 1 'mmHg'.
torr :: (Fractional a) => Unit 'NonMetric DPressure a
torr = mkUnitQ (dimensionalAtom "Torr" "Torr" "Torr") (1 Prelude./ 760) $ atmosphere

{- Radiation -}
rad :: (Fractional a) => Unit 'Metric DAbsorbedDose a
rad = mkUnitQ (ucumMetric "RAD" "RAD" "RAD") 1 $ centi gray

{- Kinematic Viscosity -}
stokes :: (Fractional a) => Unit 'Metric DKinematicViscosity a
stokes = mkUnitQ (ucumMetric "St" "St" "Stokes") 1 $ centi meter ^ pos2 / second

{- $temperature 
These units of temperature are relative. For absolute temperatures, see 'Numeric.Units.Dimensional.SIUnits.fromDegreeCelsiusAbsolute'.
-}
degreeFahrenheit :: (Fractional a) => Unit 'NonMetric DThermodynamicTemperature a
degreeFahrenheit = mkUnitQ (ucum "[degF]" "°F" "degree Fahrenheit") (5 Prelude./ 9) $ degreeCelsius

degreeRankine :: (Fractional a) => Unit 'NonMetric DThermodynamicTemperature a
degreeRankine = mkUnitQ (ucum "[degR]" "°R" "degree Rankine") 1 $ degreeFahrenheit

{- $imperial-volumes
Per http://en.wikipedia.org/wiki/Imperial_units and http://en.wikipedia.org/wiki/Cup_(unit)#Imperial_cup.
-}

imperialGallon, imperialQuart, imperialPint, imperialCup,
                imperialGill, imperialFluidOunce
                :: (Fractional a) => Unit 'NonMetric DVolume a
imperialGallon     = mkUnitQ (ucum "[gal_br]" "gal" "gallon")         4.54609          $ liter
imperialQuart      = mkUnitQ (ucum "[qt_br]" "qt" "quart")            (1 Prelude./ 4)  $ imperialGallon
imperialPint       = mkUnitQ (ucum "[pt_br]" "pt" "pint")             (1 Prelude./ 8)  $ imperialGallon
imperialCup        = mkUnitQ (dimensionalAtom "[cup_br]" "cup" "cup") 0.5              $ imperialPint
imperialGill       = mkUnitQ (ucum "[gil_br]" "gill" "gill")          (1 Prelude./ 4)  $ imperialPint
imperialFluidOunce = mkUnitQ (ucum "[foz_br]" "fl oz" "fluid ounce")  (1 Prelude./ 20) $ imperialPint

{- $us-customary-volumes
Per http://www.nist.gov/pml/wmd/pubs/upload/2012-hb44-final.pdf page 452 and http://en.wikipedia.org/wiki/United_States_customary_units#Fluid_volume
Note that there exist rarely-used "dry" variants of units with overlapping names.
-}

usGallon, usQuart, usPint, usCup, usGill, usFluidOunce :: (Fractional a) => Unit 'NonMetric DVolume a
usGallon     = mkUnitQ (ucum "[gal_us]" "gal" "gallon")        231              $ (cubic inch)
usQuart      = mkUnitQ (ucum "[qt_us]" "qt" "quart")           (1 Prelude./ 4)  $ usGallon
usPint       = mkUnitQ (ucum "[pt_us]" "pt" "pint")            (1 Prelude./ 8)  $ usGallon
usCup        = mkUnitQ (ucum "[cup_us]" "cup" "cup")           (1 Prelude./ 2)  $ usPint
usGill       = mkUnitQ (ucum "[gil_us]" "gill" "gill")         (1 Prelude./ 4)  $ usPint
usFluidOunce = mkUnitQ (ucum "[foz_us]" "fl oz" "fluid ounce") (1 Prelude./ 16) $ usPint -- sic, does not match factor used in imperial system
