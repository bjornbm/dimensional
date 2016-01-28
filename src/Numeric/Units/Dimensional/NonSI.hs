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
  gee,
  -- * Inch-pound Units
  -- $inch-pound-units
  poundMass, ounce, poundForce, horsepower, btu,
  nauticalMile, knot,
  revolution, solid,
  slug, psi,
  teaspoon,
  -- ** International Foot
  foot, inch, mil, yard, mile, acre,
  -- ** US Survey Foot
  usSurveyFoot, usSurveyInch, usSurveyMil, usSurveyYard, usSurveyMile, usSurveyAcre,
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
electronVolt = mkUnitR (ucumMetric "eV" "eV" "electron volt") 1.60217733e-19 $ joule

unifiedAtomicMassUnit :: Floating a => Unit 'Metric DMass a
unifiedAtomicMassUnit = mkUnitR (ucumMetric "u" "u" "atomic mass unit") 1.6605402e-27 $ kilo gram

dalton :: Floating a => Unit 'Metric DMass a
dalton = mkUnitR (ucumMetric "eV" "Da" "Dalton") 1 $ unifiedAtomicMassUnit

-- | One gee is the standard value of the acceleration due to gravity at the
-- Earth's surface, as standardized by CIPM.
--
-- Note that local values of acceleration due to gravity will differ from the
-- standard gravity.
--
-- See <https://en.wikipedia.org/wiki/Standard_gravity here> for further information.
--
-- >>> 1 *~ gee
-- 9.80665 m s^-2
--
-- >>> 1 *~ gee :: Acceleration Rational
-- 196133 % 20000 m s^-2
gee :: Fractional a => Unit 'Metric DAcceleration a
gee = mkUnitQ (ucumMetric "[g]" "g" "gee") 9.80665 $ meter / second ^ pos2

{- $inch-pound-units
Some US customary (that is, inch-pound) units.
-}

-- | One international foot is one third of an international 'yard'.
--
-- See <https://en.wikipedia.org/wiki/Foot_%28unit%29#International_foot here> for further information.
--
-- >>> 1 *~ foot
-- 0.3048 m
--
-- >>> 1 *~ foot :: Length Rational
-- 381 % 1250 m
foot :: Fractional a => Unit 'NonMetric DLength a
foot = mkUnitQ (ucum "[ft_i]" "ft" "foot") (1 Prelude./ 3) $ yard

-- | One inch is one twelth of a 'foot'.
--
-- This inch is based on the international 'foot'.
--
-- See <https://en.wikipedia.org/wiki/Inch#Modern_standardisation here> for further information.
--
-- >>> 1 *~ inch
-- 2.54e-2 m
--
-- >>> 1 *~ inch :: Length Rational
-- 127 % 5000 m
inch :: Fractional a => Unit 'NonMetric DLength a
inch = mkUnitQ (ucum "[in_i]" "in" "inch") (1 Prelude./ 12) $ foot

-- | One mil is one thousandth of an 'inch'.
--
-- This mil is based on the international 'inch'.
--
-- See <https://en.wikipedia.org/wiki/Thousandth_of_an_inch here> for further information.
--
-- >>> 1 *~ mil
-- 2.54e-5 m
--
-- >>> 1 *~ mil :: Length Rational
-- 127 % 5000000 m
mil :: Fractional a => Unit 'NonMetric DLength a
mil = mkUnitQ (ucum "[mil_i]" "mil" "mil") 0.001 $ inch

-- | One yard, as defined by international agreement in 1959, is precisely
-- 0.9144 'meter'.
--
-- See <https://en.wikipedia.org/wiki/Yard here> for further information.
--
-- >>> 1 *~ yard
-- 0.9144 m
--
-- >>> 1 *~ yard :: Length Rational
-- 1143 % 1250 m
yard :: (Fractional a) => Unit 'NonMetric DLength a
yard = mkUnitQ (ucum "[yd_i]" "yd" "yard") 0.9144 $ meter

-- | One mile is 5,280 feet.
--
-- This mile is based on the international 'foot'.
--
-- See <https://en.wikipedia.org/wiki/Mile#International_mile here> for further information.
--
-- >>> 1 *~ mile
-- 1609.344 m
--
-- >>> 1 *~ mile :: Length Rational
-- 201168 % 125 m
mile :: (Fractional a) => Unit 'NonMetric DLength a
mile = mkUnitQ (ucum "[mi_i]" "mi" "mile") 5280 $ foot

-- | One acre is 43,560 square feet.
--
-- This acre is based on the international 'foot'. For the acre based on the US Survey Foot,
-- see 'usSurveyAcre'. While both acres are in use, the difference between them is of little consequence
-- for most applications in which either is used.
--
-- See <https://en.wikipedia.org/wiki/Acre#Differences_between_international_and_US_survey_acres here> for further information.
--
-- >>> 1 *~ acre
-- 4046.8564224 m^2
--
-- >>> 1 *~ acre :: Area Rational
-- 316160658 % 78125 m^2
acre :: (Fractional a) => Unit 'NonMetric DArea a
acre = mkUnitQ (dimensionalAtom "[acr_i]" "ac" "acre") 43560 $ square foot

-- | One US survey foot is 1200/3937 'meter'.
--
-- For the international foot, see 'foot'. Note that this is not the foot in routine use
-- in the United States.
--
-- See <https://en.wikipedia.org/wiki/Foot_%28unit%29#US_survey_foot here> for further information.
--
-- >>> 1 *~ usSurveyFoot
-- 0.3048006096012192 m
--
-- >>> 1 *~ usSurveyFoot :: Length Rational
-- 1200 % 3937 m
usSurveyFoot :: Fractional a => Unit 'NonMetric DLength a
usSurveyFoot = mkUnitQ (ucum "[ft_us]" "ft" "foot") (1200 Prelude./ 3937) $ meter

-- | One inch is one twelth of a foot.
--
-- This inch is based on the 'usSurveyFoot'. For the inch based on the international foot,
-- see 'inch'. Note that this is not the inch in routine use in the United States.
--
-- See <https://en.wikipedia.org/wiki/Inch here> for further information.
--
-- >>> 1 *~ usSurveyInch
-- 2.54000508001016e-2 m
--
-- >>> 1 *~ usSurveyInch :: Length Rational
-- 100 % 3937 m
usSurveyInch :: Fractional a => Unit 'NonMetric DLength a
usSurveyInch = mkUnitQ (ucum "[in_us]" "in" "inch") (1 Prelude./ 12) $ usSurveyFoot

-- | One mil is one thousandth of an inch.
--
-- This mil is based on the 'usSurveyInch'. For the mil based on the international inch,
-- see 'mil'. Note that this is not the mil in routine use in the United States.
--
-- See <https://en.wikipedia.org/wiki/Thousandth_of_an_inch here> for further information.
--
-- >>> 1 *~ usSurveyMil
-- 2.54000508001016e-5 m
--
-- >>> 1 *~ usSurveyMil :: Length Rational
-- 1 % 39370 m
usSurveyMil :: Fractional a => Unit 'NonMetric DLength a
usSurveyMil = mkUnitQ (ucum "[mil_us]" "mil" "mil") 0.001 $ usSurveyInch

-- | One yard is three feet.
--
-- This yard is based on the 'usSurveyFoot'. For the international yard,
-- see 'yard'. Note that this is not the yard in routine use in the United States.
--
-- See <https://en.wikipedia.org/wiki/Yard here> for further information.
--
-- >>> 1 *~ usSurveyYard
-- 0.9144018288036576 m
--
-- >>> 1 *~ usSurveyYard :: Length Rational
-- 3600 % 3937 m
usSurveyYard :: (Fractional a) => Unit 'NonMetric DLength a
usSurveyYard = mkUnitQ (ucum "[yd_us]" "yd" "yard") 3 $ usSurveyFoot

-- | One US survey mile is 5,280 US survey feet.
--
-- This mile is based on the 'usSurveyFoot'. For the mile based on the international foot,
-- see 'mile'. Note that this is not the mile in routine use in the United States.
--
-- See <https://en.wikipedia.org/wiki/Mile#US_survey_mile here> for further information.
--
-- >>> 1 *~ usSurveyMile
-- 1609.3472186944373 m
--
-- >>> 1 *~ usSurveyMile :: Length Rational
-- 6336000 % 3937 m
usSurveyMile :: (Fractional a) => Unit 'NonMetric DLength a
usSurveyMile = mkUnitQ (ucum "[mi_us]" "mi" "mile") 5280 $ usSurveyFoot

-- | One acre is 43,560 square feet.
--
-- This acre is based on the 'usSurveyFoot'. For the acre based on the international foot,
-- see 'acre'. While both acres are in use, the difference between them is of little consequence
-- for most applications in which either is used. This is the only acre defined by the UCUM.
--
-- See <https://en.wikipedia.org/wiki/Acre#Differences_between_international_and_US_survey_acres here> for further information.
--
-- >>> 1 *~ usSurveyAcre
-- 4046.872609874252 m^2
--
-- >>> 1 *~ usSurveyAcre :: Area Rational
-- 62726400000 % 15499969 m^2
usSurveyAcre :: (Fractional a) => Unit 'NonMetric DArea a
usSurveyAcre = mkUnitQ (ucum "[acr_us]" "ac" "acre") 43560 $ square usSurveyFoot

-- | One avoirdupois pound is a mass, exactly defined in terms of the kilogram by the international
-- yard and pound agreement of 1959.
--
-- See <https://en.wikipedia.org/wiki/Avoirdupois#Internationalization here> for further information.
--
-- >>> 1 *~ poundMass
-- 0.45359237 kg
--
-- >>> 1 *~ poundMass :: Mass Rational
-- 45359237 % 100000000 kg
poundMass :: Fractional a => Unit 'NonMetric DMass a
poundMass = mkUnitQ (ucum "[lb_av]" "lb" "pound") 0.45359237 $ kilo gram

-- | One avoirdupois ounce is one sixteenth of a 'poundMass'.
--
-- See <https://en.wikipedia.org/wiki/Ounce#International_avoirdupois_ounce here> for further information.
--
-- >>> 1 *~ ounce
-- 2.8349523125e-2 kg
--
-- >>> 1 *~ ounce :: Mass Rational
-- 45359237 % 1600000000 kg
ounce :: Fractional a => Unit 'NonMetric DMass a
ounce = mkUnitQ (ucum "[oz_av]" "oz" "ounce") (1 Prelude./ 16) $ poundMass

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

-- | One nautical mile is a unit of length, set by international agreement as being exactly 1,852 meters.
--
-- Historically, it was defined as the distance spanned by one minute of arc along a meridian of the Earth.
--
-- See <https://en.wikipedia.org/wiki/Nautical_mile here> for further information.
--
-- >>> 1 *~ nauticalMile
-- 1852.0 m
--
-- >>> 1 *~ nauticalMile :: Length Rational
-- 1852 % 1 m
nauticalMile :: (Num a) => Unit 'NonMetric DLength a
nauticalMile = mkUnitZ (ucum "[nmi_i]" "NM" "nautical mile") 1852 $ meter

-- | One knot is a velocity equal to one 'nauticalMile' per 'hour'.
--
-- See <https://en.wikipedia.org/wiki/Knot_%28unit%29 here> for further information.
--
-- >>> 1 *~ knot
-- 0.5144444444444445 m s^-1
--
-- >>> 1 *~ knot :: Velocity Rational
-- 463 % 900 m s^-1
knot :: (Fractional a) => Unit 'NonMetric DVelocity a
knot = mkUnitQ (ucum "[kt_i]" "kt" "knot") 1 $ nauticalMile / hour

revolution :: (Floating a) => Unit 'NonMetric DOne a
revolution = mkUnitR (dimensionalAtom "rev" "rev" "revolution") (2 Prelude.* Prelude.pi) $ radian

solid :: (Floating a) => Unit 'NonMetric DOne a
solid = mkUnitR (dimensionalAtom "solid" "solid" "solid") (4 Prelude.* Prelude.pi) $ steradian

teaspoon :: (Fractional a) => Unit 'NonMetric DVolume a
teaspoon = mkUnitQ (ucum "[tsp_m]" "tsp" "teaspoon") 5 $ milli liter

-- | One btu is is the 'QuantityOfHeat' required to raise the temperature
-- of 1 avoirdupois 'poundMass' of liquid water by 1 'degreeFahrenheit' at a constant pressure of one 'atmosphere'.
--
-- Because this value must be determined experimentally and varies with temperature, several standardized
-- values of the btu have arisen. This is the value based on the International Steam Table calorie,
-- defined by the Fifth International Conference on the Properties of Steam.
--
-- See <https://en.wikipedia.org/wiki/British_thermal_unit#Definitions here> for further information.
--
-- >>> 1 *~ btu
-- 1055.05585262 m^2 kg s^-2
--
-- >>> 1 *~ btu :: Energy Rational
-- 52752792631 % 50000000 m^2 kg s^-2
btu :: Fractional a => Unit 'NonMetric DEnergy a
btu = mkUnitQ (ucum "[Btu_IT]" "btu" "British thermal unit") 1055.05585262 $ joule


{- $year

The IAU recommends <#note2 [2]> that:

  Although there are several different kinds of year (as there are
  several kinds of day), it is best to regard a year as a julian
  year of 365.25 days (31.5576 Ms) unless otherwise specified.

-}

-- | One mean Julian year is a unit of measurement of time defined as exactly 365.25 days of 86400 'second's each.
--
-- See <https://en.wikipedia.org/wiki/Julian_year_%28astronomy%29 here> for further information.
--
-- >>> 1 *~ year
-- 3.15576e7 s
--
-- >>> 1 *~ year :: Time Rational
-- 31557600 % 1 s
year :: Num a => Unit 'NonMetric DTime a
year = mkUnitZ (ucum "a_j" "a" "mean Julian year") 31557600 $ second

-- | One mean Julain century is one hundred mean Julian 'year's.
--
-- >>> 1 *~ century
-- 3.15576e9 s
--
-- >>> 1 *~ century :: Time Rational
-- 3155760000 % 1 s
century :: Num a => Unit 'NonMetric DTime a
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
--
-- >>> 1 *~ mmHg
-- 133.322 m^-1 kg s^-2
--
-- >>> 1 *~ mmHg :: Pressure Rational
-- 66661 % 500 m^-1 kg s^-2
mmHg :: (Fractional a) => Unit 'NonMetric DPressure a
mmHg = milli mHg

mHg :: (Fractional a) => Unit 'Metric DPressure a
mHg = mkUnitQ (ucumMetric "m[Hg]" "m Hg" "meter of mercury") 133.3220 $ kilo pascal

-- | The conventional value for the pressure exerted by a 1 inch high column of mercury.
--
-- Column inches of mercury are also used to measure pressure, especially in
-- meteorological or aeronautical contexts in the United States.
--
-- This is the value defined by UCUM. For the value defined by NIST, see 'inHg_NIST'.
--
-- >>> 1 *~ inHg
-- 3386.3788 m^-1 kg s^-2
--
-- >>> 1 *~ inHg :: Pressure Rational
-- 8465947 % 2500 m^-1 kg s^-2
inHg :: (Fractional a) => Unit 'NonMetric DPressure a
inHg = inHg_UCUM

-- | The conventional value for the pressure exerted by a 1 inch high column of mercury.
--
-- Column inches of mercury are also used to measure pressure, especially in
-- meteorological or aeronautical contexts in the United States.
--
-- This is the value defined by UCUM. For the value defined by NIST, see 'inHg_NIST'.
--
-- >>> 1 *~ inHg_UCUM
-- 3386.3788 m^-1 kg s^-2
--
-- >>> 1 *~ inHg_UCUM :: Pressure Rational
-- 8465947 % 2500 m^-1 kg s^-2
inHg_UCUM :: (Fractional a) => Unit 'NonMetric DPressure a
inHg_UCUM = mkUnitQ (ucum "[in_i'Hg]" "in Hg" "inch of mercury") 1 $ mHg * inch / meter

-- | The conventional value for the pressure exerted by a 1 inch high column of mercury.
--
-- Column inches of mercury are also used to measure pressure, especially in
-- meteorological or aeronautical contexts in the United States.
--
-- This is the value defined by NIST. For the value defined by UCUM, see 'inHg_UCUM'.
--
-- >>> 1 *~ inHg_NIST
-- 3386.389 m^-1 kg s^-2
--
-- >>> 1 *~ inHg_NIST :: Pressure Rational
-- 3386389 % 1000 m^-1 kg s^-2
inHg_NIST :: (Fractional a) => Unit 'NonMetric DPressure a
inHg_NIST = mkUnitQ (dimensionalAtom "[in_i'Hg_NIST]" "in Hg" "inch of mercury") 3.386389e3 $ pascal

-- | One torr (symbol: Torr) is defined as 1/760 'atmosphere', which is approximately equal to 1 'mmHg'.
--
-- See <https://en.wikipedia.org/wiki/Torr here> for further information.
--
-- >>> 1 *~ torr
-- 133.32236842105263 m^-1 kg s^-2
--
-- >>> 1 *~ torr :: Pressure Rational
-- 20265 % 152 m^-1 kg s^-2
torr :: (Fractional a) => Unit 'NonMetric DPressure a
torr = mkUnitQ (dimensionalAtom "Torr" "Torr" "Torr") (1 Prelude./ 760) $ atmosphere

{- Radiation -}
rad :: (Fractional a) => Unit 'Metric DAbsorbedDose a
rad = mkUnitQ (ucumMetric "RAD" "RAD" "RAD") 1 $ centi gray

-- | One Stokes is a unit of 'KinematicViscosity' equal to @1 cm^2 / s@.
--
-- See <https://en.wikipedia.org/wiki/Viscosity#Kinematic_viscosity_.CE.BD here> for further information.
--
-- >>> 1 *~ stokes
-- 1.0e-4 m^2 s^-1
--
-- >>> 1 *~ stokes :: KinematicViscosity Rational
-- 1 % 10000 m^2 s^-1
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

imperialGallon :: (Fractional a) => Unit 'NonMetric DVolume a
imperialGallon = mkUnitQ (ucum "[gal_br]" "gal" "gallon") 4.54609 $ liter

imperialQuart :: (Fractional a) => Unit 'NonMetric DVolume a
imperialQuart = mkUnitQ (ucum "[qt_br]" "qt" "quart") (1 Prelude./ 4) $ imperialGallon

imperialPint :: (Fractional a) => Unit 'NonMetric DVolume a
imperialPint = mkUnitQ (ucum "[pt_br]" "pt" "pint") (1 Prelude./ 8) $ imperialGallon

imperialCup :: (Fractional a) => Unit 'NonMetric DVolume a
imperialCup = mkUnitQ (dimensionalAtom "[cup_br]" "cup" "cup") 0.5 $ imperialPint

imperialGill :: (Fractional a) => Unit 'NonMetric DVolume a
imperialGill = mkUnitQ (ucum "[gil_br]" "gill" "gill") (1 Prelude./ 4) $ imperialPint

imperialFluidOunce :: (Fractional a) => Unit 'NonMetric DVolume a
imperialFluidOunce = mkUnitQ (ucum "[foz_br]" "fl oz" "fluid ounce") (1 Prelude./ 20) $ imperialPint

{- $us-customary-volumes
Per http://www.nist.gov/pml/wmd/pubs/upload/2012-hb44-final.pdf page 452 and http://en.wikipedia.org/wiki/United_States_customary_units#Fluid_volume
Note that there exist rarely-used "dry" variants of units with overlapping names.
-}

usGallon :: (Fractional a) => Unit 'NonMetric DVolume a
usGallon = mkUnitQ (ucum "[gal_us]" "gal" "gallon") 231 $ (cubic inch)

usQuart :: (Fractional a) => Unit 'NonMetric DVolume a
usQuart = mkUnitQ (ucum "[qt_us]" "qt" "quart") (1 Prelude./ 4) $ usGallon

usPint :: (Fractional a) => Unit 'NonMetric DVolume a
usPint = mkUnitQ (ucum "[pt_us]" "pt" "pint") (1 Prelude./ 8) $ usGallon

usCup :: (Fractional a) => Unit 'NonMetric DVolume a
usCup = mkUnitQ (ucum "[cup_us]" "cup" "cup") (1 Prelude./ 2) $ usPint

usGill :: (Fractional a) => Unit 'NonMetric DVolume a
usGill = mkUnitQ (ucum "[gil_us]" "gill" "gill") (1 Prelude./ 4) $ usPint

usFluidOunce :: (Fractional a) => Unit 'NonMetric DVolume a
usFluidOunce = mkUnitQ (ucum "[foz_us]" "fl oz" "fluid ounce") (1 Prelude./ 16) $ usPint -- sic, does not match factor used in imperial system
