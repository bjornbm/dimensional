Numeric.Dimensional.SIUnits
Bjorn Buckwalter, bjorn.buckwalter@gmail.com
License: BSD3


= Summary =

This module defines the SI prefixes, the SI base units and the SI
derived units. It also defines the units outside of the SI that are
accepted for use with the SI. Any chapters, sections or tables
referenced are from [1] unless otherwise specified.

> {- |
>    Copyright  : Copyright (C) 2006-2012 Bjorn Buckwalter
>    License    : BSD3
>
>    Maintainer : bjorn.buckwalter@gmail.com
>    Stability  : Stable
>    Portability: GHC only?
>
> Please refer to the literate Haskell code for documentation of both API
> and implementation.
> -}

> module Numeric.Units.Dimensional.TF.SIUnits where

> import Numeric.Units.Dimensional.TF
> import Numeric.Units.Dimensional.TF.Quantities
> import Numeric.NumType.TF ( neg1, neg2, pos2, pos3 )
> import Data.Time.Clock (DiffTime)
> import Prelude ( (.), Num, Real (toRational), Fractional (fromRational), Floating, recip )
> import qualified Prelude


= SI prefixes (section 4.4) =

Prefixes are used to form decimal multiples and submultiples of SI
Units as described in section 4.4. We will define the SI prefixes
in terms of the 'prefix' function which applies a scale factor to a
unit.

We define all SI prefixes from Table 5. Multiples first.

> deka, deca, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta
>   :: Num a => Unit d a -> Unit d a
> deka  = prefix 10 -- International English.
> deca  = deka      -- American English.
> hecto = deka . deka
> kilo  = deka . hecto
> mega  = kilo . kilo
> giga  = kilo . mega
> tera  = kilo . giga
> peta  = kilo . tera
> exa   = kilo . peta
> zetta = kilo . exa
> yotta = kilo . zetta

Then the submultiples.

> deci, centi, milli, micro, nano, pico, femto, atto, zepto, yocto
>   :: Fractional a => Unit d a -> Unit d a
> deci  = prefix 0.1
> centi = deci . deci
> milli = deci . centi
> micro = milli . milli
> nano  = milli . micro
> pico  = milli . nano
> femto = milli . pico
> atto  = milli . femto
> zepto = milli . atto
> yocto = milli . zepto

By defining SI prefixes as functions applied to a 'Unit' we satisfy
section 6.2.6 "Unacceptability of stand-alone prefixes".


= SI base units (section 4.1) =

Now we will define the SI base unitsi from section 4.1. To avoid a
myriad of one-letter functions that would doubtlessly cause clashes
and frustration in users' code we spell out all unit names in full,
as we did for prefixes. We also elect to spell the unit names in
singular form, as allowed by section 9.7 "Other spelling conventions".

We define the SI base units in the order of table 1.

> metre, meter :: Num a => Unit DLength a
> metre = Dimensional 1 -- International English.
> meter = metre         -- American English.

For mass the SI base unit is kilogram. For sensible prefixes we
define gram here (see section 6.2.7 "Prefixes and the kilogram").
The drawback is that we are forced to use 'Fractional'.

> gram    :: Fractional a => Unit DMass a
> gram    = Dimensional 1e-3
> second  :: Num a => Unit DTime a
> second  = Dimensional 1
> ampere  :: Num a => Unit DElectricCurrent a
> ampere  = Dimensional 1
> kelvin  :: Num a => Unit DThermodynamicTemperature a
> kelvin  = Dimensional 1
> mole    :: Num a => Unit DAmountOfSubstance a
> mole    = Dimensional 1
> candela :: Num a => Unit DLuminousIntensity a
> candela = Dimensional 1


= DiffTime conversion =

It is not within the scope of this library to handle the complex
task of date and time arithmetic. It is recommended to use the
'Data.Time' library for handling dates and using 'Time' quantities
only when time differences are involved in calculations with other
quantities. In order to convert between the 'DiffTime' data type
in the 'Data.Time' library and 'Time' quantities we provide the
functions 'fromDiffTime' and 'toDiffTime'.

> fromDiffTime :: (Fractional a) => DiffTime -> Time a
> fromDiffTime = (*~ second) . fromRational . toRational
> toDiffTime :: (Real a, Fractional a) => Time a -> DiffTime
> toDiffTime = fromRational . toRational . (/~ second)


= SI derived units (section 4.2) =

Before defining the derived units themselves we provide type synonyms
for derived quantities and their dimensionalities. For lack of better
organization we provide definitions grouped by table in [1].


== Table 3a ==

"SI derived units with special names and symbols, including the
radian and steradian."

> radian :: Fractional a => Unit DPlaneAngle a
> radian = one -- meter * meter ^ neg1
> steradian :: Fractional a => Unit DSolidAngle a
> steradian = one -- meter ^ pos2 * meter ^ neg2
> hertz :: Fractional a => Unit DFrequency a
> hertz = second ^ neg1
> newton :: Fractional a => Unit DForce a
> newton = kilo gram * meter * second ^ neg2
> pascal :: Fractional a => Unit DPressure a
> pascal = newton / meter ^ pos2
> joule :: Fractional a => Unit DEnergy a
> joule = newton * meter
> watt :: Fractional a => Unit DPower a
> watt = joule / second
> coulomb :: Fractional a => Unit DElectricCharge a
> coulomb = second * ampere
> volt :: Fractional a => Unit DElectricPotential a
> volt = watt / ampere
> farad :: Fractional a => Unit DCapacitance a
> farad = coulomb / volt
> ohm :: Fractional a => Unit DElectricResistance a
> ohm = volt / ampere
> siemens :: Fractional a => Unit DElectricConductance a
> siemens = ampere / volt
> weber :: Fractional a => Unit DMagneticFlux a
> weber = volt * second
> tesla :: Fractional a => Unit DMagneticFluxDensity a
> tesla = weber / meter ^ pos2
> henry :: Fractional a => Unit DInductance a
> henry = weber / ampere

We defer the definition of Celcius temperature to the end (would
appear here if we stricly followed table 3a).

> lumen :: Fractional a => Unit DLuminousFlux a
> lumen = candela / steradian
> lux :: Fractional a => Unit DIlluminance a
> lux = lumen / meter ^ pos2

=== Degree Celsius ===

A problematic area is units which increase proportionally to the
base SI units but cross zero at a different point. An example would
be degrees Celsius (see section 4.2.1.1). The author feels that it
is appropriate to define a unit for use with relative quantities
(taking only into account the proportionality) and complement the
unit with functions for converting absolute values.

> degreeCelsius :: Num a => Unit DCelsiusTemperature a
> degreeCelsius = kelvin

The function 'fromDegreeCelsiusAbsolute' should be used in lieu of
"*~ degreeCelsius" when working with absolute temperatures. Similarily,
'toDegreeCelsiusAbsolute' should be used in lieu of "/~ degreeCelsius"
when working with absolute temperatures.

> fromDegreeCelsiusAbsolute :: Fractional a => a -> ThermodynamicTemperature a
> fromDegreeCelsiusAbsolute x = x *~ degreeCelsius + 273.15 *~ degreeCelsius
> toDegreeCelsiusAbsolute :: Fractional a => ThermodynamicTemperature a -> a
> toDegreeCelsiusAbsolute x = (x - 273.15 *~ degreeCelsius) /~ degreeCelsius


== Table 3b ==

"SI derived units with special names and symbols admitted for reasons
of safeguarding human health"

We use the same grouping as for table 3a.

> becquerel :: Fractional a => Unit DActivity a
> becquerel = second ^ neg1

Above we gave a new name to the dimensionality instead of reusing
'Frequency' in the quantity type definition. This will allow GHCi
be more specific when queried for the type of 'becquerel'. For
quantity types without a specific unit we don't bother doing this
(though perhaps we should in case there is a non-SI unit for the
quantity type?).

> gray :: Fractional a => Unit DAbsorbedDose a
> gray = joule / kilo gram
> sievert :: Fractional a => Unit DDoseEquivalent a
> sievert = joule / kilo gram


= Units outside the SI =

There are several units that are not strictly part of the SI but
are either permanently or temporarily accepted for use with the SI.
We define the permanently accepted ones in this module.

== Table 6 ==

"Units accepted for use with the SI."

We start with time which we grant exclusive rights to 'minute' and
'second'.

> minute, hour, day :: Num a => Unit DTime a
> minute = prefix 60 second
> hour   = prefix 60 minute
> day    = prefix 24 hour -- Mean solar day.

Since 'minute' and 'second' are already in use for time we use
'arcminute' and 'arcsecond' [2] for plane angle instead.

> degree, arcminute, arcsecond :: Floating a => Unit DPlaneAngle a
> degree = prefix (Prelude.pi Prelude./ 180) radian
> arcminute = prefix (recip 60) degreeOfArc
> arcsecond = prefix (recip 60) minuteOfArc

Alternate (longer) forms of the above. In particular 'degreeOfArc'
can be used if there is a percieved need to disambiguate from e.g.
temperature.

> degreeOfArc, minuteOfArc, secondOfArc :: Floating a => Unit DPlaneAngle a
> degreeOfArc = degree
> secondOfArc = arcsecond
> minuteOfArc = arcminute

> litre, liter :: Fractional a => Unit DVolume a
> litre = deci meter ^ pos3 -- International English.
> liter = litre             -- American English.

> tonne, metricTon :: Fractional a => Unit DMass a
> tonne     = prefix 1000 (kilo gram) -- Name in original SI text.
> metricTon = tonne                   -- American name.


= References =

[1] http://physics.nist.gov/Pubs/SP811/
[2] http://en.wikipedia.org/wiki/Minute_of_arc

