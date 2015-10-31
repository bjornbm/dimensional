dimensional
==============

This library provides statically-checked dimensional arithmetic for physical quantities, using the 7 SI base dimensions.

Data kinds and closed type families provide a flexible, safe, and discoverable implementation that leads to largely self-documenting
client code.

[![Build Status](https://travis-ci.org/bjornbm/dimensional-dk.svg?branch=master)](https://travis-ci.org/bjornbm/dimensional-dk)

Usage
-----

Simply importing `Numeric.Units.Dimensional.Prelude` provides access to dimensional arithmetic opertors, SI units and other common units
accepted for use with the SI, and convenient aliases for quantities with commonly used dimensions.

The `Unit d a` type represents a unit with dimension `d`, whose conversion factor to the coherent SI base unit of the corresponding dimension
is represented by a value of type `a`. `a` is commonly chosen to be `Double`, but can be any `Floating` type. Where possible, support is also
provided for `Fractional` or `Num` values.

Similarly, the `Quantity d a` type represents a quantity with dimension `d`, whose numeric value is of type `a`. Aliases allow the use of, e.g.,
`Length Double` to mean `Quantity DLength Double`. A complete list of available aliases is given in the haddock documentation for the
`Numeric.Units.Dimensional.Quantities`.

```haskell
{-# LANGUAGE NoImplicitPrelude #-}

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.NonSI (gee)

radiusOfEarth :: Length Double
radiusOfEarth = 6371 *~ kilo meter

massOfEarth :: Mass Double
massOfEarth = 5.97e24 *~ kilo gram

g :: GravitationalParameter Double
g = 6.67384e-11 *~ (meter^pos3 * (kilo gram)^neg1 * second^neg2)

gravitationalFieldStrength :: Mass a -> Length a -> Acceleration a
gravitationalFieldStrength m r = g * m / r^pos2

approximateAccelerationDueToGravityOnEarth = gravitationalFieldStrength massOfEarth radiusOfEarth

differenceFromStandardValue = approximateAccelerationDueToGravityOnEarth /~ gee
```

Contributing
------------

For project information (issues, updates, wiki, examples) see:
  https://github.com/bjornbm/dimensional-dk
