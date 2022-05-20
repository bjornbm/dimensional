# dimensional

This library provides statically-checked dimensional arithmetic for physical quantities, using the 7 SI base dimensions.

Data kinds and closed type families provide a flexible, safe, and discoverable implementation that leads to largely self-documenting
client code.

[![Build Status](https://travis-ci.org/bjornbm/dimensional.svg?branch=master)](https://travis-ci.org/bjornbm/dimensional)
[![Hackage Version](https://img.shields.io/hackage/v/dimensional.svg)](https://hackage.haskell.org/package/dimensional)
[![Stackage version](https://www.stackage.org/package/dimensional/badge/lts?label=Stackage)](https://www.stackage.org/package/dimensional)

## Usage

Simply importing `Numeric.Units.Dimensional.Prelude` provides access to dimensional arithmetic opertors, SI units and other common units
accepted for use with the SI, and convenient aliases for quantities with commonly used dimensions.

The `Unit d a` type represents a unit with dimension `d`, whose conversion factor to the coherent SI base unit of the corresponding dimension
is represented by a value of type `a`. `a` is commonly chosen to be `Double`, but can be any `Floating` type. Where possible, support is also
provided for `Fractional` or `Num` values.

Similarly, the `Quantity d a` type represents a quantity with dimension `d`, whose numeric value is of type `a`. Aliases allow the use of, e.g.,
`Length Double` to mean `Quantity DLength Double`. A complete list of available aliases is given in the haddock documentation for the
`Numeric.Units.Dimensional.Quantities`.

In the example below, we will solve a simple word problem.

A car travels at 60 kilometers per hour for one mile, at 50 kph for one mile,
at 40 kph for one mile, and at 30 kph for one mile. How many minutes does the journey take?
What is the average speed of the car? How many seconds does the journey take, rounded up to the next whole second?

```haskell
{-# LANGUAGE NoImplicitPrelude #-}

module ReadmeExample where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.NonSI (mile)

leg :: Length Double
leg = 1 *~ mile -- *~ combines a raw number and a unit to form a quantity

speeds :: [Velocity Double]
speeds = [60, 50, 40, 30] *~~ (kilo meter / hour)
  -- *~~ does the same thing for a whole Functor at once
  -- Parentheses are required around unit expressions that are comingled with *~, /~, *~~, or /~~ operations

timeOfJourney :: Time Double
timeOfJourney = sum $ fmap (leg /) speeds
  -- We can use dimensional versions of ordinary functions like / and sum to combine quantities

averageSpeed :: Velocity Double
averageSpeed = _4 * leg / timeOfJourney
  -- _4 is an alias for the dimensionless number 4

wholeSeconds :: Integer
wholeSeconds = ceiling $ timeOfJourney /~ second
  -- /~ lets us recover a raw number from a quantity and a unit in which it should be expressed

main :: IO ()
main = do
         putStrLn $ "Length of journey is: " ++ showIn minute timeOfJourney
         putStrLn $ "Average speed is: " ++ showIn (mile / hour) averageSpeed
         putStrLn $ "If we don't want to be explicit about units, the show instance uses the SI basis: " ++ show averageSpeed
         putStrLn $ "The journey requires " ++ show wholeSeconds ++ " seconds, rounded up to the nearest second."
```

## Contributing

For project information (issues, updates, wiki, examples) see:
  https://github.com/bjornbm/dimensional
