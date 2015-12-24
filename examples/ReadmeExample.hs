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
