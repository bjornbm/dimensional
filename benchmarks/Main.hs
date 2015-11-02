{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Criterion.Main
import Numeric.Units.Dimensional.Prelude
import qualified Prelude as P

main :: IO ()
main = defaultMain [
         bench "RawArithmetic" $ nf rawArithmetic 1000
       , bench "Arithmetic" $ nf arithmetic 1000
       ]

rawArithmetic :: Int -> [Double]
rawArithmetic n = fmap (P./ 3.7) $ [1.0 .. fromIntegral n]

arithmetic :: Int -> [Density Double]
arithmetic n = fmap (/ (3.7 *~ cubic meter)) $ [1.0 .. fromIntegral n] *~~ kilo gram
