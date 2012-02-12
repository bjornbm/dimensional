{-# LANGUAGE NoMonomorphismRestriction #-}

module Numeric.Units.Dimensional.TF.Test where

import Numeric.Units.Dimensional.TF.Prelude
import qualified Prelude
import Test.HUnit

testPower = TestLabel "Power test" $ TestList
    [ TestCase $ (9 *~ one) @=? (3 *~ one) ^ pos2
    , TestCase $ (1 *~ one) @=? (12.1231 *~ one) ^ zero
    , TestCase $ (0.25 *~ one) @=? (2 *~ one) ^ neg2
    ]

testDimensionless = TestLabel "Dimensionless test" $ TestList
    [ TestCase $ (3 Prelude.** 2) *~ one @=? (3 *~ one) ** (2 *~ one)
    ]

testShow = TestLabel "Test 'Show' instance" $ TestList
    [ TestCase $ show (1 *~ one) @?= "1"
    , TestCase $ show (2 *~ meter) @?= "2 m"
    , TestCase $ show (2.0 *~ (meter / second)) @?= "2.0 m s^-1"
    , TestCase $ show (2.0 *~ (meter ^ pos2 / second ^ pos2)) @?= "2.0 m^2 s^-2"
    , TestCase $ show (undefined :: DVelocity) @?= "m s^-1"
    ]

-- Collect the test cases.
tests = TestList
    [ testPower
    , testDimensionless
    , testShow
    ]

main = runTestTT tests

