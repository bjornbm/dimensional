module Numeric.Units.DimensionalSpec where

import Numeric.Units.Dimensional.Prelude
import Test.Hspec
import qualified Prelude as P

spec :: Spec
spec = do
         describe "Arithmetic" $ do
           it "Exponentiation" $ do
             ((9::Double) *~ one) `shouldBe` (3 *~ one) ^ pos2
             ((1::Double) *~ one) `shouldBe` (12.1231 *~ one) ^ zero
             ((0.25::Double) *~ one) `shouldBe` (2 *~ one) ^ neg2
           it "Dimensionless Exponentiation" $ do
             (3 P.** 2::Double) *~ one `shouldBe` (3 *~ one) ** (2 *~ one)

         describe "Show Instance" $ do
           it "Properly Prints Basic Quantities" $ do
             show ((1.0::Double) *~ one) `shouldBe` "1.0"
             show ((2.0::Double) *~ meter) `shouldBe` "2.0 m"
             show ((2.0::Double) *~ (meter / second)) `shouldBe` "2.0 m s^-1"
             show ((2.0::Double) *~ (meter ^ pos2 / second ^ pos2)) `shouldBe` "2.0 m^2 s^-2"

         describe "Ord Instance" $ do
           it "Properly Sorts Quantities" $ do
             compare ((1 :: Integer) *~ one) (3 *~ one) `shouldBe` LT
             compare ((1 :: Double) *~ (kilo meter)) (1 *~ meter) `shouldBe` GT
             compare ((0 :: Double) *~ second) (_0) `shouldBe` EQ

         describe "Enumeration Function nFromTo" $ do
           it "Handles Zero Intermediate Values" $ do
             nFromTo' _1 _6 0 `shouldBe` [_1, _6]
           it "Handles Negative Number of Intermediate Values" $ do
             nFromTo' _1 _6 (-1) `shouldBe` [_1, _6]
           it "Handles Straightforward Cases" $ do
             nFromTo' _1 _3 1 `shouldBe` [_1, _2, _3]
             nFromTo' _1 _6 4 `shouldBe` [_1, _2, _3, _4, _5, _6]
             nFromTo' _0 _6 2 `shouldBe` [_0, _2, _4, _6]
           it "Handles Decreasing Intervals" $ do
             nFromTo' _5 _2 2 `shouldBe` [_5, _4, _3, _2]
             nFromTo' _6 _0 2 `shouldBe` [_6, _4, _2, _0]
           it "Handles Empty Intervals" $ do
             nFromTo' _1 _1 0 `shouldBe` [_1, _1]
             nFromTo' _0 _0 2 `shouldBe` [_0, _0, _0, _0]

nFromTo' :: Dimensionless Double -> Dimensionless Double -> Int -> [Dimensionless Double]
nFromTo' = nFromTo
