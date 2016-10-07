module Numeric.Units.DimensionalSpec where

import Numeric.Units.Dimensional.Prelude
import Test.Hspec
import qualified Prelude as P

spec :: Spec
spec = do
         describe "Exponentiation operators" $ do
           it "correctly exponentiate quantities with integer exponents" $ do
             ((9::Double) *~ one) `shouldBe` (3 *~ one) ^ pos2
             ((1::Double) *~ one) `shouldBe` (12.1231 *~ one) ^ zero
             ((0.25::Double) *~ one) `shouldBe` (2 *~ one) ^ neg2
           it "correctly exponentiate dimensionless quantities with floating point exponents" $ do
             (3 P.** 2::Double) *~ one `shouldBe` (3 *~ one) ** (2 *~ one)
             (3 P.** (-2.231)::Double) *~ one `shouldBe` (3 *~ one) ** ((-2.231) *~ one)

         describe "Show instance" $ do
           it "properly prints basic quantities" $ do
             show ((1.0::Double) *~ one) `shouldBe` "1.0"
             show ((2.0::Double) *~ meter) `shouldBe` "2.0 m"
             show ((2.0::Double) *~ (meter / second)) `shouldBe` "2.0 m s^-1"
             show ((2.0::Double) *~ (meter ^ pos2 / second ^ pos2)) `shouldBe` "2.0 m^2 s^-2"

         describe "Ord instance" $ do
           it "properly sorts quantities" $ do
             compare ((1 :: Integer) *~ one) (3 *~ one) `shouldBe` LT
             compare ((1 :: Double) *~ (kilo meter)) (1 *~ meter) `shouldBe` GT
             compare ((0 :: Double) *~ second) (_0) `shouldBe` EQ

         describe "Enumeration function nFromTo" $ do
           it "handles zero intermediate values" $ do
             nFromTo' _1 _6 0 `shouldBe` [_1, _6]
           it "handles negative number of intermediate values" $ do
             nFromTo' _1 _6 (-1) `shouldBe` [_1, _6]
           it "handles straightforward cases" $ do
             nFromTo' _1 _3 1 `shouldBe` [_1, _2, _3]
             nFromTo' _1 _6 4 `shouldBe` [_1, _2, _3, _4, _5, _6]
             nFromTo' _0 _6 2 `shouldBe` [_0, _2, _4, _6]
           it "handles decreasing intervals" $ do
             nFromTo' _5 _2 2 `shouldBe` [_5, _4, _3, _2]
             nFromTo' _6 _0 2 `shouldBe` [_6, _4, _2, _0]
           it "handles empty intervals" $ do
             nFromTo' _1 _1 0 `shouldBe` [_1, _1]
             nFromTo' _0 _0 2 `shouldBe` [_0, _0, _0, _0]

nFromTo' :: Dimensionless Double -> Dimensionless Double -> Int -> [Dimensionless Double]
nFromTo' = nFromTo
