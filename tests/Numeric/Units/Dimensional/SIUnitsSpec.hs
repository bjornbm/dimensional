module Numeric.Units.Dimensional.SIUnitsSpec where

import Numeric.Units.Dimensional.Prelude
import Test.Hspec

spec :: Spec
spec = do
         describe "Dynamic prefix selection" $ do
           it "selects no prefix when appropriate" $ do
             withAppropriatePrefix meter ((1.3 :: Double) *~ meter) `shouldBe` weaken meter
           it "selects kilo as a prefix when appropriate" $ do
             withAppropriatePrefix newton ((-1742.1 :: Double) *~ newton) `shouldBe` kilo newton
           it "selects yotta as a prefix when appropriate" $ do
             withAppropriatePrefix gram ((875 :: Double) *~ yotta gram) `shouldBe` yotta gram
           it "selects atto as a prefix when appropriate" $ do
             withAppropriatePrefix second ((85.4 :: Double) *~ atto second) `shouldBe` atto second
           it "selects yocto as a prefix when appropriate" $ do
             withAppropriatePrefix watt ((1e-7 :: Double) *~ yocto watt) `shouldBe` yocto watt
