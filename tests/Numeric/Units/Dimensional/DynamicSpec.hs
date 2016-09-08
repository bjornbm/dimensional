module Numeric.Units.Dimensional.DynamicSpec where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.Dynamic hiding ((*),(/),(^),(*~),(/~), recip)
import qualified Numeric.Units.Dimensional.Dynamic as Dyn
import qualified Prelude as P
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
         describe "Dynamic quantity promotion and demotion" $ do
           it "round-trips through AnyQuantity" $ property $
             \x -> let x' = x *~ kilo newton
                       x'' = demoteQuantity x' :: AnyQuantity Double
                    in Just x' == promoteQuantity x''
           it "round-trips through DynQuantity" $ property $
             \x -> let x' = x *~ micro watt
                       x'' = demoteQuantity x' :: DynQuantity Rational
                    in Just x' == promoteQuantity x''
           it "round-trips through AnyQuantity then DynQuantity" $ property $
             \x -> let x' = x *~ gram
                       x'' = demoteQuantity x' :: AnyQuantity Double
                       x''' = demoteQuantity x'' :: DynQuantity Double
                    in Just x' == promoteQuantity x'''
           it "doesn't promote invalid quantities" $ do
             (promoteQuantity invalidQuantity :: Maybe (Length Double)) `shouldBe` Nothing
           it "doesn't promote AnyQuantity to the wrong dimension" $ do
             let x = 12.3 *~ meter
                 x' = demoteQuantity x :: AnyQuantity Double
             (promoteQuantity x' :: Maybe (Mass Double)) `shouldBe` Nothing
           it "doesn't promote DynQuantity to the wrong dimension" $ do
             let x = 12.3 *~ mole
                 x' = demoteQuantity x :: DynQuantity Double
             (promoteQuantity x' :: Maybe (Time Double)) `shouldBe` Nothing
           it "properly combines with dynamic units" $ do
             pending
           it "properly eliminates dynamic units" $ do
             pending
           it "doesn't eliminate dynamic units of the wrong dimension" $ do
             pending
         describe "DynQuantity arithmetic" $ do
           let x1 = 12.3 *~ meter
               x2 = (-7.9) *~ meter
               m = 147 *~ kilo gram
               t = 14.9 *~ second
               f = 87.2 *~ milli newton
               x1' = demoteQuantity x1 :: DynQuantity Double
               x2' = demoteQuantity x2 :: DynQuantity Double
               m' = demoteQuantity m :: DynQuantity Double
               t' = demoteQuantity t :: DynQuantity Double
               f' = demoteQuantity f :: DynQuantity Double
           context "Num instance" $ do
             it "matches static addition" $ do
               promoteQuantity (x1' P.+ x2') `shouldBe` Just (x1 + x2)
             it "matches static subtraction" $ do
               pending
             it "matches static multiplication" $ do
               pending
             it "matches static negation" $ do
               pending
             it "matches static absolute value" $ do
               pending
             it "implements signum with dimensionless result" $ do
               pending
             it "implements fromInteger with dimensionless result" $ do
               pending
           context "Fractional instance" $ do
             it "matches static division" $ do
               pending
             it "matches static reciprocal" $ do
               pending
             it "implements fromRational with dimensionless result" $ do
               pending
           context "Floating instance" $ do
             it "implements dimensionless pi" $ do
               pending
             it "implements dimensionless sin" $ do
               -- this will serve as a test for all the single-argument dimensionless functions
               pending
             it "rejects non-dimensionless arguments to sin" $ do
               pending
             it "matches static square root" $ do
               pending
             it "rejects arguments to square root with non-square dimensions" $ do
               pending
             it "matches static dimensionless exponentiation" $ do
               pending
             it "rejects non-dimensionless arguments to dimensionless exponentiation" $ do
               pending
             it "matches static logBase" $ do
               pending
             it "rejects non-dimensionless arguments to logBase" $ do
               pending
         describe "Dynamic units" $ do
           describe "Promotion and demotion" $ do
             return ()
           describe "Arithmetic" $ do
             return ()
