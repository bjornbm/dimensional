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
           -- declare some static quantities and their dynamic counterparts for arithmetic tests
           let x1 = 12.3 *~ meter
               x2 = (-7.9) *~ meter
               a = 93 *~ square (kilo meter)
               m = 147 *~ kilo gram
               t = 14.9 *~ second
               f = 87.2 *~ milli newton
               phi = 1.61803398875 *~ one
               x1' = demoteQuantity x1 :: DynQuantity Double
               x2' = demoteQuantity x2 :: DynQuantity Double
               a' = demoteQuantity a :: DynQuantity Double
               m' = demoteQuantity m :: DynQuantity Double
               t' = demoteQuantity t :: DynQuantity Double
               f' = demoteQuantity f :: DynQuantity Double
               phi' = demoteQuantity phi :: DynQuantity Double
           context "Num instance" $ do
             it "matches static addition" $ do
               promoteQuantity (x1' P.+ x2') `shouldBe` Just (x1 + x2)
             it "matches static subtraction" $ do
               promoteQuantity (x2' P.- x1') `shouldBe` Just (x2 - x1)
             it "matches static multiplication" $ do
               promoteQuantity (x1' P.* f') `shouldBe` Just (x1 * f)
             it "matches static negation" $ do
               promoteQuantity (P.negate m') `shouldBe` Just (negate m)
             it "matches static absolute value" $ do
               promoteQuantity (P.abs x2') `shouldBe` Just (abs x2)
             it "matches static signum" $ do
               promoteQuantity (P.signum x1') `shouldBe` Just (signum x1)
               promoteQuantity (P.signum x2') `shouldBe` Just (signum x2)
             it "implements fromInteger with dimensionless result" $ do
               promoteQuantity (P.fromInteger 7 :: DynQuantity Double) `shouldBe` Just _7
           context "Fractional instance" $ do
             it "matches static division" $ do
               promoteQuantity ((f' P.* x1') P./ t') `shouldBe` Just ((f * x1) / t)
             it "matches static reciprocal" $ do
               promoteQuantity (P.recip m') `shouldBe` Just (recip m)
             it "implements fromRational with dimensionless result" $ do
               let pi' = 22 P./ 7 :: Rational
               promoteQuantity (P.fromRational pi' :: DynQuantity Rational) `shouldBe` Just (pi' *~ one)
           context "Floating instance" $ do
             it "implements dimensionless pi" $ do
               promoteQuantity (P.pi :: DynQuantity Double) `shouldBe` Just pi
             it "implements dimensionless sin" $ do
               -- this will serve as a test for all the single-argument dimensionless functions
               promoteQuantity (P.sin phi') `shouldBe` Just (sin phi)
             it "rejects non-dimensionless arguments to sin" $ do
               promoteQuantity (P.sin m') `shouldBe` (Nothing :: Maybe (Dimensionless Double))
             it "matches static square root" $ do
               promoteQuantity (P.sqrt a') `shouldBe` Just (sqrt a)
             it "rejects arguments to square root with non-square dimensions" $ do
               dynamicDimension (P.sqrt f') `shouldBe` Nothing
             it "matches static dimensionless exponentiation" $ do
               promoteQuantity (phi' P.** phi') `shouldBe` Just (phi ** phi)
             it "rejects non-dimensionless arguments to dimensionless exponentiation" $ do
               dynamicDimension (phi' P.** m') `shouldBe` Nothing
               dynamicDimension (x1' P.** phi') `shouldBe` Nothing
             it "matches static logBase" $ do
               promoteQuantity (P.logBase 10 phi') `shouldBe` Just (logBase (10 *~ one) phi)
             it "rejects non-dimensionless arguments to logBase" $ do
               dynamicDimension (P.logBase 10 x1') `shouldBe` Nothing
               dynamicDimension (P.logBase x1' 10) `shouldBe` Nothing
         describe "Dynamic units" $ do
           describe "Promotion and demotion" $ do
             return ()
           describe "Arithmetic" $ do
             return ()
