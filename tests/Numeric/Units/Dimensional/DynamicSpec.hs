module Numeric.Units.Dimensional.DynamicSpec where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.Dynamic hiding ((*),(/),(^),(*~),(/~), recip)
import Numeric.Units.Dimensional.Dimensions.TermLevel (hasSomeDimension)
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
             let meter' = demoteUnit' meter
             (promoteQuantity (139.4 Dyn.*~ meter' :: AnyQuantity Double)) `shouldBe` Just (139.4 *~ meter)
           it "properly eliminates dynamic units" $ do
             let ampere' = demoteUnit' ampere
                 i = demoteQuantity $ 47 *~ ampere :: AnyQuantity Double
             i Dyn./~ ampere' `shouldBe` Just 47
           it "doesn't eliminate dynamic units of the wrong dimension" $ do
             let ampere' = demoteUnit' ampere
                 i = demoteQuantity $ 47 *~ joule :: AnyQuantity Double
             i Dyn./~ ampere' `shouldBe` Nothing
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
               (x1' P.+ x2') `shouldBe` demoteQuantity (x1 + x2)
             it "allows addition with polydimensional zero" $ do
               (t' P.+ polydimensionalZero) `shouldBe` t'
               (polydimensionalZero P.+ t') `shouldBe` t'
               (polydimensionalZero P.+ polydimensionalZero) `shouldBe` (polydimensionalZero :: DynQuantity Double)               
             it "propagates witnesses to zero during addition" $ do
               -- We want to test that the witness for polymorphic zero was actually added to the other addend.
               -- The reason for this property is that if the other addend is some element of the underlying type
               -- which can't act as a divisor (such as a propagating nAn), then we want that information to still
               -- be around when we go to promote the result.
               let nan = 0 P./ 0 :: Double
                   x = demoteQuantity $ nan *~ meter
                   Just y = promoteQuantity (polydimensionalZero P.+ x) :: Maybe (Length Double)
               (y /~ meter) `shouldSatisfy` P.isNaN
             it "matches static subtraction" $ do
               (x2' P.- x1') `shouldBe` demoteQuantity (x2 - x1)
             it "allows subtraction with polydimensional zero" $ do
               (m' P.- polydimensionalZero) `shouldBe` m'
               (polydimensionalZero P.- m') `shouldBe` (P.negate m')
               (polydimensionalZero P.- polydimensionalZero) `shouldBe` (polydimensionalZero :: DynQuantity Double)               
             it "matches static multiplication" $ do
               promoteQuantity (x1' P.* f') `shouldBe` Just (x1 * f)
             it "allows multiplication with polydimensional zero" $ do
               (f' P.* polydimensionalZero) `shouldBe` polydimensionalZero
               (polydimensionalZero P.* m') `shouldBe` polydimensionalZero
               (polydimensionalZero P.* polydimensionalZero) `shouldBe` (polydimensionalZero :: DynQuantity Double)
             it "matches static negation" $ do
               (P.negate m') `shouldBe` demoteQuantity (negate m)
             it "negates polydimensional zero" $ do
               (P.negate polydimensionalZero) `shouldBe` (polydimensionalZero :: DynQuantity Double)
             it "matches static absolute value" $ do
               (P.abs x2') `shouldBe` demoteQuantity (abs x2)
             it "takes absolute value of polydimensional zero" $ do
               (P.abs polydimensionalZero) `shouldBe` (polydimensionalZero :: DynQuantity Double)
             it "matches static signum" $ do
               (P.signum x1') `shouldBe` demoteQuantity (signum x1)
               (P.signum x2') `shouldBe` demoteQuantity (signum x2)
             it "takes signum of polydimensional zero" $ do
               (P.signum polydimensionalZero) `shouldBe` demoteQuantity (_0 :: Dimensionless Double)
             it "implements fromInteger with dimensionless result" $ do
               (P.fromInteger 7 :: DynQuantity Double) `shouldBe` demoteQuantity _7
           context "Fractional instance" $ do
             it "matches static division" $ do
               ((f' P.* x1') P./ t') `shouldBe` demoteQuantity ((f * x1) / t)
             it "matches static reciprocal" $ do
               (P.recip m') `shouldBe` demoteQuantity (recip m)
             it "implements fromRational with dimensionless result" $ do
               let pi' = 22 P./ 7 :: Rational
               (P.fromRational pi' :: DynQuantity Rational) `shouldBe` demoteQuantity (pi' *~ one)
             it "permits polydimensional zero as a dividend" $ do
               (polydimensionalZero P./ m') `shouldBe` polydimensionalZero
             it "propagates witnesses to zero during division" $ do
               -- We want to test that the witness for polymorphic zero was actually divided by the divisor.
               -- The reason for this property is that if the divisor is itself zero (but not polydimensionalZero),
               -- or some other element of the underlying type which can't act as a divisor (such as a propagating nAn),
               -- then we want that information to still be around when we go to promote the result.
               let nan = 0 P./ 0 :: Double
                   x = demoteQuantity $ nan *~ meter
                   y = polydimensionalZero P./ x
                   Just y' = promoteQuantity y :: Maybe (Length Double)
               (y' /~ meter) `shouldSatisfy` P.isNaN
           context "Floating instance" $ do
             it "implements dimensionless pi" $ do
               (P.pi :: DynQuantity Double) `shouldBe` demoteQuantity pi
             it "implements dimensionless sin" $ do
               -- this will serve as a test for all the single-argument dimensionless functions
               (P.sin phi') `shouldBe` demoteQuantity (sin phi)
             it "rejects non-dimensionless arguments to sin" $ do
               (P.sin m') `shouldBe` invalidQuantity
             it "implements dimensionless sin of polydimensional zero" $ do
               (P.sin polydimensionalZero) `shouldBe` (0 :: DynQuantity Double)
             it "matches static square root" $ do
               (P.sqrt a') `shouldBe` demoteQuantity (sqrt a)
             it "rejects arguments to square root with non-square dimensions" $ do
               (P.sqrt f') `shouldNotSatisfy` hasSomeDimension
             it "takes the square root of polydimensional zero" $ do
               (P.sqrt polydimensionalZero) `shouldBe` (polydimensionalZero :: DynQuantity Double)
             it "matches static dimensionless exponentiation" $ do
               (phi' P.** phi') `shouldBe` demoteQuantity (phi ** phi)
             it "rejects non-dimensionless arguments to dimensionless exponentiation" $ do
               (phi' P.** m') `shouldNotSatisfy` hasSomeDimension
               (x1' P.** phi') `shouldNotSatisfy` hasSomeDimension
             it "matches static logBase" $ do
               (P.logBase 10 phi') `shouldBe` demoteQuantity (logBase (10 *~ one) phi)
             it "rejects non-dimensionless arguments to logBase" $ do
               (P.logBase 10 x1') `shouldNotSatisfy` hasSomeDimension
               (P.logBase x1' 10) `shouldNotSatisfy` hasSomeDimension
         describe "Dynamic units" $ do
           describe "Promotion and demotion" $ do
             return ()
           describe "Arithmetic" $ do
             return ()
