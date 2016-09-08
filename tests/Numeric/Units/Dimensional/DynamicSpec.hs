module Numeric.Units.Dimensional.DynamicSpec where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.Dynamic hiding ((*),(/),(^),(*~),(/~), recip)
import qualified Numeric.Units.Dimensional.Dynamic as Dyn
import qualified Prelude as P
import Test.Hspec

spec :: Spec
spec = do
         describe "Dynamic quantity promotion and demotion" $ do
           it "round-trips through AnyQuantity" $ do
             pending
           it "round-trips through DynQuantity" $ do
             pending
           it "round-trips through AnyQuantity then DynQuantity" $ do
             pending
           it "doesn't promote invalid quantities" $ do
             pending
           it "doesn't promote to the wrong dimension" $ do
             pending
           it "properly combines with dynamic units" $ do
             pending
           it "properly eliminates dynamic units" $ do
             pending
           it "doesn't eliminate dynamic units of the wrong dimension" $ do
             pending
         describe "DynQuantity arithmetic" $ do
           context "Num instance" $ do
             it "matches static addition" $ do
               pending
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
