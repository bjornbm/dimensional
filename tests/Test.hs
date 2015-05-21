import qualified Numeric.Units.Dimensional.DK.Test
import qualified Numeric.Units.Dimensional.DK.QuantitiesTest
import System.Exit

main :: IO ()
main = do
         Numeric.Units.Dimensional.DK.QuantitiesTest.main
         ok <- Numeric.Units.Dimensional.DK.Test.main
         if ok
           then exitSuccess
           else exitFailure
