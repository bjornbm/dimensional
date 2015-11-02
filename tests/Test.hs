import qualified Numeric.Units.Dimensional.Test
import qualified Numeric.Units.Dimensional.QuantitiesTest
import System.Exit

main :: IO ()
main = do
         Numeric.Units.Dimensional.QuantitiesTest.main
         ok <- Numeric.Units.Dimensional.Test.main
         if ok
           then exitSuccess
           else exitFailure
