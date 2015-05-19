import qualified Numeric.Units.Dimensional.DK.Test
import qualified Numeric.Units.Dimensional.DK.QuantitiesTest

main :: IO ()
main = do
  _ <- Numeric.Units.Dimensional.DK.Test.main
  Numeric.Units.Dimensional.DK.QuantitiesTest.main
