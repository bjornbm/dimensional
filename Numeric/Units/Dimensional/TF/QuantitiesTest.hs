module Numeric.Units.Dimensional.TF.QuantitiesTest where

import Numeric.Units.Dimensional.TF.Prelude
import qualified Prelude

-- These definitions simply verify that the type synonyms are
-- consistent with the appropriate units from table 2. If the
-- definitions compile the type synonyms are good.

x1 :: Area Double
x1 = 1 *~ meter ^ pos2
x2 :: Volume Double
x2 = 1 *~ meter ^ pos3
x3 :: Velocity Double
x3 = 1 *~ (meter / second)
x4 :: Acceleration Double
x4 = 1 *~ (meter / second ^ pos2)
x5 :: WaveNumber Double
x5 = 1 *~ meter ^ neg1
x6 :: Density Double
x6 = 1 *~ (kilo gram / meter ^ pos3)
x7 :: SpecificVolume Double
x7 = 1 *~ (meter ^ pos3 / kilo gram)
x8 :: CurrentDensity Double
x8 = 1 *~ (ampere / meter ^ pos2)
x9 :: MagneticFieldStrength Double
x9 = 1 *~ (ampere / meter)
x10 :: Concentration Double
x10 = 1 *~ (mole / meter ^ pos3)
x11 :: Luminance Double
x11 = 1 *~ (candela / meter ^ pos2)

-- Tables 3a and 3b are implicitely tested by the corresponding
-- unit definitions.

-- Verification of table 4. If the definitions compile the type
-- synonyms are good.

y1 :: AngularVelocity Double
y1 = 1 *~ (radian / second)
y2 :: AngularAcceleration Double
y2 = 1 *~ (radian / second ^ pos2)
y3 :: DynamicViscosity Double
y3 = 1 *~ (pascal * second)
y4 :: MomentOfForce Double
y4 = 1 *~ (newton * meter)
y5 :: SurfaceTension Double
y5 = 1 *~ (newton / meter)
y6 :: HeatFluxDensity Double
y6 = 1 *~ (watt / meter ^ pos2)
y7 :: RadiantIntensity Double
y7 = 1 *~ (watt / steradian)
y8 :: Radiance Double
y8 = 1 *~ (watt / (meter ^ pos2 * steradian))
y9 :: HeatCapacity Double
y9 = 1 *~ (joule / kelvin)
y10 :: SpecificHeatCapacity Double
y10 = 1 *~ (joule / (kilo gram * kelvin))
y11 :: ThermalConductivity Double
y11 = 1 *~ (watt / (meter * kelvin))
y12 :: EnergyDensity Double
y12 = 1 *~ (joule / meter ^ pos3)
y13 :: ElectricFieldStrength Double
y13 = 1 *~ (volt / meter)
y14 :: ElectricChargeDensity Double
y14 = 1 *~ (coulomb / meter ^ pos3)
y15 :: ElectricFluxDensity Double
y15 = 1 *~ (coulomb / meter ^ pos2)
y16 :: Permittivity Double
y16 = 1 *~ (farad / meter)
y17 :: Permeability Double
y17 = 1 *~ (henry / meter)
y18 :: MolarEnergy Double
y18 = 1 *~ (joule / mole)
y19 :: MolarEntropy Double
y19 = 1 *~ (joule / (mole * kelvin))
y20 :: Exposure Double
y20 = 1 *~ (coulomb / kilo gram)
y21 :: AbsorbedDoseRate Double
y21 = 1 *~ (gray / second)

-- Other quantitites.
mu :: GravitationalParameter Double
mu = 398600.4418 *~ (kilo meter ^ pos3 / second ^ pos2)

-- Dummy main function.
main = Prelude.putStrLn "If I compiled I'm OK!"

