module Numeric.Units.Dimensional.DK.QuantitiesTest where

import Numeric.Units.Dimensional.DK.Prelude
import Numeric.Units.Dimensional.DK
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

-- These definitions simply verify that the type synonyms are
-- consistent with the appropriate units from table 3. If the
-- definitions compile the type synonyms are good.

y1 :: PlaneAngle Double
y1 = coerceAngles $ 1 *~ (meter / meter)
y2 :: SolidAngle Double
y2 = coerceAngles $ 1 *~ (meter ^ pos2 / meter ^ pos2)
y3 :: Frequency Double
y3 = 1 *~ (one / second)
y4 :: Force Double
y4 = 1 *~ (meter * kilo gram / second ^ pos2)
y5 :: Pressure Double
y5 = 1 *~ (newton / meter ^ pos2)
y6 :: Energy Double
y6 = 1 *~ (newton * meter)
y7 :: Power Double
y7 = 1 *~ (joule / second)
y8 :: ElectricCharge Double
y8 = 1 *~ (second * ampere)
y9 :: ElectricPotential Double
y9 = 1 *~ (watt / ampere)
y10 :: Capacitance Double
y10 = 1 *~ (coulomb / volt)
y11 :: ElectricResistance Double
y11 = 1 *~ (volt / ampere)
y12 :: ElectricConductance Double
y12 = 1 *~ (ampere / volt)
y13 :: MagneticFlux Double
y13 = 1 *~ (volt * second)
y14 :: MagneticFluxDensity Double
y14 = 1 *~ (weber / meter ^ pos2)
y15 :: Inductance Double
y15 = 1 *~ (weber / ampere)
y16 :: LuminousFlux Double
y16 = 1 *~ (candela * steradian)
y17 :: Illuminance Double
y17 = 1 *~ (lumen / meter ^ pos2)
y18 :: Activity Double
y18 = 1 *~ (one / second)
y19 :: AbsorbedDose Double
y19 = 1 *~ (joule / kilo gram)
y20 :: DoseEquivalent Double
y20 = 1 *~ (joule / kilo gram)
y21 :: CatalyticActivity Double
y21 = 1 *~ (mole / second)

-- Verification of table 4. If the definitions compile the type
-- synonyms are good.

z1 :: AngularVelocity Double
z1 = 1 *~ (radian / second)
z2 :: AngularAcceleration Double
z2 = 1 *~ (radian / second ^ pos2)
z3 :: DynamicViscosity Double
z3 = 1 *~ (pascal * second)
z4 :: MomentOfForce Double
z4 = 1 *~ (newton * meter)
z5 :: SurfaceTension Double
z5 = 1 *~ (newton / meter)
z6 :: HeatFluxDensity Double
z6 = 1 *~ (watt / meter ^ pos2)
z7 :: RadiantIntensity Double
z7 = 1 *~ (watt / steradian)
z8 :: Radiance Double
z8 = 1 *~ (watt / (meter ^ pos2 * steradian))
z9 :: HeatCapacity Double
z9 = 1 *~ (joule / kelvin)
z10 :: SpecificHeatCapacity Double
z10 = 1 *~ (joule / (kilo gram * kelvin))
z11 :: ThermalConductivity Double
z11 = 1 *~ (watt / (meter * kelvin))
z12 :: EnergyDensity Double
z12 = 1 *~ (joule / meter ^ pos3)
z13 :: ElectricFieldStrength Double
z13 = 1 *~ (volt / meter)
z14 :: ElectricChargeDensity Double
z14 = 1 *~ (coulomb / meter ^ pos3)
z15 :: ElectricFluxDensity Double
z15 = 1 *~ (coulomb / meter ^ pos2)
z16 :: Permittivity Double
z16 = 1 *~ (farad / meter)
z17 :: Permeability Double
z17 = 1 *~ (henry / meter)
z18 :: MolarEnergy Double
z18 = 1 *~ (joule / mole)
z19 :: MolarEntropy Double
z19 = 1 *~ (joule / (mole * kelvin))
z20 :: Exposure Double
z20 = 1 *~ (coulomb / kilo gram)
z21 :: AbsorbedDoseRate Double
z21 = 1 *~ (gray / second)

-- Other quantitites.
mu :: GravitationalParameter Double
mu = 398600.4418 *~ (kilo meter ^ pos3 / second ^ pos2)

-- Dummy main function.
main :: IO ()
main = Prelude.putStrLn "If I compiled I'm OK!"
