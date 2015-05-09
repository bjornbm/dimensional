-- | Provides both term-level and type-level representations for physical dimensions in 
-- a single import for convenience.
--
-- Presuming that users intend to work primarily with type level dimensions, this module hides
-- arithmetic operators over term level dimensions and aliases for the base term-level dimensions
-- to avoid namespace pollution. These features are available directly from
-- "Numeric.Units.Dimensional.DK.Dimensions.TermLevel" if desired.
module Numeric.Units.Dimensional.DK.Dimensions
(
  module Numeric.Units.Dimensional.DK.Dimensions.TermLevel,
  module Numeric.Units.Dimensional.DK.Dimensions.TypeLevel
)
where

import Numeric.Units.Dimensional.DK.Dimensions.TermLevel hiding ((*), (/), recip, dOne, dLength, dMass, dTime, dElectricCurrent, dThermodynamicTemperature, dAmountOfSubstance, dLuminousIntensity)
import Numeric.Units.Dimensional.DK.Dimensions.TypeLevel