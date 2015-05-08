{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE DataKinds #-}

{- |
   Copyright  : Copyright (C) 2006-2014 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn@buckwalter.se
   Stability  : Stable
   Portability: GHC only

= Summary

This module defines type synonyms for common dimensionalities and
the associated quantity types. Additional dimensionalities and
quantity types will be added on an as-needed basis.

The definitions in this module are grouped so that a type synonym
for the dimensionality is defined first in terms of base dimension
exponents. Then a type synonym for the corresponding quantity type
is defined. If there are several quantity types with the same
dimensionality type synonyms are provided for each quantity type.

= References

1. #note1# http://physics.nist.gov/Pubs/SP811/

-}

module Numeric.Units.Dimensional.DK.Quantities 
(
  -- * Quantities from the NIST Guide
  -- $nist-guide
  Area, Volume, Velocity, Acceleration, WaveNumber, MassDensity, Density, SpecificVolume, CurrentDensity,
  MagneticFieldStrength, AmountOfSubstanceConcentration, Concentration, Luminance,
  PlaneAngle, SolidAngle, Frequency, Force, Pressure, Stress, Energy, Work, QuantityOfHeat, Power, RadiantFlux,
  ElectricCharge, QuantityOfElectricity, ElectricPotential, PotentialDifference, ElectromotiveForce,
  Capacitance, ElectricResistance, ElectricConductance, MagneticFlux, MagneticFluxDensity,
  Inductance, LuminousFlux, Illuminance, CelsiusTemperature,
  Activity, AbsorbedDose, SpecificEnergy, Kerma, DoseEquivalent, AmbientDoseEquivalent, DirectionalDoseEquivalent, PersonalDoseEquivalent, EquivalentDose,
  AngularVelocity, AngularAcceleration, DynamicViscosity, MomentOfForce, SurfaceTension, HeatFluxDensity,
  Irradiance, RadiantIntensity, Radiance, HeatCapacity, Entropy, SpecificHeatCapacity, SpecificEntropy,
  ThermalConductivity, EnergyDensity, ElectricFieldStrength, ElectricChargeDensity, ElectricFluxDensity, Permittivity, Permeability,
  MolarEnergy, MolarEntropy, MolarHeatCapacity, Exposure, AbsorbedDoseRate,
  -- * Quantities not from the NIST Guide
  -- $not-nist-guide
  Impulse, MassFlow, GravitationalParameter, KinematicViscosity, FirstMassMoment, MomentOfInertia, AngularMomentum,
  ThermalResistivity, ThermalConductance, ThermalResistance, HeatTransferCoefficient, ThermalAdmittance, ThermalInsulance,
  Jerk, Angle, Thrust, Torque, EnergyPerUnitMass,
  -- * Powers of Unit Lengths
  -- $powers-of-unit-lengths
  square, cubic,
  -- * Dimension Aliases
  -- $dimension-aliases
  DArea, DVolume, DVelocity, DAcceleration, DWaveNumber, DMassDensity, DDensity, DSpecificVolume, DCurrentDensity,
  DMagneticFieldStrength, DAmountOfSubstanceConcentration, DConcentration, DLuminance,
  DPlaneAngle, DSolidAngle, DFrequency, DForce, DPressure, DStress, DEnergy, DWork, DQuantityOfHeat, DPower, DRadiantFlux,
  DElectricCharge, DQuantityOfElectricity, DElectricPotential, DPotentialDifference, DElectromotiveForce,
  DCapacitance, DElectricResistance, DElectricConductance, DMagneticFlux, DMagneticFluxDensity,
  DInductance, DLuminousFlux, DIlluminance, DCelsiusTemperature,
  DActivity, DAbsorbedDose, DSpecificEnergy, DKerma, DDoseEquivalent, DAmbientDoseEquivalent, DDirectionalDoseEquivalent, DPersonalDoseEquivalent, DEquivalentDose,
  DAngularVelocity, DAngularAcceleration, DDynamicViscosity, DMomentOfForce, DSurfaceTension, DHeatFluxDensity,
  DIrradiance, DRadiantIntensity, DRadiance, DHeatCapacity, DEntropy, DSpecificHeatCapacity, DSpecificEntropy,
  DThermalConductivity, DEnergyDensity, DElectricFieldStrength, DElectricChargeDensity, DElectricFluxDensity, DPermittivity, DPermeability,
  DMolarEnergy, DMolarEntropy, DMolarHeatCapacity, DExposure, DAbsorbedDoseRate,
  DImpulse, DMassFlow, DGravitationalParameter, DKinematicViscosity, DFirstMassMoment, DMomentOfInertia, DAngularMomentum,
  DThermalResistivity, DThermalConductance, DThermalResistance, DHeatTransferCoefficient, DThermalAdmittance, DThermalInsulance,
  DJerk, DAngle, DThrust, DTorque, DEnergyPerUnitMass
)
where

import Numeric.Units.Dimensional.DK
  ( Dimension (Dim), Quantity, Dimensionless
  , DOne, DLuminousIntensity, DThermodynamicTemperature
  , Unit, DLength, (^)  -- Used only for 'square' and 'cubic'.
  )
import Numeric.NumType.DK
  ( NumType (Neg3, Neg2, Neg1, Zero, Pos1, Pos2, Pos3, Pos4)
  , pos2, pos3  -- Used only for 'square' and 'cubic'.
  )
import Prelude (Fractional)

{- $nist-guide
The following quantities are all from the NIST publication "Guide
for the Use of the International System of Units (SI)" <#note1 [1]>. Any
chapters, sections or tables referenced are from <#note1 [1]> unless otherwise
specified.

For lack of better organization we provide definitions grouped by
table in <#note1 [1]>.

== Table 2

"Examples of SI derived units expressed in terms of SI base units."

-}

{- $dimension-aliases
For each 'Quantity' alias supplied above, we also supply a corresponding 'Dimension' alias.

These dimension aliases may be convenient for supplying type signatures for 'Unit's or for other type-level dimensional programming.
-}

type DArea = 'Dim 'Pos2 'Zero 'Zero 'Zero 'Zero 'Zero 'Zero
type Area  = Quantity DArea

type DVolume = 'Dim 'Pos3 'Zero 'Zero 'Zero 'Zero 'Zero 'Zero
type Volume  = Quantity DVolume

type DVelocity = 'Dim 'Pos1 'Zero 'Neg1 'Zero 'Zero 'Zero 'Zero
type Velocity  = Quantity DVelocity

type DAcceleration = 'Dim 'Pos1 'Zero 'Neg2 'Zero 'Zero 'Zero 'Zero
type Acceleration  = Quantity DAcceleration

type DWaveNumber = 'Dim 'Neg1 'Zero 'Zero 'Zero 'Zero 'Zero 'Zero
type WaveNumber  = Quantity DWaveNumber

type DMassDensity = 'Dim 'Neg3 'Pos1 'Zero 'Zero 'Zero 'Zero 'Zero
type DDensity     = DMassDensity
type MassDensity  = Quantity DMassDensity
type Density      = MassDensity -- Short name.

type DSpecificVolume = 'Dim 'Pos3 'Neg1 'Zero 'Zero 'Zero 'Zero 'Zero
type SpecificVolume  = Quantity DSpecificVolume

type DCurrentDensity = 'Dim 'Neg2 'Zero 'Zero 'Pos1 'Zero 'Zero 'Zero
type CurrentDensity  = Quantity DCurrentDensity

type DMagneticFieldStrength = 'Dim 'Neg1 'Zero 'Zero 'Pos1 'Zero 'Zero 'Zero
type MagneticFieldStrength  = Quantity DMagneticFieldStrength

type DAmountOfSubstanceConcentration = 'Dim 'Neg3 'Zero 'Zero 'Zero 'Zero 'Pos1 'Zero
type DConcentration                  = DAmountOfSubstanceConcentration
type AmountOfSubstanceConcentration  = Quantity DAmountOfSubstanceConcentration
type Concentration                   = AmountOfSubstanceConcentration -- Short name.

type DLuminance = 'Dim 'Neg2 'Zero 'Zero 'Zero 'Zero 'Zero 'Pos1
type Luminance  = Quantity DLuminance


{-

== Table 3a

"SI derived units with special names and symbols, including the
radian and steradian."

-}

type DPlaneAngle = DOne
type PlaneAngle  = Dimensionless

type DSolidAngle = DOne
type SolidAngle  = Dimensionless

type DFrequency = 'Dim 'Zero 'Zero 'Neg1 'Zero 'Zero 'Zero 'Zero
type Frequency  = Quantity DFrequency

type DForce = 'Dim 'Pos1 'Pos1 'Neg2 'Zero 'Zero 'Zero 'Zero
type Force  = Quantity DForce

type DPressure = 'Dim 'Neg1 'Pos1 'Neg2 'Zero 'Zero 'Zero 'Zero
type DStress   = DPressure
type Pressure  = Quantity DPressure
type Stress    = Quantity DStress

type DEnergy         = 'Dim 'Pos2 'Pos1 'Neg2 'Zero 'Zero 'Zero 'Zero
type DWork           = DEnergy
type DQuantityOfHeat = DEnergy
type Energy          = Quantity DEnergy
type Work            = Quantity DWork
type QuantityOfHeat  = Quantity DQuantityOfHeat

type DPower       = 'Dim 'Pos2 'Pos1 'Neg3 'Zero 'Zero 'Zero 'Zero
type DRadiantFlux = DPower
type Power        = Quantity DPower
type RadiantFlux  = Quantity DRadiantFlux

type DElectricCharge        = 'Dim 'Zero 'Zero 'Pos1 'Pos1 'Zero 'Zero 'Zero
type DQuantityOfElectricity = DElectricCharge
type ElectricCharge         = Quantity DElectricCharge
type QuantityOfElectricity  = Quantity DQuantityOfElectricity

type DElectricPotential   = 'Dim 'Pos2 'Pos1 'Neg3 'Neg1 'Zero 'Zero 'Zero
type DPotentialDifference = DElectricPotential
type DElectromotiveForce  = DElectricPotential
type ElectricPotential    = Quantity DElectricPotential
type PotentialDifference  = Quantity DPotentialDifference
type ElectromotiveForce   = Quantity DElectromotiveForce

type DCapacitance = 'Dim 'Neg2 'Neg1 'Pos4 'Pos2 'Zero 'Zero 'Zero
type Capacitance  = Quantity DCapacitance

type DElectricResistance = 'Dim 'Pos2 'Pos1 'Neg3 'Neg2 'Zero 'Zero 'Zero
type ElectricResistance  = Quantity DElectricResistance

type DElectricConductance = 'Dim 'Neg2 'Neg1 'Pos3 'Pos2 'Zero 'Zero 'Zero
type ElectricConductance  = Quantity DElectricConductance

type DMagneticFlux = 'Dim 'Pos2 'Pos1 'Neg2 'Neg1 'Zero 'Zero 'Zero
type MagneticFlux  = Quantity DMagneticFlux

type DMagneticFluxDensity = 'Dim 'Zero 'Pos1 'Neg2 'Neg1 'Zero 'Zero 'Zero
type MagneticFluxDensity  = Quantity DMagneticFluxDensity

type DInductance = 'Dim 'Pos2 'Pos1 'Neg2 'Neg2 'Zero 'Zero 'Zero
type Inductance  = Quantity DInductance

type DLuminousFlux = DLuminousIntensity
type LuminousFlux  = Quantity DLuminousFlux

type DIlluminance = 'Dim 'Neg2 'Zero 'Zero 'Zero 'Zero 'Zero 'Pos1
type Illuminance  = Quantity DIlluminance

type DCelsiusTemperature = DThermodynamicTemperature
type CelsiusTemperature  = Quantity DCelsiusTemperature

{-

== Table 3b

"SI derived units with special names and symbols admitted for reasons
of safeguarding human health"

-}

type DActivity = DFrequency -- Activity of a radionuclide.
type Activity  = Quantity DActivity

type DAbsorbedDose   = 'Dim 'Pos2 'Zero 'Neg2 'Zero 'Zero 'Zero 'Zero
type DSpecificEnergy = DAbsorbedDose
type DKerma          = DAbsorbedDose
type AbsorbedDose    = Quantity DAbsorbedDose
type SpecificEnergy  = Quantity DSpecificEnergy -- Specific energy imparted.
type Kerma           = Quantity DKerma

type DDoseEquivalent            = DAbsorbedDose
type DAmbientDoseEquivalent     = DDoseEquivalent
type DDirectionalDoseEquivalent = DDoseEquivalent
type DPersonalDoseEquivalent    = DDoseEquivalent
type DEquivalentDose            = DDoseEquivalent
type DoseEquivalent             = Quantity DDoseEquivalent
type AmbientDoseEquivalent      = DoseEquivalent
type DirectionalDoseEquivalent  = DoseEquivalent
type PersonalDoseEquivalent     = DoseEquivalent
type EquivalentDose             = DoseEquivalent

{-

== Table 4

"Examples of SI derived units expressed with the aid of SI derived
units having special names and symbols."

We use the same grouping as for table 2.

-}

type DAngularVelocity = DFrequency
type AngularVelocity  = Quantity DAngularVelocity

type DAngularAcceleration = 'Dim 'Zero 'Zero 'Neg2 'Zero 'Zero 'Zero 'Zero
type AngularAcceleration  = Quantity DAngularAcceleration

type DDynamicViscosity = 'Dim 'Neg1 'Pos1 'Neg1 'Zero 'Zero 'Zero 'Zero
type DynamicViscosity  = Quantity DDynamicViscosity

type DMomentOfForce = DEnergy
type MomentOfForce  = Quantity DMomentOfForce

type DSurfaceTension = 'Dim 'Zero 'Pos1 'Neg2 'Zero 'Zero 'Zero 'Zero
type SurfaceTension  = Quantity DSurfaceTension

type DHeatFluxDensity = 'Dim 'Zero 'Pos1 'Neg3 'Zero 'Zero 'Zero 'Zero
type DIrradiance      = DHeatFluxDensity
type HeatFluxDensity  = Quantity DHeatFluxDensity
type Irradiance       = Quantity DIrradiance

type DRadiantIntensity = DPower
type RadiantIntensity  = Quantity DRadiantIntensity

type DRadiance = DIrradiance
type Radiance  = Quantity DRadiance

type DHeatCapacity = 'Dim 'Pos2 'Pos1 'Neg2 'Zero 'Neg1 'Zero 'Zero
type DEntropy      = DHeatCapacity
type HeatCapacity  = Quantity DHeatCapacity
type Entropy       = Quantity DEntropy

type DSpecificHeatCapacity = 'Dim 'Pos2 'Zero 'Neg2 'Zero 'Neg1 'Zero 'Zero
type DSpecificEntropy      = DSpecificHeatCapacity
type SpecificHeatCapacity  = Quantity DSpecificHeatCapacity
type SpecificEntropy       = Quantity DSpecificEntropy

{-

Specific energy was already defined in table 3b.

-}

type DThermalConductivity = 'Dim 'Pos1 'Pos1 'Neg3 'Zero 'Neg1 'Zero 'Zero
type ThermalConductivity  = Quantity DThermalConductivity

type DEnergyDensity = DPressure
type EnergyDensity  = Quantity DEnergyDensity

type DElectricFieldStrength = 'Dim 'Pos1 'Pos1 'Neg3 'Neg1 'Zero 'Zero 'Zero
type ElectricFieldStrength  = Quantity DElectricFieldStrength

type DElectricChargeDensity = 'Dim 'Neg3 'Zero 'Pos1 'Pos1 'Zero 'Zero 'Zero
type ElectricChargeDensity  = Quantity DElectricChargeDensity

type DElectricFluxDensity = 'Dim 'Neg2 'Zero 'Pos1 'Pos1 'Zero 'Zero 'Zero
type ElectricFluxDensity  = Quantity DElectricFluxDensity

type DPermittivity = 'Dim 'Neg3 'Neg1 'Pos4 'Pos2 'Zero 'Zero 'Zero
type Permittivity  = Quantity DPermittivity

type DPermeability = 'Dim 'Pos1 'Pos1 'Neg2 'Neg2 'Zero 'Zero 'Zero
type Permeability  = Quantity DPermeability

type DMolarEnergy = 'Dim 'Pos2 'Pos1 'Neg2 'Zero 'Zero 'Neg1 'Zero
type MolarEnergy  = Quantity DMolarEnergy

type DMolarEntropy      = 'Dim 'Pos2 'Pos1 'Neg2 'Zero 'Neg1 'Neg1 'Zero
type DMolarHeatCapacity = DMolarEntropy
type MolarEntropy       = Quantity DMolarEntropy
type MolarHeatCapacity  = Quantity DMolarHeatCapacity

type DExposure = 'Dim 'Zero 'Neg1 'Pos1 'Pos1 'Zero 'Zero 'Zero
type Exposure  = Quantity DExposure -- Exposure to x and gamma rays.

type DAbsorbedDoseRate = 'Dim 'Pos2 'Zero 'Neg3 'Zero 'Zero 'Zero 'Zero
type AbsorbedDoseRate  = Quantity DAbsorbedDoseRate

{- $not-nist-guide
Here we define additional quantities on an as-needed basis. We also
provide some synonyms that we anticipate will be useful.
-}

type DImpulse = 'Dim 'Pos1 'Pos1 'Neg1 'Zero 'Zero 'Zero 'Zero
type Impulse  = Quantity DImpulse

type DMassFlow = 'Dim 'Zero 'Pos1 'Neg1 'Zero 'Zero 'Zero 'Zero
type MassFlow  = Quantity DMassFlow

type DGravitationalParameter = 'Dim 'Pos3 'Zero 'Neg2 'Zero 'Zero 'Zero 'Zero
type GravitationalParameter  = Quantity DGravitationalParameter

type DKinematicViscosity = 'Dim 'Pos2 'Zero 'Neg1 'Zero 'Zero 'Zero 'Zero
type KinematicViscosity  = Quantity DKinematicViscosity

type DFirstMassMoment = 'Dim 'Pos1 'Pos1 'Zero 'Zero 'Zero 'Zero 'Zero
type FirstMassMoment = Quantity DFirstMassMoment

type DMomentOfInertia = 'Dim 'Pos2 'Pos1 'Zero 'Zero 'Zero 'Zero 'Zero
type MomentOfInertia = Quantity DMomentOfInertia

type DAngularMomentum = 'Dim 'Pos2 'Pos1 'Neg1 'Zero 'Zero 'Zero 'Zero
type AngularMomentum = Quantity DAngularMomentum

{-

The reciprocal of thermal conductivity.

-}

type DThermalResistivity = 'Dim 'Neg1 'Neg1 'Pos3 'Zero 'Pos1 'Zero 'Zero
type ThermalResistivity = Quantity DThermalResistivity

{-

Thermal conductance and resistance quantities after http://en.wikipedia.org/wiki/Thermal_conductivity#Definitions.

-}

type DThermalConductance = 'Dim 'Pos2 'Pos1 'Neg3 'Zero 'Neg1 'Zero 'Zero
type ThermalConductance = Quantity DThermalConductance

type DThermalResistance = 'Dim 'Neg2 'Neg1 'Pos3 'Zero 'Pos1 'Zero 'Zero
type ThermalResistance = Quantity DThermalResistance

type DHeatTransferCoefficient = 'Dim 'Zero 'Pos1 'Neg3 'Zero 'Neg1 'Zero 'Zero
type HeatTransferCoefficient = Quantity DHeatTransferCoefficient

type DThermalAdmittance = DHeatTransferCoefficient
type ThermalAdmittance = HeatTransferCoefficient

type DThermalInsulance = 'Dim 'Zero 'Neg1 'Pos3 'Zero 'Pos1 'Zero 'Zero
type ThermalInsulance = Quantity DThermalInsulance

type DJerk = 'Dim 'Pos1 'Zero 'Neg3 'Zero 'Zero 'Zero 'Zero
type Jerk = Quantity DJerk

type Angle = PlaneAngle -- Abbreviation
type DAngle = DPlaneAngle -- Abbreviation

type Thrust = Force
type DThrust = DForce

type Torque = MomentOfForce
type DTorque = DMomentOfForce

type EnergyPerUnitMass = SpecificEnergy
type DEnergyPerUnitMass = DSpecificEnergy

{- $powers-of-length-units
It is permissible to express powers of length units by prefixing
'square' and 'cubic' (see section 9.6 "Spelling unit names raised
to powers" of <#note1 [1]>).

These definitions may seem slightly out of place but these is no
obvious place where they should be. Here they are at least close
to the definitions of 'DLength' and 'DVolume'.
-}

-- | Constructs a unit of area from a unit of length, taking the area of a square whose sides are that length.
square :: Fractional a => Unit DLength a -> Unit DArea a
square x = x ^ pos2

-- | Constructs a unit of volume from a unit of length, taking the volume of a cube whose sides are that length.
cubic  :: Fractional a => Unit DLength a -> Unit DVolume a
cubic  x = x ^ pos3
