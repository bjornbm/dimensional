Numeric.Dimensional.Quantities
Bjorn Buckwalter, bjorn.buckwalter@gmail.com
License: BSD3


= Summary =

This module defines type synonyms for common dimensionalities and
the associated quantity types. Additional dimensionalities and
quantity types will be added on an as-needed basis.

The definitions in this module are grouped so that a type synonym
for the dimensionality is defined first in terms of base dimension
exponents. Then a type synonym for the corresponding quantity type
is defined. If there are several quantity types with the same
dimensionality type synonyms are provided for each quantity type.

> {-# LANGUAGE DataKinds #-}

> {- |
>    Copyright  : Copyright (C) 2006-2012 Bjorn Buckwalter
>    License    : BSD3
>
>    Maintainer : bjorn.buckwalter@gmail.com
>    Stability  : Stable
>    Portability: GHC only?
>
> Please refer to the literate Haskell code for documentation of both API
> and implementation.
> -}

> module Numeric.Units.Dimensional.DK.Quantities where

> import Numeric.Units.Dimensional.DK
>   ( Dimensions (Dim), Quantity, Dimensionless
>   , DOne, DLuminousIntensity, DThermodynamicTemperature
>   , Unit, DLength, (^+) -- Used only for 'square' and 'cubic'.
>   )
> import Numeric.NumType.DK
>   ( Neg3, Neg2, Neg1, Zero, Pos1, Pos2, Pos3, Pos4
>   , pos2, pos3 -- Used only for 'square' and 'cubic'.
>   )


= Quantities from [1] =

The following quantities are all from the NIST publication "Guide
for the Use of the International System of Units (SI)" [1]. Any
chapters, sections or tables referenced are from [1] unless otherwise
specified.

For lack of better organization we provide definitions grouped by
table in [1].


== Table 2 ==

"Examples of SI derived units expressed in terms of SI base units."

> type DArea = Dim Pos2 Zero Zero Zero Zero Zero Zero
> type Area  = Quantity DArea

> type DVolume = Dim Pos3 Zero Zero Zero Zero Zero Zero
> type Volume  = Quantity DVolume

> type DVelocity = Dim Pos1 Zero Neg1 Zero Zero Zero Zero
> type Velocity  = Quantity DVelocity

> type DAcceleration = Dim Pos1 Zero Neg2 Zero Zero Zero Zero
> type Acceleration  = Quantity DAcceleration

> type DWaveNumber = Dim Neg1 Zero Zero Zero Zero Zero Zero
> type WaveNumber  = Quantity DWaveNumber

> type DMassDensity = Dim Neg3 Pos1 Zero Zero Zero Zero Zero
> type MassDensity  = Quantity DMassDensity
> type Density      = MassDensity -- Short name.

> type DSpecificVolume = Dim Pos3 Neg1 Zero Zero Zero Zero Zero
> type SpecificVolume  = Quantity DSpecificVolume

> type DCurrentDensity = Dim Neg2 Zero Zero Pos1 Zero Zero Zero
> type CurrentDensity  = Quantity DCurrentDensity

> type DMagneticFieldStrength = Dim Neg1 Zero Zero Pos1 Zero Zero Zero
> type MagneticFieldStrength  = Quantity DMagneticFieldStrength

> type DAmountOfSubstanceConcentration = Dim Neg3 Zero Zero Zero Zero Pos1 Zero
> type AmountOfSubstanceConcentration  = Quantity DAmountOfSubstanceConcentration
> type Concentration                   = AmountOfSubstanceConcentration -- Short name.

> type DLuminance = Dim Neg2 Zero Zero Zero Zero Zero Pos1
> type Luminance  = Quantity DLuminance

=== Powers of length units ===

It is permissible to express powers of length units by prefixing
'square' and 'cubic' (see section 9.6 "Spelling unit names raised
to powers" of [1]).

> square :: (Num a) => Unit DLength a -> Unit DArea a
> square x = x ^+ pos2
> cubic  :: (Num a) => Unit DLength a -> Unit DVolume a
> cubic  x = x ^+ pos3

These definitions may seem slightly out of place but these is no
obvious place where they should be. Here they are at least close
to the definitions of 'DLength' and 'DVolume'.


== Table 3a ==

"SI derived units with special names and symbols, including the
radian and steradian."

> type DPlaneAngle = DOne
> type PlaneAngle  = Dimensionless

> type DSolidAngle = DOne
> type SolidAngle  = Dimensionless

> type DFrequency = Dim Zero Zero Neg1 Zero Zero Zero Zero
> type Frequency  = Quantity DFrequency

> type DForce = Dim Pos1 Pos1 Neg2 Zero Zero Zero Zero
> type Force  = Quantity DForce

> type DPressure = Dim Neg1 Pos1 Neg2 Zero Zero Zero Zero
> type DStress   = DPressure
> type Pressure  = Quantity DPressure
> type Stress    = Quantity DStress

> type DEnergy         = Dim Pos2 Pos1 Neg2 Zero Zero Zero Zero
> type DWork           = DEnergy
> type DQuantityOfHeat = DEnergy
> type Energy          = Quantity DEnergy
> type Work            = Quantity DWork
> type QuantityOfHeat  = Quantity DQuantityOfHeat

> type DPower       = Dim Pos2 Pos1 Neg3 Zero Zero Zero Zero
> type DRadiantFlux = DPower
> type Power        = Quantity DPower
> type RadiantFlux  = Quantity DRadiantFlux

> type DElectricCharge        = Dim Zero Zero Pos1 Pos1 Zero Zero Zero
> type DQuantityOfElectricity = DElectricCharge
> type ElectricCharge         = Quantity DElectricCharge
> type QuantityOfElectricity  = Quantity DQuantityOfElectricity

> type DElectricPotential   = Dim Pos2 Pos1 Neg3 Neg1 Zero Zero Zero
> type DPotentialDifference = DElectricPotential
> type DElectromotiveForce  = DElectricPotential
> type ElectricPotential    = Quantity DElectricPotential
> type PotentialDifference  = Quantity DPotentialDifference
> type ElectromotiveForce   = Quantity DElectromotiveForce

> type DCapacitance = Dim Neg2 Neg1 Pos4 Pos2 Zero Zero Zero
> type Capacitance  = Quantity DCapacitance

> type DElectricResistance = Dim Pos2 Pos1 Neg3 Neg2 Zero Zero Zero
> type ElectricResistance  = Quantity DElectricResistance

> type DElectricConductance = Dim Neg2 Neg1 Pos3 Pos2 Zero Zero Zero
> type ElectricConductance  = Quantity DElectricConductance

> type DMagneticFlux = Dim Pos2 Pos1 Neg2 Neg1 Zero Zero Zero
> type MagneticFlux  = Quantity DMagneticFlux

> type DMagneticFluxDensity = Dim Zero Pos1 Neg2 Neg1 Zero Zero Zero
> type MagneticFluxDensity  = Quantity DMagneticFluxDensity

> type DInductance = Dim Pos2 Pos1 Neg2 Neg2 Zero Zero Zero
> type Inductance  = Quantity DInductance

> type DLuminousFlux = DLuminousIntensity
> type LuminousFlux  = Quantity DLuminousFlux

> type DIlluminance = Dim Neg2 Zero Zero Zero Zero Zero Pos1
> type Illuminance  = Quantity DIlluminance

> type DCelsiusTemperature = DThermodynamicTemperature
> type CelsiusTemperature  = Quantity DCelsiusTemperature


== Table 3b ==

"SI derived units with special names and symbols admitted for reasons
of safeguarding human health"

> type DActivity = DFrequency -- Activity of a radionuclide.
> type Activity  = Quantity DActivity

> type DAbsorbedDose   = Dim Pos2 Zero Neg2 Zero Zero Zero Zero
> type DSpecificEnergy = DAbsorbedDose
> type DKerma          = DAbsorbedDose
> type AbsorbedDose    = Quantity DAbsorbedDose
> type SpecificEnergy  = Quantity DSpecificEnergy -- Specific energy imparted.
> type Kerma           = Quantity DKerma

> type DDoseEquivalent            = DAbsorbedDose
> type DAmbientDoseEquivalent     = DDoseEquivalent
> type DDirectionalDoseEquivalent = DDoseEquivalent
> type DPersonalDoseEquivalent    = DDoseEquivalent
> type DEquivalentDose            = DDoseEquivalent
> type DoseEquivalent             = Quantity DDoseEquivalent
> type AmbientDoseEquivalent      = DoseEquivalent
> type DirectionalDoseEquivalent  = DoseEquivalent
> type PersonalDoseEquivalent     = DoseEquivalent
> type EquivalentDose             = DoseEquivalent


== Table 4 ==

"Examples of SI derived units expressed with the aid of SI derived
units having special names and symbols."

We use the same grouping as for table 2.

> type DAngularVelocity = DFrequency
> type AngularVelocity  = Quantity DAngularVelocity

> type DAngularAcceleration = Dim Zero Zero Neg2 Zero Zero Zero Zero
> type AngularAcceleration  = Quantity DAngularAcceleration

> type DDynamicViscosity = Dim Neg1 Pos1 Neg1 Zero Zero Zero Zero
> type DynamicViscosity  = Quantity DDynamicViscosity

> type DMomentOfForce = DEnergy
> type MomentOfForce  = Quantity DMomentOfForce

> type DSurfaceTension = Dim Zero Pos1 Neg2 Zero Zero Zero Zero
> type SurfaceTension  = Quantity DSurfaceTension

> type DHeatFluxDensity = Dim Zero Pos1 Neg3 Zero Zero Zero Zero
> type DIrradiance      = DHeatFluxDensity
> type HeatFluxDensity  = Quantity DHeatFluxDensity
> type Irradiance       = Quantity DIrradiance

> type DRadiantIntensity = DPower
> type RadiantIntensity  = Quantity DRadiantIntensity

> type DRadiance = DIrradiance
> type Radiance  = Quantity DRadiance

> type DHeatCapacity = Dim Pos2 Pos1 Neg2 Zero Neg1 Zero Zero
> type DEntropy      = DHeatCapacity
> type HeatCapacity  = Quantity DHeatCapacity
> type Entropy       = Quantity DEntropy

> type DSpecificHeatCapacity = Dim Pos2 Zero Neg2 Zero Neg1 Zero Zero
> type DSpecificEntropy      = DSpecificHeatCapacity
> type SpecificHeatCapacity  = Quantity DSpecificHeatCapacity
> type SpecificEntropy       = Quantity DSpecificEntropy

Specific energy was already defined in table 3b.

> type DThermalConductivity = Dim Pos1 Pos1 Neg3 Zero Neg1 Zero Zero
> type ThermalConductivity  = Quantity DThermalConductivity

> type DEnergyDensity = DPressure
> type EnergyDensity  = Quantity DEnergyDensity

> type DElectricFieldStrength = Dim Pos1 Pos1 Neg3 Neg1 Zero Zero Zero
> type ElectricFieldStrength  = Quantity DElectricFieldStrength

> type DElectricChargeDensity = Dim Neg3 Zero Pos1 Pos1 Zero Zero Zero
> type ElectricChargeDensity  = Quantity DElectricChargeDensity

> type DElectricFluxDensity = Dim Neg2 Zero Pos1 Pos1 Zero Zero Zero
> type ElectricFluxDensity  = Quantity DElectricFluxDensity

> type DPermittivity = Dim Neg3 Neg1 Pos4 Pos2 Zero Zero Zero
> type Permittivity  = Quantity DPermittivity

> type DPermeability = Dim Pos1 Pos1 Neg2 Neg2 Zero Zero Zero
> type Permeability  = Quantity DPermeability

> type DMolarEnergy = Dim Pos2 Pos1 Neg2 Zero Zero Neg1 Zero
> type MolarEnergy  = Quantity DMolarEnergy

> type DMolarEntropy      = Dim Pos2 Pos1 Neg2 Zero Neg1 Neg1 Zero
> type DMolarHeatCapacity = DMolarEntropy
> type MolarEntropy       = Quantity DMolarEntropy
> type MolarHeatCapacity  = Quantity DMolarHeatCapacity

> type DExposure = Dim Zero Neg1 Pos1 Pos1 Zero Zero Zero
> type Exposure  = Quantity DExposure -- Exposure to x and gamma rays.

> type DAbsorbedDoseRate = Dim Pos2 Zero Neg3 Zero Zero Zero Zero
> type AbsorbedDoseRate  = Quantity DAbsorbedDoseRate


= Quantities not defined in [1] =

Here we define additional quantities on an as-needed basis. We also
provide some synonyms that we anticipate will be useful.

> type DImpulse = Dim Pos1 Pos1 Neg1 Zero Zero Zero Zero
> type Impulse  = Quantity DImpulse

> type DMassFlow = Dim Zero Pos1 Neg1 Zero Zero Zero Zero
> type MassFlow  = Quantity DMassFlow

> type DGravitationalParameter = Dim Pos3 Zero Neg2 Zero Zero Zero Zero
> type GravitationalParameter  = Quantity DGravitationalParameter

> type DKinematicViscosity = Dim Pos2 Zero Neg1 Zero Zero Zero Zero
> type KinematicViscosity  = Quantity DKinematicViscosity

For these we don't bother defining new type synonyms for dimensionalities.
Is this rational?

> type Angle             = PlaneAngle -- Abbreviation
> type Thrust            = Force
> type EnergyPerUnitMass = SpecificEnergy


= References =

[1] http://physics.nist.gov/Pubs/SP811/

