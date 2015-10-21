{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

{- |
   Copyright  : Copyright (C) 2006-2015 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn@buckwalter.se
   Stability  : Stable
   Portability: GHC only

= Summary

This module defines a variant of "Numeric.Units.Dimensional.DK.Quantities" where plane angles are treated as dimensionless.

Compare, e.g., 'LuminousFlux' with 'Numeric.Units.Dimensional.DK.Quantities.LuminousFlux'

-}
module Numeric.Units.Dimensional.DK.SI.Quantities
(
  module Numeric.Units.Dimensional.DK.Quantities,

  LuminousFlux,  Illuminance,  AngularVelocity,  AngularAcceleration,  RadiantIntensity,  Radiance,  AngularMomentum,  Torque, MomentOfInertia, SolidAngle,
  DLuminousFlux, DIlluminance, DAngularVelocity, DAngularAcceleration, DRadiantIntensity, DRadiance, DAngularMomentum, DTorque, DMomentOfInertia, DSolidAngle
)
where

import Numeric.Units.Dimensional.DK.SI
import qualified Numeric.Units.Dimensional.DK.Quantities as A
import Numeric.Units.Dimensional.DK.Quantities hiding (LuminousFlux,  Illuminance,  AngularVelocity,  AngularAcceleration,  RadiantIntensity,  Radiance,  AngularMomentum,  Torque,  MomentOfInertia,  SolidAngle,
                                                       DLuminousFlux, DIlluminance, DAngularVelocity, DAngularAcceleration, DRadiantIntensity, DRadiance, DAngularMomentum, DTorque, DMomentOfInertia, DSolidAngle)

type DLuminousFlux = ToSIDim A.DLuminousFlux
type LuminousFlux = Quantity DLuminousFlux

type DIlluminance = ToSIDim A.DIlluminance
type Illuminance = Quantity DIlluminance

type DAngularVelocity = ToSIDim A.DAngularVelocity
type AngularVelocity = Quantity DAngularVelocity

type DAngularAcceleration = ToSIDim A.DAngularAcceleration
type AngularAcceleration = Quantity DAngularAcceleration

type DRadiantIntensity = ToSIDim A.DRadiantIntensity
type RadiantIntensity = Quantity DRadiantIntensity

type DRadiance = ToSIDim A.DRadiance
type Radiance = Quantity DRadiance

type DAngularMomentum = ToSIDim A.DAngularMomentum
type AngularMomentum = Quantity DAngularMomentum

type DTorque = ToSIDim A.DTorque
type Torque = Quantity DTorque

type DMomentOfInertia = ToSIDim A.DMomentOfInertia
type MomentOfInertia = Quantity DMomentOfInertia

type DSolidAngle = ToSIDim A.DSolidAngle
type SolidAngle = Quantity DSolidAngle
