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

This module defines a variant of "Numeric.Units.Dimensional.DK.Quantities" where plane and solid angles are not treated as dimensions.

Compare, e.g., 'LuminousFlux' with 'Numeric.Units.Dimensional.DK.Quantities.LuminousFlux'

-}
module Numeric.Units.Dimensional.DK.SI.Quantities
(
  module Numeric.Units.Dimensional.DK.Quantities,

  LuminousFlux,  Illuminance,  AngularVelocity,  AngularAcceleration,  RadiantIntensity,  Radiance,  AngularMomentum,  Torque,
  DLuminousFlux, DIlluminance, DAngularVelocity, DAngularAcceleration, DRadiantIntensity, DRadiance, DAngularMomentum, DTorque
)
where

import Numeric.Units.Dimensional.DK.SI
import qualified Numeric.Units.Dimensional.DK.Quantities as A
import Numeric.Units.Dimensional.DK.Quantities hiding (LuminousFlux,  Illuminance,  AngularVelocity,  AngularAcceleration,  RadiantIntensity,  Radiance,  AngularMomentum,  Torque,
                                                       DLuminousFlux, DIlluminance, DAngularVelocity, DAngularAcceleration, DRadiantIntensity, DRadiance, DAngularMomentum, DTorque)
import Numeric.NumType.DK.Integers
  ( TypeInt (Zero)
  )

type family RemoveAngles (d :: Dimension) :: Dimension where
  RemoveAngles ('Dim l m t i th n j pa sa) = 'Dim l m t i th n j 'Zero 'Zero

type DLuminousFlux = RemoveAngles A.DLuminousFlux
type LuminousFlux = Quantity DLuminousFlux

type DIlluminance = RemoveAngles A.DIlluminance
type Illuminance = Quantity DIlluminance

type DAngularVelocity = RemoveAngles A.DAngularVelocity
type AngularVelocity = Quantity DAngularVelocity

type DAngularAcceleration = RemoveAngles A.DAngularAcceleration
type AngularAcceleration = Quantity DAngularAcceleration

type DRadiantIntensity = RemoveAngles A.DRadiantIntensity
type RadiantIntensity = Quantity DRadiantIntensity

type DRadiance = RemoveAngles A.DRadiance
type Radiance = Quantity DRadiance

type DAngularMomentum = RemoveAngles A.DAngularMomentum
type AngularMomentum = Quantity DAngularMomentum

type DTorque = RemoveAngles A.DTorque
type Torque = Quantity DTorque
