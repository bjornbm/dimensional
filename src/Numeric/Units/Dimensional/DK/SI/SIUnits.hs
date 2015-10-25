{- |
   Copyright  : Copyright (C) 2006-2015 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn@buckwalter.se
   Stability  : Stable
   Portability: GHC only

= Summary

This module defines a variant of "Numeric.Units.Dimensional.DK.SIUnits" where plane angles are treated as dimensionless.

Compare, e.g., 'radian' with 'Numeric.Units.Dimensional.DK.SIUnits.radian'

-}
module Numeric.Units.Dimensional.DK.SI.SIUnits
(
  module Numeric.Units.Dimensional.DK.SIUnits,
  radian, steradian,
  degree, arcminute, arcsecond, degreeOfArc, minuteOfArc, secondOfArc,
  lumen, lux
)
where

import Numeric.Units.Dimensional.DK.SI
import Numeric.Units.Dimensional.DK.SI.Quantities
import qualified Numeric.Units.Dimensional.DK.SIUnits as A
import Numeric.Units.Dimensional.DK.SIUnits hiding (radian, steradian, degree, arcminute, arcsecond, degreeOfArc, minuteOfArc, secondOfArc, lumen, lux)

radian, steradian :: (Num a) => Unit DPlaneAngle a
radian = removeAngles A.radian
steradian = removeAngles A.steradian

degree, arcminute, arcsecond, degreeOfArc, minuteOfArc, secondOfArc :: (Floating a) => Unit DPlaneAngle a
degree = removeAngles A.degree
arcminute = removeAngles A.arcminute
arcsecond = removeAngles A.arcsecond
degreeOfArc = removeAngles A.degreeOfArc
minuteOfArc = removeAngles A.minuteOfArc
secondOfArc = removeAngles A.secondOfArc

lumen :: Num a => Unit DLuminousFlux a
lumen = removeAngles A.lumen
lux :: Num a => Unit DIlluminance a
lux = removeAngles A.lux
