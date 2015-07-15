{-# OPTIONS_HADDOCK not-home, show-extensions #-}

{- |
   Copyright  : Copyright (C) 2006-2015 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn@buckwalter.se
   Stability  : Stable
   Portability: GHC only

This module defines physical dimensions expressed in terms of
the SI base dimensions along with plane and solid angles, including arithmetic.

-}
module Numeric.Units.Dimensional.DK.Dimensions.TermLevel
(
  -- * Type
  Dimension'(..),
  -- * Access to Dimension of Dimensional Values
  HasDimension(..),
  -- * Dimension Arithmetic
  (*), (/), (^), recip,
  -- * Synonyms for Base Dimensions
  dOne,
  dLength, dMass, dTime, dElectricCurrent, dThermodynamicTemperature, dAmountOfSubstance, dLuminousIntensity, dPlaneAngle, dSolidAngle,
  -- * Deconstruction
  asList
)
where

import Data.Monoid (Monoid(..))
import Prelude (id, (+), (-), Int, Show, Eq, Ord)
import qualified Prelude as P

-- | A physical dimension, encoded as 9 integers, representing a factorization of the dimension into the
-- 7 SI base dimensions, along with the dimensions of plane and solid angles.
-- By convention they are stored in the same order as 
-- in the 'Numeric.Units.Dimensional.DK.Dimensions.TypeLevel.Dimension' data kind.
data Dimension' = Dim' !Int !Int !Int !Int !Int !Int !Int !Int !Int
  deriving (Show, Eq, Ord)

-- | The monoid of dimensions under multiplication.
instance Monoid Dimension' where
  mempty = dOne
  mappend = (*)

-- | Dimensional values inhabit this class, which allows access to a term-level representation of their dimension.
class HasDimension a where 
  -- | Obtains a term-level representation of a value's dimension.
  dimension :: a -> Dimension'

instance HasDimension Dimension' where
  dimension = id

-- | The dimension of dimensionless values.
dOne :: Dimension'
dOne = Dim' 0 0 0 0 0 0 0 0 0

dLength, dMass, dTime, dElectricCurrent, dThermodynamicTemperature, dAmountOfSubstance, dLuminousIntensity, dPlaneAngle, dSolidAngle :: Dimension'
dLength                   = Dim' 1 0 0 0 0 0 0 0 0
dMass                     = Dim' 0 1 0 0 0 0 0 0 0
dTime                     = Dim' 0 0 1 0 0 0 0 0 0
dElectricCurrent          = Dim' 0 0 0 1 0 0 0 0 0
dThermodynamicTemperature = Dim' 0 0 0 0 1 0 0 0 0
dAmountOfSubstance        = Dim' 0 0 0 0 0 1 0 0 0
dLuminousIntensity        = Dim' 0 0 0 0 0 0 1 0 0
dPlaneAngle               = Dim' 0 0 0 0 0 0 0 1 0
dSolidAngle               = Dim' 0 0 0 0 0 0 0 0 1

{-
We will reuse the operators and function names from the Prelude.
To prevent unpleasant surprises we give operators the same fixity
as the Prelude.
-}

infixr 8  ^
infixl 7  *, /

-- | Forms the product of two dimensions.
(*) :: Dimension' -> Dimension' -> Dimension'
(Dim' l m t i th n j pa sa) * (Dim' l' m' t' i' th' n' j' pa' sa') = Dim' (l + l') (m + m') (t + t') (i + i') (th + th') (n + n') (j + j') (pa + pa') (sa + sa')

-- | Forms the quotient of two dimensions.
(/) :: Dimension' -> Dimension' -> Dimension'
(Dim' l m t i th n j pa sa) / (Dim' l' m' t' i' th' n' j' pa' sa') = Dim' (l - l') (m - m') (t - t') (i - i') (th - th') (n - n') (j - j') (pa - pa') (sa - sa')

-- | Raises a dimension to an integer power.
(^) :: Dimension' -> Int -> Dimension'
(Dim' l m t i th n j pa sa) ^ x = Dim' (x P.* l) (x P.* m) (x P.* t) (x P.* i) (x P.* th) (x P.* n) (x P.* j) (x P.* pa) (x P.* sa)

-- | Forms the reciprocal of a dimension.
recip :: Dimension' -> Dimension'
recip = (dOne /)

-- | Converts a dimension to a list of 9 integers, representing the exponent associated with each
-- of the 7 SI base dimensions in the standard order, followed by the dimensions of plane and spherical angles.
asList :: Dimension' -> [Int]
asList (Dim' l m t i th n j pa sa) = [l, m, t, i, th, n, j, pa, sa]
