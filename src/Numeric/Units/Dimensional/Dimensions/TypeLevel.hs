{-# OPTIONS_HADDOCK not-home, show-extensions #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
   Copyright  : Copyright (C) 2006-2015 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn@buckwalter.se
   Stability  : Stable
   Portability: GHC only

This module defines type-level physical dimensions expressed in terms of
the SI base dimensions using 'Numeric.NumType.DK.NumType' for type-level integers.

Type-level arithmetic, synonyms for the base dimensions, and conversion to the term-level are included.
-}
module Numeric.Units.Dimensional.Dimensions.TypeLevel
(
  -- * Kind of Type-Level Dimensions
  type Dimension(..),
  -- * Dimension Arithmetic
  type (*), type (/), type (^), type Recip, type NRoot, type Sqrt, type Cbrt,
  -- * Synonyms for Base Dimensions
  DOne,
  DLength, DMass, DTime, DElectricCurrent, DThermodynamicTemperature, DAmountOfSubstance, DLuminousIntensity,
  -- * Conversion to Term Level
  type KnownDimension
)
where

import Data.Proxy
import Numeric.NumType.DK.Integers
  ( TypeInt (Zero, Pos1, Pos2, Pos3), (+)(), (-)()
  , KnownTypeInt, toNum
  )
import qualified Numeric.NumType.DK.Integers as N
import Numeric.Units.Dimensional.Dimensions.TermLevel

-- | Represents a physical dimension in the basis of the 7 SI base dimensions,
-- where the respective dimensions are represented by type variables
-- using the following convention:
--
--  * l: Length
--  * m: Mass
--  * t: Time
--  * i: Electric current
--  * th: Thermodynamic temperature
--  * n: Amount of substance
--  * j: Luminous intensity
--
-- For the equivalent term-level representation, see 'Dimension''
data Dimension = Dim TypeInt TypeInt TypeInt TypeInt TypeInt TypeInt TypeInt

-- | The type-level dimension of dimensionless values.
type DOne                      = 'Dim 'Zero 'Zero 'Zero 'Zero 'Zero 'Zero 'Zero
type DLength                   = 'Dim 'Pos1 'Zero 'Zero 'Zero 'Zero 'Zero 'Zero
type DMass                     = 'Dim 'Zero 'Pos1 'Zero 'Zero 'Zero 'Zero 'Zero
type DTime                     = 'Dim 'Zero 'Zero 'Pos1 'Zero 'Zero 'Zero 'Zero
type DElectricCurrent          = 'Dim 'Zero 'Zero 'Zero 'Pos1 'Zero 'Zero 'Zero
type DThermodynamicTemperature = 'Dim 'Zero 'Zero 'Zero 'Zero 'Pos1 'Zero 'Zero
type DAmountOfSubstance        = 'Dim 'Zero 'Zero 'Zero 'Zero 'Zero 'Pos1 'Zero
type DLuminousIntensity        = 'Dim 'Zero 'Zero 'Zero 'Zero 'Zero 'Zero 'Pos1

{-
We will reuse the operators and function names from the Prelude.
To prevent unpleasant surprises we give operators the same fixity
as the Prelude.
-}

infixr 8  ^
infixl 7  *, /

-- | Multiplication of dimensions corresponds to adding of the base
-- dimensions' exponents.
type family (a::Dimension) * (b::Dimension) where
  DOne * d = d
  d * DOne = d
  ('Dim l  m  t  i  th  n  j) * ('Dim l' m' t' i' th' n' j')
    = 'Dim (l + l') (m + m') (t + t') (i + i') (th + th') (n + n') (j + j')

-- | Division of dimensions corresponds to subtraction of the base
-- dimensions' exponents.
type family (a::Dimension) / (d::Dimension) where
  d / DOne = d
  d / d = DOne
  ('Dim l  m  t  i  th  n  j) / ('Dim l' m' t' i' th' n' j')
    = 'Dim (l - l') (m - m') (t - t') (i - i') (th - th') (n - n') (j - j')

-- | The reciprocal of a dimension is defined as the result of dividing 'DOne' by it,
-- or of negating each of the base dimensions' exponents.
type Recip (d :: Dimension) = DOne / d

-- | Powers of dimensions corresponds to multiplication of the base
-- dimensions' exponents by the exponent.
--
-- We limit ourselves to integer powers of Dimensionals as fractional
-- powers make little physical sense.
type family (d::Dimension) ^ (x::TypeInt) where
  DOne ^ x = DOne
  d ^ 'Zero = DOne
  d ^ 'Pos1 = d
  ('Dim l  m  t  i  th  n  j) ^ x
    = 'Dim (l N.* x) (m N.* x) (t N.* x) (i N.* x) (th N.* x) (n N.* x) (j N.* x)

-- | Roots of dimensions corresponds to division of the base dimensions'
-- exponents by the order of the root.
type family NRoot (d::Dimension) (x::TypeInt) where
  NRoot DOne x = DOne
  NRoot d 'Pos1 = d
  NRoot ('Dim l  m  t  i  th  n  j) x
    = 'Dim (l N./ x) (m N./ x) (t N./ x) (i N./ x) (th N./ x) (n N./ x) (j N./ x)

-- | Square root is a special case of 'NRoot' with order 2.
type Sqrt d = NRoot d 'Pos2

-- | Cube root is a special case of 'NRoot' with order 3.
type Cbrt d = NRoot d 'Pos3

-- | A KnownDimension is one for which we can construct a term-level representation.
-- Each validly constructed type of kind 'Dimension' has a 'KnownDimension' instance.
--
-- While 'KnownDimension' is a constraint synonym, the presence of @'KnownDimension' d@ in
--  a context allows use of @'dimension' :: 'Proxy' d -> 'Dimension''@.
type KnownDimension (d :: Dimension) = HasDimension (Proxy d)

instance ( KnownTypeInt l
         , KnownTypeInt m
         , KnownTypeInt t
         , KnownTypeInt i
         , KnownTypeInt th
         , KnownTypeInt n
         , KnownTypeInt j
         ) => HasDynamicDimension (Proxy ('Dim l m t i th n j))
  where

instance ( KnownTypeInt l
         , KnownTypeInt m
         , KnownTypeInt t
         , KnownTypeInt i
         , KnownTypeInt th
         , KnownTypeInt n
         , KnownTypeInt j
         ) => HasDimension (Proxy ('Dim l m t i th n j))
  where
    dimension _ = Dim'
                (toNum (Proxy :: Proxy l))
                (toNum (Proxy :: Proxy m))
                (toNum (Proxy :: Proxy t))
                (toNum (Proxy :: Proxy i))
                (toNum (Proxy :: Proxy th))
                (toNum (Proxy :: Proxy n))
                (toNum (Proxy :: Proxy j))
