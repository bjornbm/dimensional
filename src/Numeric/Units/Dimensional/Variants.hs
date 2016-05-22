{-# OPTIONS_HADDOCK not-home, show-extensions #-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
   Copyright  : Copyright (C) 2006-2015 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn@buckwalter.se
   Stability  : Stable
   Portability: GHC only

Provides a type level representation of 'Variant's of dimensional values,
which may be quantities or units.
-}
module Numeric.Units.Dimensional.Variants
(
  type Variant(..),
  Metricality(..),
  type (*), type (/), type Weaken,
  type CompatibleVariants
)
where

import Control.DeepSeq
import Data.Data
import qualified Data.ExactPi.TypeLevel as E
import GHC.Generics

-- | Encodes whether a unit is a metric unit, that is, whether it can be combined
-- with a metric prefix to form a related unit.
data Metricality = Metric    -- ^ Capable of receiving a metric prefix.
                 | NonMetric -- ^ Incapable of receiving a metric prefix.
  deriving (Eq, Ord, Data, Typeable, Generic)

instance NFData Metricality where -- instance is derived from Generic instance

{-
The variety 'v' of 'Dimensional'

The phantom type variable v is used to distinguish between units
and quantities. It must be one of the following:
-}

-- | The kind of variants of dimensional values.
data Variant = DQuantity E.ExactPi' -- ^ The value is a quantity, stored as an `ExactPi` multiple of its value in its dimension's SI coherent unit.
             | DUnit Metricality  -- ^ The value is a unit, possibly a 'Metric' unit.
  deriving (Typeable, Generic)

{-
We will reuse the operators and function names from the Prelude.
To prevent unpleasant surprises we give operators the same fixity
as the Prelude.
-}

infixl 7  *

-- | Forms the product of two 'Variant's.
--
-- The product of units is a non-metric unit.
--
-- The product of quantities is a quantity.
type family (v1 :: Variant) * (v2 :: Variant) :: Variant where
  'DUnit m1 * 'DUnit m2 = 'DUnit 'NonMetric
  'DQuantity s1 * 'DQuantity s2 = 'DQuantity (s1 E.* s2)

type family (v1 :: Variant) / (v2 :: Variant) :: Variant where
  'DUnit m1 / 'DUnit m2 = 'DUnit 'NonMetric
  'DQuantity s1 / 'DQuantity s2 = 'DQuantity (s1 E./ s2)

-- | Weakens a 'Variant' by forgetting possibly uninteresting type-level information.
type family Weaken (v :: Variant) :: Variant where
  Weaken ('DQuantity s) = 'DQuantity s
  Weaken ('DUnit m) = 'DUnit 'NonMetric

-- | Two 'Variant's are compatible when dimensional values of the first may be converted
-- into the second merely by changing the representation of their values.
type family AreCompatible (v1 :: Variant) (v2 :: Variant) :: Bool where
  AreCompatible ('DQuantity s1)  ('DQuantity s2) = 'True
  AreCompatible ('DUnit m) ('DUnit 'NonMetric)   = 'True
  AreCompatible s s   = 'True
  AreCompatible s1 s2 = 'False

-- | Two 'Variant's are compatible when dimensional values of the first may be converted
-- into the second merely by changing the representation of their values.
type CompatibleVariants v1 v2 = ('True ~ AreCompatible v1 v2)
