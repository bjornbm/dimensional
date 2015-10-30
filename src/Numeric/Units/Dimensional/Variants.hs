{-# OPTIONS_HADDOCK not-home, show-extensions #-}

{-# LANGUAGE AutoDeriveTypeable #-}
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
  type (*), type Weaken
)
where

import Data.Data
import GHC.Generics

-- | Encodes whether a unit is a metric unit, that is, whether it can be combined
-- with a metric prefix to form a related unit.
data Metricality = Metric    -- ^ Capable of receiving a metric prefix.
                 | NonMetric -- ^ Incapable of receiving a metric prefix.
  deriving (Eq, Ord, Data, Typeable, Generic)

{-
The variety 'v' of 'Dimensional'

The phantom type variable v is used to distinguish between units
and quantities. It must be one of the following:
-}

-- | The kind of variants of dimensional values.
data Variant = DQuantity         -- ^ The value is a quantity.
             | DUnit Metricality -- ^ The value is a unit, possibly a 'Metric' unit.
  deriving (Eq, Ord, Data, Typeable, Generic)

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
  'DUnit m1  * 'DUnit m2  = 'DUnit 'NonMetric
  'DQuantity * 'DQuantity = 'DQuantity 

-- | Weakens a 'Variant' by forgetting possibly uninteresting type-level information.
type family Weaken (v :: Variant) :: Variant where
  Weaken 'DQuantity = 'DQuantity
  Weaken ('DUnit m) = 'DUnit 'NonMetric
