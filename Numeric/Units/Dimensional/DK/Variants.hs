{-# OPTIONS_HADDOCK not-home, show-extensions #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Numeric.Units.Dimensional.DK.Variants
(
  Metricality(..),
  type Variant(..),
  type (*), type Weaken
)
where

import Data.Typeable

data Metricality = Metric
                 | NonMetric
  deriving (Eq, Ord, Typeable)

{-
The variety 'v' of 'Dimensional'

The phantom type variable v is used to distinguish between units
and quantities. It must be one of the following:
-}

data Variant = DQuantity | DUnit Metricality
  deriving (Eq, Ord, Typeable)

{-
We will reuse the operators and function names from the Prelude.
To prevent unpleasant surprises we give operators the same fixity
as the Prelude.
-}

infixl 7  *

type family (v1 :: Variant) * (v2 :: Variant) :: Variant where
  'DUnit m1  * 'DUnit m2  = 'DUnit 'NonMetric
  'DQuantity * 'DQuantity = 'DQuantity

type family Weaken (v :: Variant) :: Variant where
  Weaken 'DQuantity = 'DQuantity
  Weaken ('DUnit m) = 'DUnit 'NonMetric
