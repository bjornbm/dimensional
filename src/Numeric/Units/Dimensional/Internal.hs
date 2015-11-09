{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Numeric.Units.Dimensional.Internal
(
  KnownVariant(..),
  Dimensional(..),
  type Unit, type Quantity, type SQuantity
)
where

import Data.Data
import Data.ExactPi
import qualified Data.ExactPi.TypeLevel as E
import GHC.Generics
import Numeric.Units.Dimensional.Dimensions
import Numeric.Units.Dimensional.Variants
import Numeric.Units.Dimensional.UnitNames hiding ((*), (/), (^), weaken, strengthen)
import qualified Numeric.Units.Dimensional.UnitNames.Internal as Name
import Numeric.Units.Dimensional.UnitNames.InterchangeNames (HasInterchangeName(..))

-- | A unit of measurement.
type Unit (m :: Metricality) = Dimensional ('DUnit m)

-- | A dimensional quantity.
type Quantity = SQuantity E.One

-- | A dimensional quantity, stored as an 'ExactPi'' multiple of its value in its dimension's SI coherent unit.
type SQuantity s = Dimensional ('DQuantity s)

-- | A physical quantity or unit.
--
-- We call this data type 'Dimensional' to capture the notion that the
-- units and quantities it represents have physical dimensions.
-- 
-- The type variable 'a' is the only non-phantom type variable and
-- represents the numerical value of a quantity or the scale (w.r.t.
-- SI units) of a unit. For SI units the scale will always be 1. For
-- non-SI units the scale is the ratio of the unit to the SI unit with
-- the same physical dimension.
--
-- Since 'a' is the only non-phantom type we were able to define
-- 'Dimensional' as a newtype, avoiding boxing at runtime.
class KnownVariant (v :: Variant) where
  -- | A dimensional value, either a 'Quantity' or a 'Unit', parameterized by its 'Dimension' and representation.
  data Dimensional v :: Dimension -> * -> *
  -- | A scale factor by which the numerical value of this dimensional value is implicitly multiplied.
  type ScaleFactor v :: E.ExactPi'
  extractValue :: Dimensional v d a -> (a, Maybe ExactPi)
  extractName :: Dimensional v d a -> Maybe (UnitName 'NonMetric)
  injectValue :: (Maybe (UnitName 'NonMetric)) -> (a, Maybe ExactPi) -> Dimensional v d a
  -- | Maps over the underlying representation of a dimensional value.
  -- The caller is responsible for ensuring that the supplied function respects the dimensional abstraction.
  -- This means that the function must preserve numerical values, or linearly scale them while preserving the origin.
  dmap :: (a1 -> a2) -> Dimensional v d a1 -> Dimensional v d a2

deriving instance Typeable Dimensional

instance (E.KnownExactPi s) => KnownVariant ('DQuantity s) where
  newtype Dimensional ('DQuantity s) d a = Quantity' a
    deriving (Eq, Ord, Data, Generic, Generic1
#if MIN_VERSION_base(4,8,0)
     , Typeable -- GHC 7.8 doesn't support deriving this instance
#endif
    )
  type (ScaleFactor ('DQuantity s)) = s
  extractValue (Quantity' x) = (x, Nothing)
  extractName _ = Nothing
  injectValue _ (x, _) = Quantity' x
  dmap f (Quantity' x) = Quantity' (f x)

instance (Typeable m) => KnownVariant ('DUnit m) where
  data Dimensional ('DUnit m) d a = Unit' !(UnitName m) !ExactPi !a
    deriving (Generic, Generic1
#if MIN_VERSION_base(4,8,0)
     , Typeable -- GHC 7.8 doesn't support deriving this instance
#endif
    )
  type (ScaleFactor ('DUnit m)) = E.One
  extractValue (Unit' _ e x) = (x, Just e)
  extractName (Unit' n _ _) = Just . Name.weaken $ n
  injectValue (Just n) (x, Just e) | Just n' <- relax n = Unit' n' e x
                                   | otherwise          = Prelude.error "Shouldn't be reachable. Needed a metric name but got a non-metric one."
  injectValue _        _ = Prelude.error "Shouldn't be reachable. Needed to name a quantity."
  dmap f (Unit' n e x) = Unit' n e (f x)

-- GHC is somewhat unclear about why, but it won't derive this instance, so we give it explicitly.
instance (Bounded a) => Bounded (Quantity d a) where
  minBound = Quantity' minBound
  maxBound = Quantity' maxBound

instance HasInterchangeName (Unit m d a) where
  interchangeName (Unit' n _ _) = interchangeName n

{-
Since quantities form a monoid under addition, but not under multiplication unless they are dimensionless,
we will define a monoid instance that adds.
-}

-- | 'Quantity's of a given 'Dimension' form a 'Monoid' under addition.
instance (Num a) => Monoid (SQuantity s d a) where
  mempty = Quantity' 0
  mappend = undefined -- (+)

{-

= Dimensionless =

For dimensionless quantities pretty much any operation is applicable.
We provide this freedom by making 'Dimensionless' an instance of
'Functor'.
-}

instance (E.KnownExactPi s) => Functor (SQuantity s DOne) where
  fmap = dmap
