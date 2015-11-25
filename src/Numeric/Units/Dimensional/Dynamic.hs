{- |
    Copyright  : Copyright (C) 2006-2014 Bjorn Buckwalter
    License    : BSD3

    Maintainer : bjorn@buckwalter.se
    Stability  : Stable
    Portability: GHC only?

Defines types for manipulation of units and quantities without phantom types for their dimensions.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.Units.Dimensional.Dynamic
(
  -- * Dynamic Quantities
  AnyQuantity
, demoteQuantity, promoteQuantity
  -- * Dynamic Units
, AnyUnit
, demoteUnit, promoteUnit
) where

import Numeric.Units.Dimensional.Prelude hiding (lookup)
import Numeric.Units.Dimensional.Coercion
import Numeric.Units.Dimensional.UnitNames (UnitName, baseUnitName)
import Control.DeepSeq
import Data.Data
import Data.ExactPi
import GHC.Generics

-- | A 'Quantity' whose 'Dimension' is only known dynamically.
data AnyQuantity a = AnyQuantity Dimension' a
  deriving (Eq, Data, Generic, Generic1, Typeable)

instance (Show a) => Show (AnyQuantity a) where
  show (AnyQuantity d a) = (show a) ++ " " ++ (show . baseUnitName $ d)

instance HasDimension (AnyQuantity a) where
  dimension (AnyQuantity d _) = d

instance NFData a => NFData (AnyQuantity a) -- instance is derived from Generic instance

-- | Converts a 'Quantity' of statically known 'Dimension' into an 'AnyQuantity'.
demoteQuantity :: forall d a.(KnownDimension d) => Quantity d a -> AnyQuantity a
demoteQuantity (Quantity val) = AnyQuantity dim val
  where dim = dimension (Proxy :: Proxy d)

-- | Converts an 'AnyQuantity' into a 'Quantity' of statically known 'Dimension', or 'Nothing' if the dimension does not match.
promoteQuantity :: forall d a.(KnownDimension d) => AnyQuantity a -> Maybe (Quantity d a)
promoteQuantity (AnyQuantity dim val) | dim == dim' = Just . Quantity $ val
                                      | otherwise   = Nothing
  where
    dim' = dimension (Proxy :: Proxy d)

-- | A 'Unit' whose 'Dimension' is only known dynamically.
data AnyUnit = AnyUnit Dimension' (UnitName 'NonMetric) ExactPi
  deriving (Generic, Typeable)

instance Show AnyUnit where
  show (AnyUnit _ n e) = "1 " ++ (show n) ++ " =def= " ++ (show e) ++ " of the SI base unit"

instance HasDimension AnyUnit where
  dimension (AnyUnit d _ _) = d

-- | Converts a 'Unit' of statically known 'Dimension' into an 'AnyUnit'.
demoteUnit :: forall m d a.(KnownDimension d) => Unit m d a -> AnyUnit
demoteUnit u = AnyUnit dim (name $ weaken u) (exactValue u)
  where
    dim = dimension (Proxy :: Proxy d)

-- | Converts an 'AnyUnit' into a 'Unit' of statically known 'Dimension', or 'Nothing' if the dimension does not match.
--
-- The result is represented in 'ExactPi', conversion to other representations is possible using 'changeRepApproximate'.
promoteUnit :: forall d.(KnownDimension d) => AnyUnit -> Maybe (Unit 'NonMetric d ExactPi)
promoteUnit (AnyUnit dim n e) | dim == dim' = Just $ mkUnitR n e siUnit
                              | otherwise   = Nothing
  where
    dim' = dimension (Proxy :: Proxy d)
