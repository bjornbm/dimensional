{- |
    Copyright  : Copyright (C) 2006-2014 Bjorn Buckwalter
    License    : BSD3

    Maintainer : bjorn@buckwalter.se
    Stability  : Stable
    Portability: GHC only?

Define types for manipulation of units and quantities without phantom types for their dimensions.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.Units.Dimensional.DK.Dynamic
(
  AnyQuantity
, demoteQuantity, promoteQuantity
, AnyUnit
, demoteUnit, promoteUnit
) where

import Numeric.Units.Dimensional.DK.Prelude hiding (lookup)
import Numeric.Units.Dimensional.DK.UnitNames (UnitName, baseUnitName)
import Data.ExactPi
import Data.Proxy

data AnyQuantity v = AnyQuantity Dimension' v
  deriving (Eq)

instance (Show v) => Show (AnyQuantity v) where
  show (AnyQuantity d v) = (show v) ++ " " ++ (show . baseUnitName $ d)

instance HasDimension (AnyQuantity v) where
  dimension (AnyQuantity d _) = d

demoteQuantity :: forall d v.(KnownDimension d, Fractional v) => Quantity d v -> AnyQuantity v
demoteQuantity val = AnyQuantity dim (val /~ siUnit)
  where dim = dimension (Proxy :: Proxy d)

promoteQuantity :: forall d v.(KnownDimension d, Fractional v) => AnyQuantity v -> Maybe (Quantity d v)
promoteQuantity (AnyQuantity dim val) | dim == dim' = Just $ val *~ siUnit
                                      | otherwise   = Nothing
                                                    where
                                                      dim' = dimension (Proxy :: Proxy d)

data AnyUnit = AnyUnit Dimension' (UnitName 'NonMetric) ExactPi

instance Show AnyUnit where
  show (AnyUnit _ n e) = "1 " ++ (show n) ++ " =def= " ++ (show e) ++ " of the SI base unit"

instance HasDimension AnyUnit where
  dimension (AnyUnit d _ _) = d

demoteUnit :: forall a d v.(KnownDimension d) => Unit a d v -> AnyUnit
demoteUnit u = AnyUnit dim (name $ weaken u) (exactValue u)
  where
    dim = dimension (Proxy :: Proxy d)

promoteUnit :: forall d.(KnownDimension d) => AnyUnit -> Maybe (Unit 'NonMetric d ExactPi)
promoteUnit (AnyUnit dim n e) | dim == dim' = Just $ mkUnitR n e siUnit
                              | otherwise   = Nothing
  where
    dim' = dimension (Proxy :: Proxy d)
