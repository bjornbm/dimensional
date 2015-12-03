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
{-# LANGUAGE FlexibleInstances #-}
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

import Control.DeepSeq
import Data.Data
import Data.ExactPi
import Data.Monoid (Monoid(..))
import GHC.Generics
import Prelude (Eq(..), Num(..), Fractional(..), Floating(..), Show(..), Maybe(..), (.), ($), (++), (&&), otherwise, div)
import qualified Prelude as P
import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.Coercion
import Numeric.Units.Dimensional.UnitNames (UnitName, baseUnitName)
import qualified Numeric.Units.Dimensional.UnitNames.InterchangeNames as I
import qualified Numeric.Units.Dimensional.Dimensions.TermLevel as D

-- | A 'Quantity' whose 'Dimension' is only known dynamically.
data AnyQuantity a = AnyQuantity Dimension' a
  deriving (Eq, Data, Generic, Generic1, Typeable)

instance (Show a) => Show (AnyQuantity a) where
  show (AnyQuantity d a) = (show a) ++ " " ++ (show . baseUnitName $ d)

instance HasDimension (AnyQuantity a) where
  dimension (AnyQuantity d _) = d

instance NFData a => NFData (AnyQuantity a) -- instance is derived from Generic instance

-- | 'AnyQuantity's form a 'Monoid' under multiplication, but not under addition because
-- they may not be added together if their dimensions do not match.
instance Num a => Monoid (AnyQuantity a) where
  mempty = demoteQuantity (1 *~ one)
  mappend (AnyQuantity d1 a1) (AnyQuantity d2 a2) = AnyQuantity (d1 D.* d2) (a1 P.* a2)

instance Num a => Num (Maybe (AnyQuantity a)) where
  (+) = liftAQ2 matching (P.+)
  (-) = liftAQ2 matching (P.-)
  (*) = liftAQ2 (definitely (D.*)) (P.*)
  negate = liftAQ Just (P.negate)
  abs = liftAQ Just (P.abs)
  signum = liftAQ (P.const $ Just D.dOne) (P.signum)
  fromInteger = Just . AnyQuantity D.dOne . P.fromInteger

instance Fractional a => Fractional (Maybe (AnyQuantity a)) where
  (/) = liftAQ2 (definitely (D./)) (P./)
  recip = liftAQ (Just . D.recip) (P.recip)
  fromRational = Just . AnyQuantity D.dOne . P.fromRational

instance Floating a => Floating (Maybe (AnyQuantity a)) where
  pi = Just $ AnyQuantity D.dOne P.pi
  exp = liftDimensionless P.exp
  log = liftDimensionless P.log
  sqrt = liftAQ div2 P.sqrt
    where
      div2 d@(Dim' l m t i th n j) | P.all P.even (D.asList d) = Just $ Dim' (l `div` 2) (m `div` 2) (t `div` 2) (i `div` 2) (th `div` 2) (n `div` 2) (j `div` 2)
                                   | otherwise = Nothing
  (**) = liftAQ2 (matching3 D.dOne) (P.**)
  logBase = liftAQ2 (matching3 D.dOne) (P.logBase)
  sin = liftDimensionless P.sin
  cos = liftDimensionless P.cos
  tan = liftDimensionless P.tan
  asin = liftDimensionless P.asin
  acos = liftDimensionless P.acos
  atan = liftDimensionless P.atan
  sinh = liftDimensionless P.sinh
  cosh = liftDimensionless P.cosh
  tanh = liftDimensionless P.tanh
  asinh = liftDimensionless P.asinh
  acosh = liftDimensionless P.acosh
  atanh = liftDimensionless P.atanh

liftDimensionless :: (a -> a) -> Maybe (AnyQuantity a) -> Maybe (AnyQuantity a)
liftDimensionless = liftAQ (matching D.dOne)

liftAQ :: (Dimension' -> Maybe Dimension') -> (a -> a) -> Maybe (AnyQuantity a) -> Maybe (AnyQuantity a)
liftAQ fd fv (Just (AnyQuantity d a)) | Just d' <- fd d = Just . AnyQuantity d' $ fv a
liftAQ _ _ _ = Nothing

liftAQ2 :: (Dimension' -> Dimension' -> Maybe Dimension') -> (a -> a -> a) -> Maybe (AnyQuantity a) -> Maybe (AnyQuantity a) -> Maybe (AnyQuantity a)
liftAQ2 fd fv (Just (AnyQuantity d1 a1)) (Just (AnyQuantity d2 a2)) | Just d' <- fd d1 d2 = Just . AnyQuantity d' $ fv a1 a2
liftAQ2 _ _ _ _ = Nothing

definitely :: (a -> a -> a) -> a -> a -> Maybe a
definitely f x y = Just $ f x y

matching :: Eq a => a -> a -> Maybe a
matching x y | x == y    = Just x
             | otherwise = Nothing

matching3 :: Eq a => a -> a -> a -> Maybe a
matching3 x y z | x == y && y == z = Just x
                | otherwise = Nothing

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

instance I.HasInterchangeName AnyUnit where
  interchangeName (AnyUnit _ n _) = I.interchangeName n

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
