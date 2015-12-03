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
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.Units.Dimensional.Dynamic
(
  -- * Dynamic Quantities
  AnyQuantity
, DynQuantity
, DynamicQuantity(..)
  -- * Dynamic Units
, AnyUnit
, demoteUnit, promoteUnit, demoteUnit'
  -- ** Arithmetic on Dynamic Units
, (*), (/), recip
) where

import Control.DeepSeq
import Data.Data
import Data.ExactPi
import Data.Monoid (Monoid(..))
import GHC.Generics
import Prelude (Eq(..), Num, Fractional, Floating(..), Show(..), Maybe(..), (.), ($), (&&), (++), all, const, div, even, fmap, otherwise)
import qualified Prelude as P
import Numeric.Units.Dimensional hiding ((*), (/), recip)
import Numeric.Units.Dimensional.Coercion
import Numeric.Units.Dimensional.UnitNames (UnitName, baseUnitName)
import qualified Numeric.Units.Dimensional.UnitNames as N
import qualified Numeric.Units.Dimensional.Dimensions.TermLevel as D

-- | The class of types that can be used to model 'Quantity's whose 'Dimension's are
-- only known dynamically.
class DynamicQuantity (q :: * -> *) where
  -- | Converts a 'Quantity' of statically known 'Dimension' into an dynamic quantity
  -- such as an 'AnyQuantity' or a 'DynQuantity'.
  demoteQuantity :: (KnownDimension d) => Quantity d a -> q a
  -- | Converts an dynamic quantity such as an 'AnyQuantity' or a 'DynQuantity' into a
  -- 'Quantity' of statically known 'Dimension', or 'Nothing' if the dynamic quantity
  -- does not represent a 'Quantity' of that dimension.
  promoteQuantity :: (KnownDimension d) => q a -> Maybe (Quantity d a)

-- | A 'Quantity' whose 'Dimension' is only known dynamically.
data AnyQuantity a = AnyQuantity Dimension' a
  deriving (Eq, Data, Generic, Generic1, Typeable)

instance (Show a) => Show (AnyQuantity a) where
  show (AnyQuantity d a) = (show a) ++ " " ++ (show . baseUnitName $ d)

instance D.HasDynamicDimension (AnyQuantity a) where
  dynamicDimension = Just . dimension

instance HasDimension (AnyQuantity a) where
  dimension (AnyQuantity d _) = d

instance NFData a => NFData (AnyQuantity a) -- instance is derived from Generic instance

instance DynamicQuantity AnyQuantity where
  -- these kind signatures are required by GHC 7.8
  demoteQuantity :: forall a (d :: Dimension).(KnownDimension d) => Quantity d a -> AnyQuantity a
  demoteQuantity (Quantity val) = AnyQuantity dim val
    where dim = dimension (Proxy :: Proxy d)
  promoteQuantity :: forall a (d :: Dimension).(KnownDimension d) => AnyQuantity a -> Maybe (Quantity d a)
  promoteQuantity (AnyQuantity dim val) | dim == dim' = Just . Quantity $ val
                                        | otherwise   = Nothing
    where
      dim' = dimension (Proxy :: Proxy d)

-- | 'AnyQuantity's form a 'Monoid' under multiplication, but not under addition because
-- they may not be added together if their dimensions do not match.
instance Num a => Monoid (AnyQuantity a) where
  mempty = demoteQuantity (1 *~ one)
  mappend (AnyQuantity d1 a1) (AnyQuantity d2 a2) = AnyQuantity (d1 D.* d2) (a1 P.* a2)

-- | Possibly a 'Quantity' whose 'Dimension' is only known statically.
--
-- By modeling the absence of a value, this type differs from 'AnyQuantity' in that it may
-- not be a 'Quantity' of any 'Dimension' whatsoever, but in exchange it gains instances
-- for the common numeric classes. It's therefore useful for manipulating, and not merely storing,
-- quantities of unknown dimension.
newtype DynQuantity a = DynQuantity (Maybe (AnyQuantity a))
  deriving (Eq, Data, Generic, Generic1, Typeable, Show)

instance NFData a => NFData (DynQuantity a) -- instance is derived from Generic instance

instance DynamicQuantity DynQuantity where
  demoteQuantity = DynQuantity . Just . demoteQuantity
  promoteQuantity (DynQuantity (Just x)) = promoteQuantity x
  promoteQuantity _                      = Nothing

instance D.HasDynamicDimension (DynQuantity a) where
  dynamicDimension (DynQuantity (Just x)) = D.dynamicDimension x
  dynamicDimension _                      = Nothing

instance Num a => Num (DynQuantity a) where
  (+) = liftDQ2 matching (P.+)
  (-) = liftDQ2 matching (P.-)
  (*) = liftDQ2 (always (D.*)) (P.*)
  negate = liftDQ Just (P.negate)
  abs = liftDQ Just (P.abs)
  signum = liftDQ (const $ Just D.dOne) (P.signum)
  fromInteger = demoteQuantity . (*~ one) . P.fromInteger

instance Fractional a => Fractional (DynQuantity a) where
  (/) = liftDQ2 (always (D./)) (P./)
  recip = liftDQ (Just . D.recip) (P.recip)
  fromRational = demoteQuantity . (*~ one) . P.fromRational

instance Floating a => Floating (DynQuantity a) where
  pi = DynQuantity . Just $ AnyQuantity D.dOne P.pi
  exp = liftDimensionless P.exp
  log = liftDimensionless P.log
  sqrt = liftDQ div2 (P.sqrt)
  (**) = liftDQ2 (matching3 D.dOne) (P.**)
  logBase = liftDQ2 (matching3 D.dOne) (P.logBase)
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

instance Num a => Monoid (DynQuantity a) where
  mempty = demoteQuantity (1 *~ one)
  mappend = (P.*)

-- Divides a dimension by two, or returns Nothing if it can't be done.
div2 :: Dimension' -> Maybe Dimension'
div2 d | all even ds = Just . fromList . fmap (`div` 2) $ ds
       | otherwise   = Nothing
  where
    ds = D.asList d
    fromList [l, m, t, i, th, n, j] = Dim' l m t i th n j

-- Applies Just to the result of a two argument function.
always :: (a -> a -> a) -> a -> a -> Maybe a
always f x y = Just $ f x y

-- If three items match, then Just that item, otherwise Nothing.
matching3 :: Eq a => a -> a -> a -> Maybe a
matching3 x y z | x == y && x == z = Just x
                | otherwise        = Nothing

-- If two items match, then Just that item, otherwise Nothing.
matching :: Eq a => a -> a -> Maybe a
matching x y | x == y    = Just x
             | otherwise = Nothing

-- Lifts a function which is only valid on dimensionless quantities into a function on DynQuantitys.
liftDimensionless :: (a -> a) -> DynQuantity a -> DynQuantity a
liftDimensionless = liftDQ (matching D.dOne)

-- Lifts a function on values into a function on DynQuantitys.
liftDQ :: (Dimension' -> Maybe Dimension')
       -> (a -> a)
       -> DynQuantity a -> DynQuantity a
liftDQ fd fv (DynQuantity (Just (AnyQuantity d v))) | Just d' <- fd d = DynQuantity . Just $ AnyQuantity d' (fv v)
liftDQ _ _ _ = DynQuantity Nothing

-- Lifts a function on values into a function on DynQuantitys.
liftDQ2 :: (Dimension' -> Dimension' -> Maybe Dimension')
        -> (a -> a -> a)
        -> DynQuantity a -> DynQuantity a -> DynQuantity a
liftDQ2 fd fv (DynQuantity (Just (AnyQuantity d1 v1))) (DynQuantity (Just (AnyQuantity d2 v2))) | Just d' <- fd d1 d2 = DynQuantity . Just $ AnyQuantity d' (fv v1 v2)
liftDQ2 _ _ _ _ = DynQuantity Nothing

-- | A 'Unit' whose 'Dimension' is only known dynamically.
data AnyUnit = AnyUnit Dimension' (UnitName 'NonMetric) ExactPi
  deriving (Generic, Typeable)

instance Show AnyUnit where
  show (AnyUnit _ n e) = "1 " ++ (show n) ++ " =def= " ++ (show e) ++ " of the SI base unit"

instance D.HasDynamicDimension AnyUnit where
  dynamicDimension = Just . dimension

instance HasDimension AnyUnit where
  dimension (AnyUnit d _ _) = d

-- | 'AnyUnit's form a 'Monoid' under multiplication.
instance Monoid AnyUnit where
  mempty = demoteUnit' one
  mappend = (Numeric.Units.Dimensional.Dynamic.*)

-- | Converts a 'Unit' of statically known 'Dimension' into an 'AnyUnit'.
demoteUnit :: forall m d a.(KnownDimension d) => Unit m d a -> AnyUnit
demoteUnit u = AnyUnit dim (name $ weaken u) (exactValue u)
  where
    dim = dimension (Proxy :: Proxy d)

-- | Converts a 'Unit' of statically known 'Dimension' into an 'AnyUnit'.
--
-- This is the same as the more general 'demoteUnit' but is useful in certain circumstances to avoid
-- needlessly introducing an ambiguous type variable.
demoteUnit' :: (KnownDimension d) => Unit m d ExactPi -> AnyUnit
demoteUnit' = demoteUnit

-- | Converts an 'AnyUnit' into a 'Unit' of statically known 'Dimension', or 'Nothing' if the dimension does not match.
--
-- The result is represented in 'ExactPi', conversion to other representations is possible using 'changeRepApproximate'.
promoteUnit :: forall d.(KnownDimension d) => AnyUnit -> Maybe (Unit 'NonMetric d ExactPi)
promoteUnit (AnyUnit dim n e) | dim == dim' = Just $ mkUnitR n e siUnit
                              | otherwise   = Nothing
  where
    dim' = dimension (Proxy :: Proxy d)

-- | Forms the reciprocal of a dynamic unit.
recip :: AnyUnit -> AnyUnit
recip (AnyUnit d n e) = AnyUnit (D.recip d) (N.nOne N./ n) (P.recip e)

-- | Forms the product of two dynamic units.
(*) :: AnyUnit -> AnyUnit -> AnyUnit
(AnyUnit d1 n1 e1) * (AnyUnit d2 n2 e2) = AnyUnit (d1 D.* d2) (n1 N.* n2) (e1 P.* e2)

-- | Forms the quotient of two dynamic units.
(/) :: AnyUnit -> AnyUnit -> AnyUnit
(AnyUnit d1 n1 e1) / (AnyUnit d2 n2 e2) = AnyUnit (d1 D./ d2) (n1 N./ n2) (e1 P./ e2)
