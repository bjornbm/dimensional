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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.Units.Dimensional.Dynamic
(
  -- * Dynamic Quantities
  AnyQuantity
, DynQuantity
, Demotable
, Promotable
, HasDynamicDimension(..)
, promoteQuantity, demoteQuantity
, (*~), (/~), invalidQuantity
  -- * Dynamic Units
, AnyUnit
, demoteUnit, promoteUnit, demoteUnit'
, siUnit, anyUnitName
  -- ** Arithmetic on Dynamic Units
, (*), (/), (^), recip, applyPrefix
) where

import Control.DeepSeq
import Control.Monad
import Data.Data
import Data.ExactPi
import Data.Monoid (Monoid(..))
import GHC.Generics
import Prelude (Eq(..), Num, Fractional, Floating, Show(..), Maybe(..), (.), ($), (&&), (++), const, id, otherwise)
import qualified Prelude as P
import Numeric.Units.Dimensional hiding ((*~), (/~), (*), (/), (^), recip, nroot, siUnit)
import qualified Numeric.Units.Dimensional as Dim
import Numeric.Units.Dimensional.Coercion
import Numeric.Units.Dimensional.UnitNames (UnitName, baseUnitName)
import qualified Numeric.Units.Dimensional.UnitNames.InterchangeNames as I
import qualified Numeric.Units.Dimensional.UnitNames as N
import Numeric.Units.Dimensional.Dimensions.TermLevel (HasDynamicDimension(..))
import qualified Numeric.Units.Dimensional.Dimensions.TermLevel as D

-- | The class of types that can be used to model 'Quantity's that are certain to have a value with
-- some dimension.
class Demotable (q :: * -> *) where
  demotableOut :: q a -> AnyQuantity a
  demotableIn :: AnyQuantity a -> Maybe (q a)

-- | The class of types that can be used to model 'Quantity's whose 'Dimension's are
-- only known dynamically.
class Promotable (q :: * -> *) where
  promotableIn :: AnyQuantity a -> q a
  promotableOut :: q a -> Maybe (AnyQuantity a)

-- | Forgets information about a 'Quantity' or 'AnyQuantity', yielding an 'AnyQuantity' or a 'DynQuantity'.
demoteQuantity :: (Demotable q, Promotable d) => q a -> d a
demoteQuantity = promotableIn . demotableOut

-- | Converts a dynamic quantity such as an 'AnyQuantity' or a 'DynQuantity' into a
-- quantity which is known to have some dimension (which may be statically encoded),
-- such as an 'AnyQuantity' or 'Quantity d', or to 'Nothing' if the dynamic quantity
-- cannot be represented in the narrower result type.
promoteQuantity :: (Demotable q, Promotable d) => d a -> Maybe (q a)
promoteQuantity = demotableIn <=< promotableOut

instance (KnownDimension d) => Demotable (Quantity d) where
  demotableOut q@(Quantity x) = AnyQuantity (dimension q) x
  demotableIn = demoteQ
    where
      -- This implementation is not provided directly inside the instance because it requires ScopedTypeVariables
      -- Placing the signatures inside the instance requires InstanceSigs, which interacts poorly with associated type families
      -- like Dimensional in GHC 7.8.
      demoteQ :: forall a (d' :: Dimension).KnownDimension d' => AnyQuantity a -> Maybe (Quantity d' a)
      demoteQ (AnyQuantity dim val) | dim == dim' = Just . Quantity $ val
                                    | otherwise   = Nothing
        where
          dim' = dimension (Proxy :: Proxy d')



-- | A 'Quantity' whose 'Dimension' is only known dynamically.
data AnyQuantity a = AnyQuantity Dimension' a
  deriving (Eq, Data, Generic, Generic1, Typeable)

instance (Show a) => Show (AnyQuantity a) where
  show (AnyQuantity d a) | d == D.dOne = show a
                         | otherwise   = (show a) ++ " " ++ (show . baseUnitName $ d)

instance HasDynamicDimension (AnyQuantity a) where

instance HasDimension (AnyQuantity a) where
  dimension (AnyQuantity d _) = d

instance NFData a => NFData (AnyQuantity a) -- instance is derived from Generic instance

instance Promotable AnyQuantity where
  promotableIn = id
  promotableOut = Just

instance Demotable AnyQuantity where
  demotableOut = id
  demotableIn = Just

-- | 'AnyQuantity's form a 'Monoid' under multiplication, but not under addition because
-- they may not be added together if their dimensions do not match.
instance Num a => Monoid (AnyQuantity a) where
  mempty = demoteQuantity (1 Dim.*~ one)
  mappend (AnyQuantity d1 a1) (AnyQuantity d2 a2) = AnyQuantity (d1 D.* d2) (a1 P.* a2)


-- | Possibly a 'Quantity' whose 'Dimension' is only known dynamically.
--
-- By modeling the absence of a value, this type differs from 'AnyQuantity' in that it may
-- not be a 'Quantity' of any 'Dimension' whatsoever, but in exchange it gains instances
-- for the common numeric classes. It's therefore useful for manipulating, and not merely storing,
-- quantities of unknown dimension.
--
-- Note that the 'Eq' instance for 'DynQuantity' equates all representations of an invalid value.
newtype DynQuantity a = DynQuantity (Maybe (AnyQuantity a))
  deriving (Eq, Data, Generic, Generic1, Typeable, Show)

instance NFData a => NFData (DynQuantity a) -- instance is derived from Generic instance

instance Promotable DynQuantity where
  promotableIn = DynQuantity . Just
  promotableOut (DynQuantity q) = q

instance HasDynamicDimension (DynQuantity a) where
  dynamicDimension (DynQuantity q) = q >>= dynamicDimension

instance Num a => Num (DynQuantity a) where
  (+) = liftDQ2 matching (P.+)
  (-) = liftDQ2 matching (P.-)
  (*) = liftDQ2 (valid2 (D.*)) (P.*)
  negate = liftDQ (valid id) P.negate
  abs = liftDQ (valid id) P.abs
  signum = liftDQ (valid $ const D.dOne) P.signum
  fromInteger = demoteQuantity . (Dim.*~ one) . P.fromInteger

instance Fractional a => Fractional (DynQuantity a) where
  (/) = liftDQ2 (valid2 (D./)) (P./)
  recip = liftDQ (valid D.recip) P.recip
  fromRational = demoteQuantity . (Dim.*~ one) . P.fromRational

instance Floating a => Floating (DynQuantity a) where
  pi = demoteQuantity pi
  exp = liftDimensionless P.exp
  log = liftDimensionless P.log
  sqrt = liftDQ (D.nroot 2) P.sqrt
  (**) = liftDQ2 (matching3 D.dOne) (P.**)
  logBase = liftDQ2 (matching3 D.dOne) P.logBase
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
  mempty = demoteQuantity (1 Dim.*~ one)
  mappend = (P.*)

invalidQuantity :: DynQuantity a
invalidQuantity = DynQuantity Nothing

-- Lifts a function which is only valid on dimensionless quantities into a function on DynQuantitys.
liftDimensionless :: (a -> a) -> DynQuantity a -> DynQuantity a
liftDimensionless = liftDQ (matching D.dOne)

-- Lifts a function on values into a function on DynQuantitys.
liftDQ :: (Dimension' -> Maybe Dimension')
       -> (a -> a)
       -> DynQuantity a -> DynQuantity a
liftDQ fd fv = coerce f
  where
    f q = do
            (AnyQuantity d v) <- q
            d' <- fd d
            let v' = fv v
            return $ AnyQuantity d' v'

-- Lifts a function on values into a function on DynQuantitys.
liftDQ2 :: (Dimension' -> Dimension' -> Maybe Dimension')
        -> (a -> a -> a)
        -> DynQuantity a -> DynQuantity a -> DynQuantity a
liftDQ2 fd fv = coerce f
  where
    f q1 q2 = do
                (AnyQuantity d1 v1) <- q1
                (AnyQuantity d2 v2) <- q2
                d' <- fd d1 d2
                let v' = fv v1 v2
                return $ AnyQuantity d' v'

-- Transforms an item in a way which is always valid
valid :: (a -> a) -> a -> Maybe a
valid = (Just .)

-- Transforms two items in a way which is always valid
valid2 :: (a -> a -> a) -> a -> a -> Maybe a
valid2 f x y = Just $ f x y

-- If two items match, then Just that item, otherwise Nothing.
matching :: Eq a => a -> a -> Maybe a
matching x y | x == y    = Just x
             | otherwise = Nothing

-- If three items match, then Just that item, otherwise Nothing.
matching3 :: Eq a => a -> a -> a -> Maybe a
matching3 x y z | x == y && x == z = Just x
                | otherwise        = Nothing



-- | A 'Unit' whose 'Dimension' is only known dynamically.
data AnyUnit = AnyUnit Dimension' (UnitName 'NonMetric) ExactPi
  deriving (Generic, Typeable)

instance Show AnyUnit where
  show (AnyUnit _ n e) = (show n) ++ " =def= " ++ (show e) ++ " of the SI base unit"

instance HasDynamicDimension AnyUnit where

instance HasDimension AnyUnit where
  dimension (AnyUnit d _ _) = d

instance I.HasInterchangeName AnyUnit where
  interchangeName (AnyUnit _ n _) = I.interchangeName n

-- | 'AnyUnit's form a 'Monoid' under multiplication.
instance Monoid AnyUnit where
  mempty = demoteUnit' one
  mappend = (Numeric.Units.Dimensional.Dynamic.*)

anyUnitName :: AnyUnit -> UnitName 'NonMetric
anyUnitName (AnyUnit _ n _) = n

-- | The dynamic SI coherent unit of a given dimension.
siUnit :: Dimension' -> AnyUnit
siUnit d = AnyUnit d (baseUnitName d) 1

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
--
-- The result is always tagged as 'NonMetric', conversion to a 'Metric' unit can be attempted using 'strengthen'.
promoteUnit :: forall d.(KnownDimension d) => AnyUnit -> Maybe (Unit 'NonMetric d ExactPi)
promoteUnit (AnyUnit dim n e) | dim == dim' = Just $ mkUnitR n e Dim.siUnit
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

-- | Raises a dynamic unit to an integer power.
(^) :: (P.Integral a) => AnyUnit -> a -> AnyUnit
(AnyUnit d n e) ^ x = AnyUnit (d D.^ P.fromIntegral x) (n N.^ P.fromIntegral x) (e P.^^ x)

-- | Applies a prefix to a dynamic unit.
-- Returns 'Nothing' if the 'Unit' was 'NonMetric' and thus could not accept a prefix.
applyPrefix :: N.Prefix -> AnyUnit -> Maybe AnyUnit
applyPrefix p (AnyUnit d n e) = do
                                  n' <- N.strengthen n
                                  let n'' = N.applyPrefix p n'
                                  let e' = (P.fromRational $ N.scaleFactor p) P.* e
                                  return $ AnyUnit d n'' e'

-- | Forms a dynamic quantity by multipliying a number and a dynamic unit.
(*~) :: (Floating a, Promotable q) => a -> AnyUnit -> q a
x *~ (AnyUnit d _ e) = promotableIn $ AnyQuantity d (x P.* approximateValue e)

-- | Divides a dynamic quantity by a dynamic unit, obtaining the numerical value of the quantity
-- expressed in that unit if they are of the same physical dimension, or 'Nothing' otherwise.
(/~) :: (Floating a, Promotable q) => q a -> AnyUnit -> Maybe a
x /~ (AnyUnit d _ e) = do
                         (AnyQuantity d' x') <- promotableOut x
                         if (d == d')
                           then Just $ x' P./ approximateValue e
                           else Nothing
