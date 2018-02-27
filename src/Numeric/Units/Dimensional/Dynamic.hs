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
, HasDynamicDimension(..), DynamicDimension(..)
, promoteQuantity, demoteQuantity
, (*~), (/~), invalidQuantity, polydimensionalZero
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
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))
import GHC.Generics
import Prelude (Eq(..), Num, Fractional, Floating, Show(..), Bool(..), Maybe(..), (.), ($), (++), (&&), id, otherwise, error)
import qualified Prelude as P
import Numeric.Units.Dimensional hiding ((*~), (/~), (*), (/), (^), recip, nroot, siUnit)
import qualified Numeric.Units.Dimensional as Dim
import Numeric.Units.Dimensional.Coercion
import Numeric.Units.Dimensional.UnitNames (UnitName, baseUnitName)
import qualified Numeric.Units.Dimensional.UnitNames.InterchangeNames as I
import qualified Numeric.Units.Dimensional.UnitNames as N
import Numeric.Units.Dimensional.Dimensions.TermLevel (HasDynamicDimension(..), DynamicDimension(..), matchDimensions, isCompatibleWith)
import qualified Numeric.Units.Dimensional.Dimensions.TermLevel as D

-- | The class of types that can be used to model 'Quantity's that are certain to have a value with
-- some dimension.
class Demotable (q :: * -> *) where
  demotableOut :: q a -> AnyQuantity a

-- | The class of types that can be used to model 'Quantity's whose 'Dimension's are
-- only known dynamically.
class Promotable (q :: * -> *) where
  promotableIn :: AnyQuantity a -> q a
  promotableOut :: q a -> DynQuantity a

-- | Forgets information about a 'Quantity' or 'AnyQuantity', yielding an 'AnyQuantity' or a 'DynQuantity'.
demoteQuantity :: (Demotable q, Promotable d) => q a -> d a
demoteQuantity = promotableIn . demotableOut

-- | Converts a dynamic quantity such as an 'AnyQuantity' or a 'DynQuantity' into a
-- 'Quantity', or to 'Nothing' if the dynamic quantity cannot be represented in the
-- narrower result type.
promoteQuantity :: forall a d q.(Promotable q, KnownDimension d) => q a -> Maybe (Quantity d a)
promoteQuantity = promoteQ . promotableOut
  where
    dim' = dimension (Proxy :: Proxy d)
    promoteQ (DynQuantity d v) | d `isCompatibleWith` dim' = Just . Quantity $ v
                               | otherwise                 = Nothing

instance (KnownDimension d) => Demotable (Quantity d) where
  demotableOut q@(Quantity x) = AnyQuantity (dimension q) x    

-- | A 'Quantity' whose 'Dimension' is only known dynamically.
data AnyQuantity a = AnyQuantity !Dimension' !a
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
  promotableOut (AnyQuantity d a) = DynQuantity (SomeDimension d) a

instance Demotable AnyQuantity where
  demotableOut = id

-- | 'AnyQuantity's form a 'Semigroup' under multiplication, but not under addition because
-- they may not be added together if their dimensions do not match.
instance Num a => Semigroup (AnyQuantity a) where
  (AnyQuantity d1 a1) <> (AnyQuantity d2 a2) = AnyQuantity (d1 D.* d2) (a1 P.* a2)

-- | 'AnyQuantity's form a 'Monoid' under multiplication, but not under addition because
-- they may not be added together if their dimensions do not match.
instance Num a => Monoid (AnyQuantity a) where
  mempty = demoteQuantity (1 Dim.*~ one)
  mappend = (Data.Semigroup.<>)

-- | Possibly a 'Quantity' whose 'Dimension' is only known dynamically.
--
-- By modeling the absence of a value, this type differs from 'AnyQuantity' in that it may
-- not be a 'Quantity' of any 'Dimension' whatsoever, but in exchange it gains instances
-- for the common numeric classes. It's therefore useful for manipulating, and not merely storing,
-- quantities of unknown dimension.
--
-- This type also contains a 'polydimensionalZero', representing zero value of any dimension.
--
-- Note that the 'Eq' instance for 'DynQuantity' equates all representations of an invalid value,
-- and also does not equate polydimensional zero with zero of any specific dimension.
data DynQuantity a = DynQuantity !DynamicDimension a -- we can't have strictness annotation on a as it is sometimes undefined
  deriving (Data, Generic, Generic1, Typeable)

instance Eq a => Eq (DynQuantity a) where
  (DynQuantity NoDimension _) == (DynQuantity NoDimension _) = True -- all invalid quantities are equal
  (DynQuantity NoDimension _) == _                           = False -- invalid quanties are not equal to any other quantity
  _                           == (DynQuantity NoDimension _) = False
  (DynQuantity d1 v1)         == (DynQuantity d2 v2)         = d1 == d2 && v1 == v2

instance NFData a => NFData (DynQuantity a) -- instance is derived from Generic instance

instance Show a => Show (DynQuantity a) where
  show (DynQuantity NoDimension _) = "invalidQuantity"
  show (DynQuantity AnyDimension v) = show v
  show (DynQuantity (SomeDimension d) v) = show $ AnyQuantity d v

instance Promotable DynQuantity where
  promotableIn (AnyQuantity d a) = DynQuantity (SomeDimension d) a
  promotableOut = id

instance HasDynamicDimension (DynQuantity a) where
  dynamicDimension (DynQuantity d _) = d

instance Num a => Num (DynQuantity a) where
  x + y = liftDQ2 matchDimensions (P.+) x y
  x - y = liftDQ2 matchDimensions (P.-) x y
  x * y = liftDQ2 (valid2 (D.*)) (P.*) x y
  negate = liftDQ id P.negate
  abs = liftDQ id P.abs
  signum = liftDQ (constant D.dOne) P.signum
  fromInteger = demoteQuantity . (Dim.*~ one) . P.fromInteger

instance Fractional a => Fractional (DynQuantity a) where
  x / y = liftDQ2 (valid2 (D./)) (P./) x y
  recip = liftDQ (valid D.recip) P.recip
  fromRational = demoteQuantity . (Dim.*~ one) . P.fromRational

instance Floating a => Floating (DynQuantity a) where
  pi = demoteQuantity pi
  exp = liftDimensionless P.exp
  log = liftDimensionless P.log
  sqrt = liftDQ (whenValid $ D.nroot 2) P.sqrt
  (**) = liftDQ2 (matchDimensions3 $ SomeDimension D.dOne) (P.**)
  logBase = liftDQ2 (matchDimensions3 $ SomeDimension D.dOne) P.logBase
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

-- | 'DynQuantity's form a 'Semigroup' under multiplication, but not under addition because
-- they may not be added together if their dimensions do not match.
instance Num a => Semigroup (DynQuantity a) where
    (<>) = (P.*)

-- | 'DynQuantity's form a 'Monoid' under multiplication, but not under addition because
-- they may not be added together if their dimensions do not match.
instance Num a => Monoid (DynQuantity a) where
  mempty = demoteQuantity (1 Dim.*~ one)
  mappend = (Data.Semigroup.<>)

-- | A 'DynQuantity' which does not correspond to a value of any dimension.
invalidQuantity :: DynQuantity a
invalidQuantity = DynQuantity NoDimension $ error "Attempt to evaluate the value of an invalid quantity."

-- | A 'DynQuantity' which corresponds to zero value of any dimension.
--
-- When combined through arithmetic with other 'DynQuantity's, inference is performed. For example,
-- adding a length to polydimensional zero produces that length. Adding two polydimensional zeros produces another.
-- Taking the sine of a polydimensional zero interprets it as a dimensionless zero and produces a dimensionless result.
--
-- Note that division by 'polydimensionalZero' produces a polydimensional result, which may be an error or some representation
-- of infinity, as determined by the underlying arithmetic type. This behavior was chosen for consistency with the behavior of division
-- by zero 'DynQuantity's of a specific dimension.
polydimensionalZero :: (Num a) => DynQuantity a
polydimensionalZero = DynQuantity AnyDimension 0

-- Lifts a function which is only valid on dimensionless quantities into a function on DynQuantitys.
liftDimensionless :: (a -> a) -> DynQuantity a -> DynQuantity a
liftDimensionless f = liftDQ (matchDimensions $ SomeDimension D.dOne) f

-- Lifts a function on values into a function on DynQuantitys.
liftDQ :: (DynamicDimension -> DynamicDimension) -- ^ How the function operates on dimensions.
       -> (a -> a) -- ^ How the function operates on values.
       -> DynQuantity a -> DynQuantity a
liftDQ fd fv (DynQuantity d v) = case fd d of
                                   NoDimension -> invalidQuantity
                                   d' -> DynQuantity d' $ fv v

-- Lifts a function on values into a function on DynQuantitys.
--
-- This works by treating polydimensional zeros as dimensionless zeros. If that is not the desired behavior,
-- handle polydimensional zeros first and then call this function.
liftDQ2 :: (DynamicDimension -> DynamicDimension -> DynamicDimension)
        -> (a -> a -> a)
        -> DynQuantity a -> DynQuantity a -> DynQuantity a
liftDQ2 fd fv (DynQuantity d1 v1) (DynQuantity d2 v2) = case fd d1 d2 of
                                                          NoDimension -> invalidQuantity
                                                          d' -> DynQuantity d' $ fv v1 v2

-- Transforms a dynamic dimension in a way which is always valid
valid :: (Dimension' -> Dimension') -> DynamicDimension -> DynamicDimension
valid _ AnyDimension      = AnyDimension
valid f (SomeDimension d) = SomeDimension (f d)
valid _ NoDimension       = NoDimension

whenValid :: (Dimension' -> Maybe Dimension') -> DynamicDimension -> DynamicDimension
whenValid _ AnyDimension = AnyDimension
whenValid f (SomeDimension d) | Just d' <- f d = SomeDimension d'
whenValid _ _ = NoDimension

constant :: Dimension' -> DynamicDimension -> DynamicDimension
constant d AnyDimension = SomeDimension d
constant d (SomeDimension _) = SomeDimension d
constant _ _ = NoDimension

-- Transforms two dynamic dimensions in a way which is always valid
valid2 :: (Dimension' -> Dimension' -> Dimension') -> DynamicDimension -> DynamicDimension -> DynamicDimension
valid2 _ AnyDimension       (SomeDimension _)  = AnyDimension
valid2 _ (SomeDimension _)  AnyDimension       = AnyDimension
valid2 _ AnyDimension       AnyDimension       = AnyDimension
valid2 f (SomeDimension d1) (SomeDimension d2) = SomeDimension (f d1 d2)
valid2 _ _                  _                  = NoDimension

matchDimensions3 :: DynamicDimension -> DynamicDimension -> DynamicDimension -> DynamicDimension
matchDimensions3 x y z = matchDimensions x (matchDimensions y z)

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

-- | 'AnyUnit's form a 'Semigroup' under multiplication.
instance Semigroup AnyUnit where
  (<>) = (Numeric.Units.Dimensional.Dynamic.*)

-- | 'AnyUnit's form a 'Monoid' under multiplication.
instance Monoid AnyUnit where
  mempty = demoteUnit' one
  mappend = (Data.Semigroup.<>)

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
x /~ (AnyUnit d _ e) = case promotableOut x of
                         DynQuantity d' x' | d' `isCompatibleWith` d -> Just $ x' P./ approximateValue e
                                           | otherwise -> Nothing
