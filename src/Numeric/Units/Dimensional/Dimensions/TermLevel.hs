{-# OPTIONS_HADDOCK not-home, show-extensions #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
   Copyright  : Copyright (C) 2006-2018 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn@buckwalter.se
   Stability  : Stable
   Portability: GHC only

This module defines physical dimensions expressed in terms of
the SI base dimensions, including arithmetic.

-}
module Numeric.Units.Dimensional.Dimensions.TermLevel
(
  -- * Type
  Dimension'(..),
  -- * Access to Dimension of Dimensional Values
  HasDimension(..), HasDynamicDimension(..), DynamicDimension(..),
  -- * Dimension Arithmetic
  (*), (/), (^), recip, nroot, sqrt, cbrt,
  -- * Synonyms for Base Dimensions
  dOne,
  dLength, dMass, dTime, dElectricCurrent, dThermodynamicTemperature, dAmountOfSubstance, dLuminousIntensity,
  -- * Deconstruction
  asList,
  -- * Examining Dynamic Dimensions
  matchDimensions, isCompatibleWith, hasSomeDimension
)
where

import Control.DeepSeq
import Data.Data
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))
import GHC.Generics
import Prelude (id, all, fst, snd, fmap, otherwise, divMod, ($), (+), (-), (.), (&&), Int, Show, Eq(..), Ord(..), Maybe(..), Bool(..))
import qualified Prelude as P

-- $setup
-- >>> import Prelude (negate)
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck.Arbitrary
-- >>> instance Arbitrary Dimension' where arbitrary = Dim' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | A physical dimension, encoded as 7 integers, representing a factorization of the dimension into the
-- 7 SI base dimensions. By convention they are stored in the same order as
-- in the 'Numeric.Units.Dimensional.Dimensions.TypeLevel.Dimension' data kind.
data Dimension' = Dim' !Int !Int !Int !Int !Int !Int !Int
  deriving (Show, Eq, Ord, Data, Generic, Typeable)

instance NFData Dimension' where
  rnf !_ = () -- The Dimension' constructor is already fully strict.

instance Semigroup Dimension' where
  (<>) = (*)

-- | The monoid of dimensions under multiplication.
instance Monoid Dimension' where
  mempty = dOne
  mappend = (<>)

-- | The dimension of a dynamic value, which may not have any dimension at all.
data DynamicDimension = NoDimension -- ^ The value has no valid dimension.
                      | SomeDimension Dimension' -- ^ The value has the given dimension.
                      | AnyDimension -- ^ The value may be interpreted as having any dimension.
  deriving (Eq, Ord, Show, Data, Generic, Typeable)

instance NFData DynamicDimension where

-- | Dimensional values, or those that are only possibly dimensional, inhabit this class,
-- which allows access to a term-level representation of their dimension.
class HasDynamicDimension a where
  -- | Gets the 'DynamicDimension' of a dynamic dimensional value, which may be 'NoDimension' if it does not represent
  -- a dimensional value of any 'Dimension'.
  --
  -- A default implementation is available for types that are also in the `HasDimension` typeclass.
  dynamicDimension :: a -> DynamicDimension
  default dynamicDimension :: (HasDimension a) => a -> DynamicDimension
  dynamicDimension = SomeDimension . dimension

-- | Dimensional values inhabit this class, which allows access to a term-level representation of their dimension.
class HasDynamicDimension a => HasDimension a where
  -- | Obtains a term-level representation of a value's dimension.
  dimension :: a -> Dimension'

instance HasDynamicDimension DynamicDimension where
  dynamicDimension = id

instance HasDynamicDimension Dimension' where

instance HasDimension Dimension' where
  dimension = id

-- | Combines two 'DynamicDimension's, determining the 'DynamicDimension' of a quantity that must
-- match both inputs.
--
-- This is the lattice meet operation for 'DynamicDimension'.
matchDimensions :: DynamicDimension -> DynamicDimension -> DynamicDimension
matchDimensions AnyDimension        AnyDimension                   = AnyDimension
matchDimensions d@(SomeDimension _) AnyDimension                   = d
matchDimensions AnyDimension        d@(SomeDimension _)            = d
matchDimensions (SomeDimension d1)  (SomeDimension d2) | d1 == d2  = SomeDimension d1
matchDimensions _                   _                              = NoDimension

-- | Determines if a value that has a 'DynamicDimension' is compatible with a specified 'Dimension''.
isCompatibleWith :: (HasDynamicDimension a) => a -> Dimension' -> Bool
isCompatibleWith = f . dynamicDimension
  where
    f AnyDimension       _             = True
    f (SomeDimension d1) d2 | d1 == d2 = True
    f _                  _             = False

-- | Determines if a value that has a 'DynamicDimension' in fact has any valid dimension at all.
hasSomeDimension :: (HasDynamicDimension a) => a -> Bool
hasSomeDimension = (/= NoDimension) . dynamicDimension

-- | The dimension of dimensionless values.
dOne :: Dimension'
dOne = Dim' 0 0 0 0 0 0 0

dLength, dMass, dTime, dElectricCurrent, dThermodynamicTemperature, dAmountOfSubstance, dLuminousIntensity :: Dimension'
dLength                   = Dim' 1 0 0 0 0 0 0
dMass                     = Dim' 0 1 0 0 0 0 0
dTime                     = Dim' 0 0 1 0 0 0 0
dElectricCurrent          = Dim' 0 0 0 1 0 0 0
dThermodynamicTemperature = Dim' 0 0 0 0 1 0 0
dAmountOfSubstance        = Dim' 0 0 0 0 0 1 0
dLuminousIntensity        = Dim' 0 0 0 0 0 0 1

{-
We will reuse the operators and function names from the Prelude.
To prevent unpleasant surprises we give operators the same fixity
as the Prelude.
-}

infixr 8  ^
infixl 7  *, /

-- | Forms the product of two dimensions.
(*) :: Dimension' -> Dimension' -> Dimension'
(Dim' l m t i th n j) * (Dim' l' m' t' i' th' n' j') = Dim' (l + l') (m + m') (t + t') (i + i') (th + th') (n + n') (j + j')

-- | Forms the quotient of two dimensions.
(/) :: Dimension' -> Dimension' -> Dimension'
(Dim' l m t i th n j) / (Dim' l' m' t' i' th' n' j') = Dim' (l - l') (m - m') (t - t') (i - i') (th - th') (n - n') (j - j')

-- | Raises a dimension to an integer power.
(^) :: Dimension' -> Int -> Dimension'
(Dim' l m t i th n j) ^ x = Dim' (x P.* l) (x P.* m) (x P.* t) (x P.* i) (x P.* th) (x P.* n) (x P.* j)

-- | Forms the reciprocal of a dimension.
recip :: Dimension' -> Dimension'
recip = (dOne /)

-- | Takes the nth root of a dimension, if it exists.
--
-- n must not be zero.
--
-- prop> nroot (negate n) d == nroot n (recip d)
nroot :: Int -> Dimension' -> Maybe Dimension'
nroot n d | n /= 0 && all ((== 0) . snd) ds = fromList . fmap fst $ ds
          | otherwise                      = Nothing
  where
    ds = fmap (`divMod` n) . asList $ d

-- | Takes the square root of a dimension, if it exists.
--
-- prop> sqrt d == nroot 2 d
sqrt :: Dimension' -> Maybe Dimension'
sqrt = nroot 2

-- | Takes the cube root of a dimension, if it exists.
--
-- prop> cbrt d == nroot 3 d
cbrt :: Dimension' -> Maybe Dimension'
cbrt = nroot 3

-- | Converts a dimension to a list of 7 integers, representing the exponent associated with each
-- of the 7 SI base dimensions in the standard order.
asList :: Dimension' -> [Int]
asList (Dim' l m t i th n j) = [l, m, t, i, th, n, j]

-- | Converts a list of integers, representing the exponent associated with each
-- of the 7 SI base dimensions in the standard order, to a dimension.
-- Returns 'Nothing' if the list doesn't contain exactly 7 elements.
fromList :: [Int] -> Maybe Dimension'
fromList [l, m, t, i, th, n, j] = Just $ Dim' l m t i th n j
fromList _ = Nothing
