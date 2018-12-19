{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-} -- for Vector instances only
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Numeric.Units.Dimensional.Internal
(
  KnownVariant(..),
  Dimensional(..),
  type Unit, type Quantity, type SQuantity,
  siUnit, showIn,
  liftD, liftD2,
  liftQ, liftQ2
)
where

import Control.Applicative
import Control.DeepSeq
import Control.Monad (liftM)
import Data.AEq (AEq)
import Data.Coerce (coerce)
import Data.Data
import Data.Kind
import Data.ExactPi
import Data.Functor.Classes (Eq1(..), Ord1(..))
import qualified Data.ExactPi.TypeLevel as E
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable(..))
import GHC.Generics
import Numeric.Units.Dimensional.Dimensions
import Numeric.Units.Dimensional.Variants
import Numeric.Units.Dimensional.UnitNames hiding ((*), (/), (^), weaken, strengthen)
import qualified Numeric.Units.Dimensional.UnitNames.Internal as Name
import Numeric.Units.Dimensional.UnitNames.InterchangeNames (HasInterchangeName(..))
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Base as U
import Prelude
  ( Show, Eq(..), Ord, Bounded(..), Num, Fractional, Functor, Real(..)
  , String, Maybe(..), Double
  , (.), ($), (++), (+), (/)
  , show, otherwise, undefined, error, fmap, realToFrac
  )
import qualified Prelude as P

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> import Numeric.Units.Dimensional.Prelude

-- | A unit of measurement.
type Unit (m :: Metricality) = Dimensional ('DUnit m)

-- | A dimensional quantity.
type Quantity = SQuantity E.One

-- | A dimensional quantity, stored as an 'ExactPi'' multiple of its value in its dimension's SI coherent unit.
--
-- The name is an abbreviation for scaled quantity.
type SQuantity s = Dimensional ('DQuantity s)

-- | A KnownVariant is one whose term-level 'Dimensional' values we can represent with an associated data family instance
-- and manipulate with certain functions, not all of which are exported from the package.
--
-- Each validly constructed type of kind 'Variant' has a 'KnownVariant' instance.
class KnownVariant (v :: Variant) where
  -- | A dimensional value, either a 'Quantity' or a 'Unit', parameterized by its 'Dimension' and representation.
  data Dimensional v :: Dimension -> Type -> Type
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

instance KnownVariant ('DQuantity s) where
  newtype Dimensional ('DQuantity s) d a = Quantity a
    deriving (Eq, Ord, AEq, Data, Generic, Generic1, Typeable)
  type (ScaleFactor ('DQuantity s)) = s
  extractValue (Quantity x) = (x, Nothing)
  extractName _ = Nothing
  injectValue _ (x, _) = Quantity x
  dmap = coerce

instance (Typeable m) => KnownVariant ('DUnit m) where
  data Dimensional ('DUnit m) d a = Unit !(UnitName m) !ExactPi !a
    deriving (Generic, Generic1, Typeable)
  type (ScaleFactor ('DUnit m)) = E.One
  extractValue (Unit _ e x) = (x, Just e)
  extractName (Unit n _ _) = Just . Name.weaken $ n
  injectValue (Just n) (x, Just e) | Just n' <- relax n = Unit n' e x
                                   | otherwise          = error "Shouldn't be reachable. Needed a metric name but got a non-metric one."
  injectValue _        _ = error "Shouldn't be reachable. Needed to name a quantity."
  dmap f (Unit n e x) = Unit n e (f x)

-- GHC is somewhat unclear about why, but it won't derive this instance, so we give it explicitly.
instance (Bounded a) => Bounded (SQuantity s d a) where
  minBound = Quantity minBound
  maxBound = Quantity maxBound

instance Eq1 (SQuantity s d) where
  liftEq = coerce

instance Ord1 (SQuantity s d) where
  liftCompare = coerce

instance HasInterchangeName (Unit m d a) where
  interchangeName (Unit n _ _) = interchangeName n

{-
Since quantities form a monoid under addition, but not under multiplication unless they are dimensionless,
we will define a monoid instance that adds.
-}

-- | 'Quantity's of a given 'Dimension' form a 'Semigroup' under addition.
instance (Num a) => Semigroup (SQuantity s d a) where
  (<>) = liftQ2 (+)

-- | 'Quantity's of a given 'Dimension' form a 'Monoid' under addition.
instance (Num a) => Monoid (SQuantity s d a) where
  mempty = Quantity 0
  mappend = liftQ2 (+)

{-

= Dimensionless =

For dimensionless quantities pretty much any operation is applicable.
We provide this freedom by making 'Dimensionless' an instance of
'Functor'.
-}

instance Functor (SQuantity s DOne) where
  fmap = dmap

instance (KnownDimension d) => HasDynamicDimension (Dimensional v d a) where

instance (KnownDimension d) => HasDimension (Dimensional v d a) where
  dimension _ = dimension (Proxy :: Proxy d)

-- | A polymorphic 'Unit' which can be used in place of the coherent
-- SI base unit of any dimension. This allows polymorphic quantity
-- creation and destruction without exposing the 'Dimensional' constructor.
siUnit :: forall d a.(KnownDimension d, Num a) => Unit 'NonMetric d a
siUnit = Unit (baseUnitName $ dimension (Proxy :: Proxy d)) 1 1

instance NFData a => NFData (Quantity d a) -- instance is derived from Generic instance

instance Storable a => Storable (SQuantity s d a) where
  sizeOf _ = sizeOf (undefined::a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::a)
  {-# INLINE alignment #-}
  poke ptr = poke (castPtr ptr :: Ptr a) . coerce
  {-# INLINE poke #-}
  peek ptr = liftM Quantity (peek (castPtr ptr :: Ptr a))
  {-# INLINE peek #-}

{-
Instances for vectors of quantities.
-}
newtype instance U.Vector (SQuantity s d a)    =  V_Quantity {unVQ :: U.Vector a}
newtype instance U.MVector v (SQuantity s d a) = MV_Quantity {unMVQ :: U.MVector v a}
instance U.Unbox a => U.Unbox (SQuantity s d a)

instance (M.MVector U.MVector a) => M.MVector U.MVector (SQuantity s d a) where
  basicLength          = M.basicLength . unMVQ
  {-# INLINE basicLength #-}
  basicUnsafeSlice m n = MV_Quantity . M.basicUnsafeSlice m n . unMVQ
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps u v    = M.basicOverlaps (unMVQ u) (unMVQ v)
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew       = liftM MV_Quantity . M.basicUnsafeNew
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeRead v    = liftM Quantity . M.basicUnsafeRead (unMVQ v)
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite v i = M.basicUnsafeWrite (unMVQ v) i . coerce
  {-# INLINE basicUnsafeWrite #-}
#if MIN_VERSION_vector(0,11,0)
  basicInitialize      = M.basicInitialize . unMVQ
  {-# INLINE basicInitialize #-}
#endif

instance (G.Vector U.Vector a) => G.Vector U.Vector (SQuantity s d a) where
  basicUnsafeFreeze    = liftM V_Quantity  . G.basicUnsafeFreeze . unMVQ
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw      = liftM MV_Quantity . G.basicUnsafeThaw   . unVQ
  {-# INLINE basicUnsafeThaw #-}
  basicLength          = G.basicLength . unVQ
  {-# INLINE basicLength #-}
  basicUnsafeSlice m n = V_Quantity . G.basicUnsafeSlice m n . unVQ
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM v  = liftM Quantity . G.basicUnsafeIndexM (unVQ v)
  {-# INLINE basicUnsafeIndexM #-}

{-
We will conclude by providing a reasonable 'Show' instance for
quantities. The SI unit of the quantity is inferred
from its dimension.
-}
-- | Uses non-breaking spaces between the value and the unit, and within the unit name.
instance (KnownDimension d, E.KnownExactPi s, Show a, Real a) => Show (SQuantity s d a) where
  show (Quantity x) | isExactOne s' = show x ++ showName n
                    | otherwise = "Quantity " ++ show x ++ " {- " ++ show q ++ " -}"
    where
      s' = E.exactPiVal (Proxy :: Proxy s)
      s'' = approximateValue s' :: Double
      q = Quantity (realToFrac x P.* s'') :: Quantity d Double
      (Unit n _ _) = siUnit :: Unit 'NonMetric d a

-- | Shows the value of a 'Quantity' expressed in a specified 'Unit' of the same 'Dimension'.
--
-- Uses non-breaking spaces between the value and the unit, and within the unit name.
--
-- >>> putStrLn $ showIn watt $ (37 *~ volt) * (4 *~ ampere)
-- 148.0 W
showIn :: (Show a, Fractional a) => Unit m d a -> Quantity d a -> String
showIn (Unit n _ y) (Quantity x) = show (x / y) ++ (showName . Name.weaken $ n)

showName :: UnitName 'NonMetric -> String
showName n | n == nOne = ""
           | otherwise = "\xA0" ++ show n

-- | Unit names are shown with non-breaking spaces.
instance (Show a) => Show (Unit m d a) where
  show (Unit n e x) = "The unit " ++ show n ++ ", with value " ++ show e ++ " (or " ++ show x ++ ")"

-- Operates on a dimensional value using a unary operation on values, possibly yielding a Unit.
liftD :: (KnownVariant v1, KnownVariant v2) => (ExactPi -> ExactPi) -> (a -> b) -> UnitNameTransformer -> (Dimensional v1 d1 a) -> (Dimensional v2 d2 b)
liftD fe f nt x = let (x', e') = extractValue x
                      n = extractName x
                      n' = (liftA nt) n
                   in injectValue n' (f x', fmap fe e')

-- Operates on a dimensional value using a unary operation on values, yielding a Quantity.
liftQ :: (a -> a) -> SQuantity s1 d1 a -> SQuantity s2 d2 a
liftQ = coerce

-- Combines two dimensional values using a binary operation on values, possibly yielding a Unit.
liftD2 :: (KnownVariant v1, KnownVariant v2, KnownVariant v3) => (ExactPi -> ExactPi -> ExactPi) -> (a -> a -> a) -> UnitNameTransformer2 -> Dimensional v1 d1 a -> Dimensional v2 d2 a -> Dimensional v3 d3 a
liftD2 fe f nt x1 x2 = let (x1', e1') = extractValue x1
                           (x2', e2') = extractValue x2
                           n1 = extractName x1
                           n2 = extractName x2
                           n' = (liftA2 nt) n1 n2
                        in injectValue n' (f x1' x2', fe <$> e1' <*> e2')

-- Combines two dimensional values using a binary operation on values, yielding a Quantity.
liftQ2 :: (a -> a -> a) -> SQuantity s1 d1 a -> SQuantity s2 d2 a -> SQuantity s3 d3 a
liftQ2 = coerce
