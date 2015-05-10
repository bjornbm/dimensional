{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Numeric.Units.Dimensional.DK.UnitNames.Internal
where

import Control.Applicative
import Data.Dynamic
import Data.Foldable (toList)
import Numeric.Units.Dimensional.DK.Variants (Metricality(..))
import Prelude hiding ((*), (/), (^), product)
import qualified Prelude as P

-- | The name of a unit.
data UnitName (m :: Metricality) where
  -- The name of the unit of dimensionless values.
  One :: UnitName 'NonMetric
  -- A name of an atomic unit to which metric prefixes may be applied.
  MetricAtomic :: NameAtom ('UnitAtom 'Metric) -> UnitName 'Metric
  -- A name of an atomic unit to which metric prefixes may not be applied.
  Atomic :: NameAtom ('UnitAtom 'NonMetric) -> UnitName 'NonMetric
  -- A name of a prefixed unit.
  Prefixed :: PrefixName -> UnitName 'Metric -> UnitName 'NonMetric
  -- A compound name formed from the product of two names.
  Product :: UnitName a -> UnitName b -> UnitName 'NonMetric
  -- A compound name formed from the quotient of two names.
  Quotient :: UnitName a -> UnitName b -> UnitName 'NonMetric
  -- A compound name formed by raising a unit name to an integer power.
  Power :: UnitName a -> Int -> UnitName 'NonMetric
  -- A compound name formed by grouping another name, which is generally compound.
  Grouped :: UnitName a -> UnitName 'NonMetric
  -- A weakened name formed by forgetting whether it could accept a metric prefix.
  -- Differs from 'Grouped' because it is displayed without parentheses.
  Weaken :: UnitName a -> UnitName 'NonMetric
  deriving (Typeable)

instance Show (UnitName m) where
  show One = "1"
  show (MetricAtomic a) = abbreviation_en a
  show (Atomic a) = abbreviation_en a
  show (Prefixed a n) = abbreviation_en a ++ show n
  show (Product n1 n2) = show n1 ++ " " ++ show n2
  show (Quotient n1 n2) = show n1 ++ " / " ++ show n2
  show (Power x n) = show x ++ "^" ++ show n
  show (Grouped n) = "(" ++ show n ++ ")"
  show (Weaken n) = show n

isAtomic :: UnitName m -> Bool
isAtomic (One) = True
isAtomic (MetricAtomic _) = True
isAtomic (Atomic _) = True
isAtomic (Prefixed _ _) = True
isAtomic (Grouped _) = True
isAtomic (Weaken n) = isAtomic n
isAtomic _ = False

isAtomicOrProduct :: UnitName m -> Bool
isAtomicOrProduct (Product _ _) = True
isAtomicOrProduct n = isAtomic n

-- reduce by algebraic simplifications
reduce :: UnitName m -> UnitName m
reduce (One) = One
reduce n@(MetricAtomic _) = n
reduce n@(Atomic _) = n
reduce n@(Prefixed _ _) = n
reduce (Product n1 n2) = reduce' (reduce n1 * reduce n2)
reduce (Quotient n1 n2) = reduce' (reduce n1 * reduce n2)
reduce (Power n x) = reduce' ((reduce n) ^ x)
reduce (Grouped n) = reduce' (Grouped (reduce n))
reduce (Weaken (Weaken n)) = reduce' (Weaken (reduce n))
reduce (Weaken n) = reduce' (Weaken (reduce n))

-- reduce, knowing that subterms are already in reduced form
reduce' :: UnitName m -> UnitName m
reduce' (Product One n) = reduce' $ weaken n
reduce' (Product n One) = reduce' $ weaken n
reduce' (Power (Power n x1) x2) = reduce (n ^ (x1 P.* x2))
reduce' (Power (Grouped (Power n x1)) x2) = reduce (n ^ (x1 P.* x2))
reduce' (Power _ 0) = One
reduce' (Power n 1) = reduce' $ weaken n
reduce' (Grouped n@(MetricAtomic _)) = reduce' $ weaken n
reduce' (Grouped n) = reduce' $ weaken n
reduce' (Weaken One) = One
reduce' n@(Weaken (MetricAtomic _)) = n
reduce' (Weaken n@(Atomic _)) = n
reduce' (Weaken n@(Prefixed _ _)) = n
reduce' (Weaken n@(Product _ _)) = n
reduce' (Weaken n@(Quotient _ _)) = n
reduce' (Weaken n@(Power _ _)) = n
reduce' (Weaken (Grouped n)) = reduce' $ weaken n
reduce' (Weaken n@(Weaken _)) = reduce' n
reduce' n = n

reduceOuterGroupsOnly :: UnitName m -> UnitName m
reduceOuterGroupsOnly (Grouped n) = reduceOuterGroupsOnly (Weaken n)
reduceOuterGroupsOnly n = n

data AnyUnitName where
  AnyUnitName :: (Typeable (UnitName m)) => UnitName m -> AnyUnitName

data NameAtomType = UnitAtom Metricality
                  | PrefixAtom
  deriving (Eq, Ord, Typeable)                  

type PrefixName = NameAtom 'PrefixAtom

nOne :: UnitName 'NonMetric
nOne = One

nMeter :: UnitName 'Metric
nMeter = ucumMetric "m" "m" "metre"

nGram :: UnitName 'Metric
nGram = ucumMetric "g" "g" "gram"

nKilogram :: UnitName 'NonMetric
nKilogram = applyPrefix kilo nGram

nSecond :: UnitName 'Metric
nSecond = ucumMetric "s" "s" "second"

nAmpere :: UnitName 'Metric
nAmpere = ucumMetric "A" "A" "Ampere"

nKelvin :: UnitName 'Metric
nKelvin = ucumMetric "K" "K" "Kelvin"

nMole :: UnitName 'Metric
nMole = ucumMetric "mol" "mol" "mole"

nCandela :: UnitName 'Metric
nCandela = ucumMetric "cd" "cd" "candela"

baseUnitNames :: [AnyUnitName]
baseUnitNames = [AnyUnitName nMeter, AnyUnitName nKilogram, AnyUnitName nSecond, AnyUnitName nAmpere, AnyUnitName nKelvin, AnyUnitName nMole, AnyUnitName nCandela]

deka, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta :: PrefixName
deka  = prefix "da" "da" "deka"
hecto = prefix "h" "h" "hecto"
kilo  = prefix "k" "k" "kilo"
mega  = prefix "M" "M" "mega"
giga  = prefix "G" "G" "giga"
tera  = prefix "T" "T" "tera"
peta  = prefix "P" "P" "peta"
exa   = prefix "E" "E" "exa"
zetta = prefix "Z" "Z" "zetta"
yotta = prefix "Y" "Y" "yotta"
deci, centi, milli, micro, nano, pico, femto, atto, zepto, yocto :: PrefixName
deci  = prefix "d" "d" "deci"
centi = prefix "c" "c" "centi"
milli = prefix "m" "m" "milli"
micro = prefix "u" "μ" "micro"
nano  = prefix "n" "n" "nano"
pico  = prefix "p" "p" "pico"
femto = prefix "f" "f" "femto"
atto  = prefix "a" "a" "atto"
zepto = prefix "z" "z" "zepto"
yocto = prefix "y" "y" "yocto"

applyPrefix :: PrefixName -> UnitName 'Metric -> UnitName 'NonMetric
applyPrefix = Prefixed

{-
We will reuse the operators and function names from the Prelude.
To prevent unpleasant surprises we give operators the same fixity
as the Prelude.
-}

infixr 8  ^
infixl 7  *, /

(*) :: UnitName a -> UnitName b -> UnitName 'NonMetric
(*) = Product

(/) :: UnitName a -> UnitName b -> UnitName 'NonMetric
n1 / n2 | isAtomicOrProduct n1 = Quotient n1 n2
        | otherwise            = Quotient (Grouped n1) n2

(^) :: UnitName a -> Int -> UnitName 'NonMetric
x ^ n | isAtomic x = Power x n
      | otherwise  = Power (Grouped x) n

weaken :: UnitName a -> UnitName 'NonMetric
weaken One = One
weaken n@(MetricAtomic _) = Weaken n
weaken n@(Atomic _) = n
weaken n@(Prefixed _ _) = n
weaken n@(Product _ _) = n
weaken n@(Quotient _ _) = n
weaken n@(Power _ _) = n
weaken n@(Grouped _) = n
weaken n@(Weaken _) = n

grouped :: UnitName a -> UnitName 'NonMetric
grouped = Grouped

-- | Represents the name of an atomic unit or prefix.
data NameAtom (m :: NameAtomType)
  = NameAtom 
  { 
    interchangeNameAuthority :: InterchangeNameAuthority, -- ^ The authortity which issued the interchange name for the unit.
    interchangeName :: String, -- ^ The interchange name of the unit.
    abbreviation_en :: String, -- ^ The abbreviated name of the unit in international English
    name_en :: String -- ^ The full name of the unit in international English
  }
  deriving (Eq, Ord, Typeable)

prefix :: String -> String -> String -> PrefixName
prefix i a f = NameAtom UCUM i a f

ucumMetric :: String -> String -> String -> UnitName 'Metric
ucumMetric i a f = MetricAtomic $ NameAtom UCUM i a f

ucum :: String -> String -> String -> UnitName 'NonMetric
ucum i a f = Atomic $ NameAtom UCUM i a f

dimensionalAtom :: String -> String -> String -> UnitName 'NonMetric
dimensionalAtom i a f = Atomic $ NameAtom DimensionalLibrary i a f

-- | Constructs an atomic name for a custom unit.
atom :: String -- ^ Interchange name
     -> String -- ^ Abbreviated name in international English
     -> String -- ^ Full name in international English
     -> UnitName 'NonMetric
atom i a f = Atomic $ NameAtom Custom i a f

-- | Represents the authority which issued an interchange name for a unit.
data InterchangeNameAuthority = UCUM -- ^ The interchange name originated with the Unified Code for Units of Measure.
                              | DimensionalLibrary -- ^ The interchange name originated with the dimensional-dk library.
                              | Custom -- ^ The interchange name originated with a user of the dimensional-dk library.
  deriving (Eq, Ord, Typeable)

-- | The type of a unit name transformation that may be associated with an operation that takes a single unit as input.
type UnitNameTransformer = Maybe AnyUnitName -> Maybe AnyUnitName

-- | The type of a unit name transformation that may be associated with an operation that takes two units as input.
type UnitNameTransformer2 = Maybe AnyUnitName -> Maybe AnyUnitName -> Maybe AnyUnitName

liftTransformer2 :: (forall m1 m2.UnitName m1 -> UnitName m2 -> UnitName 'NonMetric) -> AnyUnitName -> AnyUnitName -> UnitName 'NonMetric
liftTransformer2 f (AnyUnitName n1) (AnyUnitName n2) = f n1 n2

liftTransformer :: (forall m1.UnitName m1 -> UnitName 'NonMetric) -> AnyUnitName -> UnitName 'NonMetric
liftTransformer f (AnyUnitName n) = f n

product :: AnyUnitName -> AnyUnitName -> UnitName 'NonMetric
product = liftTransformer2 (*)

quotient :: AnyUnitName -> AnyUnitName -> UnitName 'NonMetric
quotient = liftTransformer2 (/)

power :: Int -> AnyUnitName -> UnitName 'NonMetric
power n = liftTransformer (^ n)

powerExcept0 :: Int -> AnyUnitName -> Maybe (UnitName 'NonMetric)
powerExcept0 0 _ = Nothing
powerExcept0 n x = Just $ power n x

product' :: UnitNameTransformer2
product' = liftA2 $ (AnyUnitName .) . product

quotient' :: UnitNameTransformer2
quotient' = liftA2 $ (AnyUnitName .) . quotient

power' :: Int -> UnitNameTransformer
power' n = liftA $ AnyUnitName . power n

nAryProduct :: Foldable f => f AnyUnitName -> UnitName 'NonMetric
nAryProduct = go . toList
  where
    go :: [AnyUnitName] -> UnitName 'NonMetric
    go [] = nOne
    go [AnyUnitName n1] = Weaken n1
    go (n1 : n2 : []) = product n1 n2
    go (n : ns) = product n (AnyUnitName . go $ ns)

nAryProductOfPowers :: (Functor f, Foldable f) => f (AnyUnitName, Int) -> UnitName 'NonMetric
nAryProductOfPowers xs = nAryProduct $ fmap (AnyUnitName . uncurry (flip power)) xs
