{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Numeric.Units.Dimensional.DK.UnitNames.Internal
where

import Control.Monad (join)
import Data.Data
#if MIN_VERSION_base(4, 8, 0)
import Data.Foldable (toList)
#else
import Data.Foldable (Foldable, toList)
#endif
import GHC.Generics
import Numeric.Units.Dimensional.DK.Dimensions.TermLevel (Dimension', asList, HasDimension(..))
import Numeric.Units.Dimensional.DK.UnitNames.InterchangeNames
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
  Product :: UnitName 'NonMetric -> UnitName 'NonMetric -> UnitName 'NonMetric
  -- A compound name formed from the quotient of two names.
  Quotient :: UnitName 'NonMetric -> UnitName 'NonMetric -> UnitName 'NonMetric
  -- A compound name formed by raising a unit name to an integer power.
  Power :: UnitName 'NonMetric -> Int -> UnitName 'NonMetric
  -- A compound name formed by grouping another name, which is generally compound.
  Grouped :: UnitName 'NonMetric -> UnitName 'NonMetric
  -- A weakened name formed by forgetting whether it could accept a metric prefix.
  -- Differs from 'Grouped' because it is displayed without parentheses.
  Weaken :: UnitName 'Metric -> UnitName 'NonMetric
  deriving (Typeable)

deriving instance Eq (UnitName m)

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
reduce (Weaken n) = reduce' (Weaken (reduce n))

-- reduce, knowing that subterms are already in reduced form
reduce' :: UnitName m -> UnitName m
reduce' (Product One n) = reduce' $ weaken n
reduce' (Product n One) = reduce' $ weaken n
reduce' (Power (Power n x1) x2) = reduce (n ^ (x1 P.* x2))
reduce' (Power (Grouped (Power n x1)) x2) = reduce (n ^ (x1 P.* x2))
reduce' (Power _ 0) = One
reduce' (Power n 1) = reduce' $ weaken n
reduce' (Grouped n) = reduce' $ weaken n
reduce' n@(Weaken (MetricAtomic _)) = n
reduce' n = n

data NameAtomType = UnitAtom Metricality
                  | PrefixAtom
  deriving (Eq, Ord, Data, Typeable, Generic)

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

baseUnitName :: Dimension' -> UnitName 'NonMetric
baseUnitName d = let powers = asList $ dimension d
                  in reduce . product $ zipWith (^) baseUnitNames powers

baseUnitNames :: [UnitName 'NonMetric]
baseUnitNames = [weaken nMeter, nKilogram, weaken nSecond, weaken nAmpere, weaken nKelvin, weaken nMole, weaken nCandela]

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
a * b = Product (weaken a) (weaken b)

(/) :: UnitName a -> UnitName b -> UnitName 'NonMetric
n1 / n2 | isAtomicOrProduct n1 = Quotient (weaken n1) (weaken n2)
        | otherwise            = Quotient (grouped n1) (weaken n2)

(^) :: UnitName a -> Int -> UnitName 'NonMetric
x ^ n | isAtomic x = Power (weaken x) n
      | otherwise  = Power (grouped x) n

-- | Convert a 'UnitName' which may or may not be 'Metric' to one
-- which is certainly 'NonMetric'.
weaken :: UnitName a -> UnitName 'NonMetric
weaken n@(MetricAtomic _) = Weaken n -- we really only need this one case and a catchall, but the typechecker can't see it
weaken n@One = n
weaken n@(Atomic _) = n
weaken n@(Prefixed _ _) = n
weaken n@(Product _ _) = n
weaken n@(Quotient _ _) = n
weaken n@(Power _ _) = n
weaken n@(Grouped _) = n
weaken n@(Weaken _) = n

-- | Attempt to convert a 'UnitName' which may or may not be 'Metric' to one
-- which is certainly 'Metric'.
strengthen :: UnitName a -> Maybe (UnitName 'Metric)
strengthen n@(MetricAtomic _) = Just n
strengthen (Weaken n) = strengthen n
strengthen _ = Nothing

-- | Convert a 'UnitName' of one 'Metricality' into a name of the other metricality by
-- strengthening or weakening if neccessary. Because it may not be possible to strengthen,
-- the result is returned in a 'Maybe' wrapper.
relax :: forall m1 m2.(Typeable m1, Typeable m2) => UnitName m1 -> Maybe (UnitName m2)
relax n = go (typeRep (Proxy :: Proxy m1)) (typeRep (Proxy :: Proxy m2)) n
  where
    metric = typeRep (Proxy :: Proxy 'Metric)
    nonMetric = typeRep (Proxy :: Proxy 'NonMetric)
    go :: TypeRep -> TypeRep -> UnitName m1 -> Maybe (UnitName m2)
    go p1 p2 | p1 == p2 = cast
             | (p1 == nonMetric) && (p2 == metric) = join . fmap gcast . strengthen
             | (p1 == metric) && (p2 == nonMetric) = cast . weaken
             | otherwise = error "Should be unreachable. TypeRep of an unexpected Metricality encountered."


grouped :: UnitName a -> UnitName 'NonMetric
grouped = Grouped . weaken

-- | Represents the name of an atomic unit or prefix.
data NameAtom (m :: NameAtomType)
  = NameAtom 
  {
    _interchangeName :: InterchangeName, -- ^ The interchange name of the unit.
    abbreviation_en :: String, -- ^ The abbreviated name of the unit in international English
    name_en :: String -- ^ The full name of the unit in international English
  }
  deriving (Eq, Ord, Data, Typeable, Generic)

instance HasInterchangeName (NameAtom m) where
  interchangeName = _interchangeName

instance HasInterchangeName (UnitName m) where
  interchangeName One = InterchangeName { name = "1", authority = UCUM }
  interchangeName (MetricAtomic a) = interchangeName a
  interchangeName (Atomic a) = interchangeName a
  interchangeName (Prefixed p n) = let n' = (name . interchangeName $ p) ++ (name . interchangeName $ n)
                                       a' = max (authority . interchangeName $ p) (authority . interchangeName $ n)
                                    in InterchangeName { name = n', authority = a' }
  interchangeName (Product n1 n2) = let n' = (name . interchangeName $ n1) ++ "." ++ (name . interchangeName $ n2)
                                        a' = max (authority . interchangeName $ n1) (authority . interchangeName $ n2)
                                     in InterchangeName { name = n', authority = a' }
  interchangeName (Quotient n1 n2) = let n' = (name . interchangeName $ n1) ++ "/" ++ (name . interchangeName $ n2)
                                         a' = max (authority . interchangeName $ n1) (authority . interchangeName $ n2)
                                      in InterchangeName { name = n', authority = a' }
  interchangeName (Power n x) = let n' = (name . interchangeName $ n) ++ (show x)
                                 in InterchangeName { name = n', authority = authority . interchangeName $ n }
  interchangeName (Grouped n) = let n' = "(" ++ (name . interchangeName $ n) ++ ")"
                                 in InterchangeName { name = n', authority = authority . interchangeName $ n }
  interchangeName (Weaken n) = interchangeName n

prefix :: String -> String -> String -> PrefixName
prefix i a f = NameAtom (InterchangeName i UCUM) a f

ucumMetric :: String -> String -> String -> UnitName 'Metric
ucumMetric i a f = MetricAtomic $ NameAtom (InterchangeName i UCUM) a f

ucum :: String -> String -> String -> UnitName 'NonMetric
ucum i a f = Atomic $ NameAtom (InterchangeName i UCUM) a f

dimensionalAtom :: String -> String -> String -> UnitName 'NonMetric
dimensionalAtom i a f = Atomic $ NameAtom (InterchangeName i DimensionalLibrary) a f

-- | Constructs an atomic name for a custom unit.
atom :: String -- ^ Interchange name
     -> String -- ^ Abbreviated name in international English
     -> String -- ^ Full name in international English
     -> UnitName 'NonMetric
atom i a f = Atomic $ NameAtom (InterchangeName i Custom) a f

-- | The type of a unit name transformation that may be associated with an operation that takes a single unit as input.
type UnitNameTransformer = (forall m.UnitName m -> UnitName 'NonMetric)

-- | The type of a unit name transformation that may be associated with an operation that takes two units as input.
type UnitNameTransformer2 = (forall m1 m2.UnitName m1 -> UnitName m2 -> UnitName 'NonMetric)

product :: Foldable f => f (UnitName 'NonMetric) -> UnitName 'NonMetric
product = go . toList
  where
    -- This is not defined using a simple fold so that it does not complicate the product with
    -- valid but meaningless occurences of nOne.
    go :: [UnitName 'NonMetric] -> UnitName 'NonMetric
    go [] = nOne
    go [n] = n
    go (n : ns) = n * go ns
