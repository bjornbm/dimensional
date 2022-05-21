{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Numeric.Units.Dimensional.UnitNames.Internal
where

import Control.DeepSeq
import Control.Monad (join)
import Data.Coerce
import Data.Data hiding (Prefix)
import Data.Foldable (toList)
import Data.Ord
import GHC.Generics hiding (Prefix)
import Numeric.Units.Dimensional.Dimensions.TermLevel (Dimension', asList, HasDimension(..))
import Numeric.Units.Dimensional.UnitNames.InterchangeNames hiding (isAtomic)
import qualified Numeric.Units.Dimensional.UnitNames.InterchangeNames as I
import Numeric.Units.Dimensional.Variants (Metricality(..))
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
  -- A weakened name formed by forgetting that it could accept a metric prefix.
  --
  -- Also available is the smart constructor `weaken` which accepts any `UnitName` as input.
  Weaken :: UnitName 'Metric -> UnitName 'NonMetric
  deriving (Typeable)

deriving instance Eq (UnitName m)

-- As it is for a GADT, this instance cannot be derived or use the generic default implementation
instance NFData (UnitName m) where
  rnf n = case n of
    One -> ()
    MetricAtomic a -> rnf a
    Atomic a -> rnf a
    Prefixed p n' -> rnf p `seq` rnf n'
    Product n1 n2 -> rnf n1 `seq` rnf n2
    Quotient n1 n2 -> rnf n1 `seq` rnf n2
    Power n' e -> rnf n' `seq` rnf e
    Grouped n' -> rnf n'
    Weaken n' -> rnf n'

-- | `UnitName`s are shown with non-breaking spaces.
instance Show (UnitName m) where
  show One = "1"
  show (MetricAtomic a) = abbreviation_en a
  show (Atomic a) = abbreviation_en a
  show (Prefixed a n) = abbreviation_en a ++ show n
  show (Product n1 n2) = show n1 ++ "\xA0" ++ show n2
  show (Quotient n1 n2) = show n1 ++ "\xA0/\xA0" ++ show n2
  show (Power x n) = show x ++ "^" ++ show n
  show (Grouped n) = "(" ++ show n ++ ")"
  show (Weaken n) = show n

-- | Converts a 'UnitName' to a 'NameAtom', if possible.
asAtomic :: UnitName m -> Maybe (NameAtom ('UnitAtom m))
asAtomic (MetricAtomic a) = Just a
asAtomic (Atomic a) = Just a
asAtomic (Weaken n) = coerce <$> asAtomic n
asAtomic _ = Nothing

-- | Returns 'True' if the 'UnitName' is atomic.
isAtomic :: UnitName m -> Bool
isAtomic One = True
isAtomic (MetricAtomic _) = True
isAtomic (Atomic _) = True
isAtomic (Prefixed _ _) = True
isAtomic (Grouped _) = True
isAtomic (Weaken n) = isAtomic n
isAtomic _ = False

isAtomicOrProduct :: UnitName m -> Bool
isAtomicOrProduct (Product _ _) = True
isAtomicOrProduct n = isAtomic n

-- | Reduce a 'UnitName' by algebraic simplifications.
reduce :: UnitName m -> UnitName m
reduce One = One
reduce n@(MetricAtomic _) = n
reduce n@(Atomic _) = n
reduce n@(Prefixed _ _) = n
reduce (Product n1 n2) = reduce' (reduce n1 * reduce n2)
reduce (Quotient n1 n2) = reduce' (reduce n1 * reduce n2)
reduce (Power n x) = reduce' (reduce n ^ x)
reduce (Grouped n) = reduce' (Grouped (reduce n))
reduce (Weaken n) = reduce' (Weaken (reduce n))

-- reduce, knowing that subterms are already in reduced form
reduce' :: UnitName m -> UnitName m
reduce' (Product One n) = reduce' n
reduce' (Product n One) = reduce' n
reduce' (Power (Power n x1) x2) = reduce (n ^ (x1 P.* x2))
reduce' (Power (Grouped (Power n x1)) x2) = reduce (n ^ (x1 P.* x2))
reduce' (Power _ 0) = One
reduce' (Power n 1) = reduce' n
reduce' (Grouped n) = reduce' n
reduce' n@(Weaken (MetricAtomic _)) = n
reduce' n = n

data NameAtomType = UnitAtom Metricality
                  | PrefixAtom
  deriving (Eq, Ord, Data, Typeable, Generic)

instance NFData NameAtomType where -- instance is derived from Generic instance

-- | The name of a metric prefix.
type PrefixName = NameAtom 'PrefixAtom

-- | A metric prefix.
data Prefix = Prefix
              {
                -- | The name of a metric prefix.
                prefixName :: PrefixName,
                -- | The scale factor denoted by a metric prefix.
                scaleFactor :: Rational
              }
  deriving (Eq, Data, Typeable, Generic)

instance Ord Prefix where
  compare = comparing scaleFactor

instance NFData Prefix where -- instance is derived from Generic instance

instance HasInterchangeName Prefix where
  interchangeName = interchangeName . prefixName

-- | The name of the unit of dimensionless values.
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

-- | The name of the base unit associated with a specified dimension.
baseUnitName :: Dimension' -> UnitName 'NonMetric
baseUnitName d = let powers = asList $ dimension d
                  in reduce . product $ zipWith (^) baseUnitNames powers

baseUnitNames :: [UnitName 'NonMetric]
baseUnitNames = [weaken nMeter, nKilogram, weaken nSecond, weaken nAmpere, weaken nKelvin, weaken nMole, weaken nCandela]

deca, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta :: Prefix
deca  = prefix "da" "da" "deca" 1e1
hecto = prefix "h" "h" "hecto"  1e2
kilo  = prefix "k" "k" "kilo"   1e3
mega  = prefix "M" "M" "mega"   1e6
giga  = prefix "G" "G" "giga"   1e9
tera  = prefix "T" "T" "tera"   1e12
peta  = prefix "P" "P" "peta"   1e15
exa   = prefix "E" "E" "exa"    1e18
zetta = prefix "Z" "Z" "zetta"  1e21
yotta = prefix "Y" "Y" "yotta"  1e24
deci, centi, milli, micro, nano, pico, femto, atto, zepto, yocto :: Prefix
deci  = prefix "d" "d" "deci"   1e-1
centi = prefix "c" "c" "centi"  1e-2
milli = prefix "m" "m" "milli"  1e-3
micro = prefix "u" "μ" "micro"  1e-6
nano  = prefix "n" "n" "nano"   1e-9
pico  = prefix "p" "p" "pico"   1e-12
femto = prefix "f" "f" "femto"  1e-15
atto  = prefix "a" "a" "atto"   1e-18
zepto = prefix "z" "z" "zepto"  1e-21
yocto = prefix "y" "y" "yocto"  1e-24

-- | A list of all 'Prefix'es defined by the SI.
siPrefixes :: [Prefix]
siPrefixes = [yocto, zepto, atto, femto, pico, nano, micro, milli, centi, deci, deca, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta]

-- | Forms a 'UnitName' from a 'Metric' name by applying a metric prefix.
applyPrefix :: Prefix -> UnitName 'Metric -> UnitName 'NonMetric
applyPrefix = Prefixed . prefixName

{-
We will reuse the operators and function names from the Prelude.
To prevent unpleasant surprises we give operators the same fixity
as the Prelude.
-}

infixr 8  ^
infixl 7  *, /

-- | Form a 'UnitName' by taking the product of two others.
(*) :: UnitName m1 -> UnitName m2 -> UnitName 'NonMetric
a * b = Product (weaken a) (weaken b)

-- | Form a 'UnitName' by dividing one by another.
(/) :: UnitName m1 -> UnitName m2 -> UnitName 'NonMetric
n1 / n2 | isAtomicOrProduct n1 = Quotient (weaken n1) (weaken n2)
        | otherwise            = Quotient (grouped n1) (weaken n2)

-- | Form a 'UnitName' by raising a name to an integer power.
(^) :: UnitName m -> Int -> UnitName 'NonMetric
x ^ n | isAtomic x = Power (weaken x) n
      | otherwise  = Power (grouped x) n

-- | Convert a 'UnitName' which may or may not be 'Metric' to one
-- which is certainly 'NonMetric'.
weaken :: UnitName m -> UnitName 'NonMetric
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
strengthen :: UnitName m -> Maybe (UnitName 'Metric)
strengthen n@(MetricAtomic _) = Just n
strengthen (Weaken n) = strengthen n
strengthen _ = Nothing

-- | Convert a 'UnitName' of one 'Metricality' into a name of another metricality by
-- strengthening or weakening if neccessary. Because it may not be possible to strengthen,
-- the result is returned in a 'Maybe' wrapper.
relax :: forall m1 m2.(Typeable m1, Typeable m2) => UnitName m1 -> Maybe (UnitName m2)
relax = go (typeRep (Proxy :: Proxy m1)) (typeRep (Proxy :: Proxy m2))
  where
    metric = typeRep (Proxy :: Proxy 'Metric)
    nonMetric = typeRep (Proxy :: Proxy 'NonMetric)
    go :: TypeRep -> TypeRep -> UnitName m1 -> Maybe (UnitName m2)
    go p1 p2 | p1 == p2 = cast
             | (p1 == nonMetric) && (p2 == metric) = join . fmap gcast . strengthen
             | (p1 == metric) && (p2 == nonMetric) = cast . weaken
             | otherwise = error "Should be unreachable. TypeRep of an unexpected Metricality encountered."

-- | Constructs a 'UnitName' by applying a grouping operation to
-- another 'UnitName', which may be useful to express precedence.
grouped :: UnitName m -> UnitName 'NonMetric
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

instance NFData (NameAtom m) where -- instance is derived from Generic instance

instance HasInterchangeName (NameAtom m) where
  interchangeName = _interchangeName

instance HasInterchangeName (UnitName m) where
  interchangeName One = InterchangeName { name = "1", authority = UCUM, I.isAtomic = True }
  interchangeName (MetricAtomic a) = interchangeName a
  interchangeName (Atomic a) = interchangeName a
  interchangeName (Prefixed p n) = let n' = (name . interchangeName $ p) ++ (name . interchangeName $ n)
                                       a' = max (authority . interchangeName $ p) (authority . interchangeName $ n)
                                    in InterchangeName { name = n', authority = a', I.isAtomic = False }
  interchangeName (Product n1 n2) = let n' = (name . interchangeName $ n1) ++ "." ++ (name . interchangeName $ n2)
                                        a' = max (authority . interchangeName $ n1) (authority . interchangeName $ n2)
                                     in InterchangeName { name = n', authority = a', I.isAtomic = False }
  interchangeName (Quotient n1 n2) = let n' = (name . interchangeName $ n1) ++ "/" ++ (name . interchangeName $ n2)
                                         a' = max (authority . interchangeName $ n1) (authority . interchangeName $ n2)
                                      in InterchangeName { name = n', authority = a', I.isAtomic = False }
  -- TODO #109: note in this case that the UCUM is changing their grammar to not accept exponents after
  -- as a result it will become necessary to distribute the exponentiation over the items in the base name
  -- prior to generating the interchange name
  interchangeName (Power n x) = let n' = (name . interchangeName $ n) ++ show x
                                 in InterchangeName { name = n', authority = authority . interchangeName $ n, I.isAtomic = False }
  interchangeName (Grouped n) = let n' = "(" ++ (name . interchangeName $ n) ++ ")"
                                 in InterchangeName { name = n', authority = authority . interchangeName $ n, I.isAtomic = False }
  interchangeName (Weaken n) = interchangeName n

prefix :: String -> String -> String -> Rational -> Prefix
prefix i a f = Prefix n
  where
    n = NameAtom (InterchangeName i UCUM True) a f

ucumMetric :: String -> String -> String -> UnitName 'Metric
ucumMetric i a f = MetricAtomic $ NameAtom (InterchangeName i UCUM True) a f

ucum :: String -> String -> String -> UnitName 'NonMetric
ucum i a f = Atomic $ NameAtom (InterchangeName i UCUM True) a f

dimensionalAtom :: String -> String -> String -> UnitName 'NonMetric
dimensionalAtom i a f = Atomic $ NameAtom (InterchangeName i DimensionalLibrary True) a f

-- | Constructs an atomic name for a custom unit.
atom :: String -- ^ Interchange name
     -> String -- ^ Abbreviated name in international English
     -> String -- ^ Full name in international English
     -> UnitName 'NonMetric
atom i a f = Atomic $ NameAtom (InterchangeName i Custom True) a f

-- | The type of a unit name transformation that may be associated with an operation that takes a single unit as input.
type UnitNameTransformer = (forall m.UnitName m -> UnitName 'NonMetric)

-- | The type of a unit name transformation that may be associated with an operation that takes two units as input.
type UnitNameTransformer2 = (forall m1 m2.UnitName m1 -> UnitName m2 -> UnitName 'NonMetric)

-- | Forms the product of a list of 'UnitName's.
--
-- If you wish to form a heterogenous product of 'Metric' and 'NonMetric' units
-- you should apply 'weaken' to the 'Metric' ones.
product :: Foldable f => f (UnitName 'NonMetric) -> UnitName 'NonMetric
product = go . toList
  where
    -- This is not defined using a simple fold so that it does not complicate the product with
    -- valid but meaningless occurences of nOne.
    go :: [UnitName 'NonMetric] -> UnitName 'NonMetric
    go [] = nOne
    go [n] = n
    go (n : ns) = n * go ns
