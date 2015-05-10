{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Numeric.Units.Dimensional.DK.UnitNames.Internal
where

-- | The name of a unit.
data UnitName (m :: Metricality) where
  -- | The name of the unit of dimensionless values.
  One :: UnitName 'NonMetric
  -- | A name of an atomic unit to which metric prefixes may be applied.
  MetricAtomic :: NameAtom ('UnitAtom 'Metric) -> UnitName 'Metric
  -- | A name of an atomic unit to which metric prefixes may not be applied.
  Atomic :: NameAtom ('UnitAtom 'NonMetric) -> UnitName 'NonMetric
  -- | A name of a prefixed unit.
  Prefixed :: NameAtom 'PrefixAtom -> UnitName 'Metric -> UnitName 'NonMetric
  -- | A compound name formed from the product of two names.
  Product :: UnitName a -> UnitName b -> UnitName 'NonMetric
  -- | A compound name formed from the quotient of two names.
  Quotient :: UnitName a -> UnitName b -> UnitName 'NonMetric
  -- | A compound name formed by raising a unit name to an integer power.
  Power :: UnitName a -> Int -> UnitName 'NonMetric
  -- | A compound name formed by grouping another name, which is generally compound.
  Grouped :: UnitName a -> UnitName 'NonMetric

data Metricality = Metric
                 | NonMetric

data NameAtomType = UnitAtom Metricality
                  | PrefixAtom

one :: UnitName 'NonMetric
one = One

applyPrefix :: NameAtom 'PrefixAtom -> UnitName 'Metric -> UnitName 'NonMetric
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
(/) = Quotient

(^) :: UnitName a -> Int -> UnitName 'NonMetric
(^) = Power

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

prefix :: String -> String -> String -> NameAtom 'PrefixAtom
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
