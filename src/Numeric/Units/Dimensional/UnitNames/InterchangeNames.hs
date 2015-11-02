{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Numeric.Units.Dimensional.UnitNames.InterchangeNames
(
  InterchangeNameAuthority(..),
  InterchangeName(..),
  HasInterchangeName(..)
)
where

import Data.Data
import GHC.Generics

-- | Represents the authority which issued an interchange name for a unit.
data InterchangeNameAuthority = UCUM -- ^ The interchange name originated with the Unified Code for Units of Measure.
                              | DimensionalLibrary -- ^ The interchange name originated with the dimensional library.
                              | Custom -- ^ The interchange name originated with a user of the dimensional library.
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data InterchangeName = InterchangeName { name :: String, authority :: InterchangeNameAuthority }
  deriving (Eq, Ord, Data, Typeable, Generic)

instance Show InterchangeName where
  show n = name n ++ " (Issued by " ++ show (authority n) ++ ")"

-- | Determines the authority which issued the interchange name of a unit or unit name.
-- For composite units, this is the least-authoritative interchange name of any constituent name.
--
-- Note that the least-authoritative authority is the one sorted as greatest by the 'Ord' instance of 'InterchangeNameAuthority'.
class HasInterchangeName a where
  interchangeName :: a -> InterchangeName

instance HasInterchangeName InterchangeName where
  interchangeName = id
