{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Numeric.Units.Dimensional.UnitNames.InterchangeNames
(
  InterchangeNameAuthority(..),
  InterchangeName(..),
  HasInterchangeName(..)
)
where

import Control.DeepSeq (NFData)
import Data.Data
import GHC.Generics
import Prelude

-- | Represents the authority which issued an interchange name for a unit.
data InterchangeNameAuthority = UCUM -- ^ The interchange name originated with the Unified Code for Units of Measure.
                              | DimensionalLibrary -- ^ The interchange name originated with the dimensional library.
                              | Custom -- ^ The interchange name originated with a user of the dimensional library.
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData InterchangeNameAuthority where -- instance is derived from Generic instance

data InterchangeName = InterchangeName { name :: String, authority :: InterchangeNameAuthority, isAtomic :: Bool }
  deriving (Eq, Ord, Data, Typeable, Generic)

instance NFData InterchangeName where -- instance is derived from Generic instance

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
