{-# OPTIONS_HADDOCK not-home, show-extensions #-}
module Numeric.Units.Dimensional.DK.Dimensions.TermLevel
(
  Dimension'(..),
  HasDimension(..),
  asList
)
where

-- | A physical dimension, encoded as 7 integers, representing a factorization of the dimension into the
-- 7 SI base dimensions. By convention they are stored in the same order as 
-- in the 'Numeric.Units.Dimensional.DK.Dimensions.TypeLevel.Dimension' data kind.
data Dimension' = Dim' !Int !Int !Int !Int !Int !Int !Int 
  deriving (Show, Eq, Ord)

-- | Dimensional values inhabit this class, which allows access to a term-level representation of their dimension.
class HasDimension a where 
  -- | Obtains a term-level representation of a value's dimension.
  dimension :: a -> Dimension'

instance HasDimension Dimension' where
  dimension = id

-- | Converts a dimension to a list of 7 integers, representing the exponent associated with each
-- of the 7 SI base dimensions in the standard order.
asList :: Dimension' -> [Int]
asList (Dim' l m t i th n j) = [l, m, t, i, th, n, j]
