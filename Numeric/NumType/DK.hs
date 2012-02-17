{-# LANGUAGE TypeFamilies
           , DataKinds
           , UndecidableInstances
           , GADTs
           , FlexibleInstances
           , ScopedTypeVariables
  #-}

module Numeric.NumType.DK where

import Prelude hiding ((+),(-),(*),(/),negate)
import qualified Prelude

-- Naturals.

data Nat0 = Z | S0 Nat0  -- Natural numbers including 0.
data Nat1 = O | S1 Nat1  -- Natural numbers excluding 0 (traditional).

-- Type synonyms to reduce brackets and whitespace.

type SZ   = S0 Z
type SS n = S0 (S0 n)
type PZ   = P Z
type PSZ  = P (S0 Z)
type PS n = P (S0 n)
type NO   = N O
type NS n = N (S1 n)

type Neg3 = NS (S1 O)
type Neg2 = NS O
type Neg1 = NO
type Zero = PZ
type Pos1 = PSZ
type Pos2 = PS SZ
type Pos3 = PS (SS Z)
type Pos4 = PS (SS SZ)

-- Conversions between the two types of naturals.

type family ToN0 (n::Nat1) :: Nat0
type instance ToN0  O     = S0 Z
type instance ToN0 (S1 n) = S0 (ToN0 n)

type family ToN1 (n::Nat0) :: Nat1
type instance ToN1 SZ     = O
type instance ToN1 (SS n) = S1 (ToN1 (S0 n))

-- Operations on N0.

type family   Add0 (n::Nat0) (m::Nat0) :: Nat0
type instance Add0  Z  n    = n
type instance Add0 (S0 n) m = Add0 n (S0 m)

type family   Mul0 (n::Nat0) (m::Nat0) :: Nat0
type instance Mul0  Z  n    = Z
type instance Mul0 (S0 n) m = Add0 m (Mul0 n m)

type family   Sub0 (n::Nat0) (m::Nat0) :: Nat0
type instance Sub0     n   Z     = n
type instance Sub0 (S0 n) (S0 m) = Sub0 n m

type family   Div0 (n::Nat0) (m::Nat0) :: Nat0
type instance Div0  Z     (S0 n) = Z
type instance Div0 (S0 n) (S0 m) = S0 (Div0 (Sub0 (S0 n) (S0 m)) (S0 m))  -- Oh my!

-- For convenience, operations on N1.

type family   Add1 (n::Nat1) (m::Nat1) :: Nat1
type instance Add1 n m = ToN1 (Add0 (ToN0 n) (ToN0 m))

type family   Mul1 (n::Nat1) (m::Nat1) :: Nat1
type instance Mul1 n m = ToN1 (Mul0 (ToN0 n) (ToN0 m))

type family   Sub1 (n::Nat1) (m::Nat1) :: Nat1
type instance Sub1 n m = ToN1 (Sub0 (ToN0 n) (ToN0 m))

type family   Div1 (n::Nat1) (m::Nat1) :: Nat1
type instance Div1 n m = ToN1 (Div0 (ToN0 n) (ToN0 m))

{-
The integers are formed by the natural numbers (including 0) (0, 1, 2, 3, ...) together with the negatives of the non-zero natural numbers (−1, −2, −3, ...). They are known as Positive and Negative Integers respectively.
-}
data INT = P Nat0 | N Nat1

type family   Negate (i::INT) :: INT
type instance Negate (N n)  = P (ToN0 n)
type instance Negate PZ     = PZ
type instance Negate (PS n) = N (ToN1 (S0 n))

type family   Pred (i::INT) :: INT
type instance Pred (N n)  = NS n
type instance Pred (P Z)  = NO
type instance Pred (PS n) = P n

type family   Succ (i::INT) :: INT
type instance Succ (NS n) = N n
type instance Succ NO     = PZ
type instance Succ (P n)  = PS n

type family   Add (i::INT) (j::INT) :: INT
type instance Add PZ     i = i
type instance Add (PS n) i = Add (Pred (PS n)) (Succ i)
type instance Add (N n)  i = Add (Succ (N  n))  (Pred i)

type family   Sub (i::INT) (j::INT) :: INT
type instance Sub i j = Add i (Negate j)

type family   Mul (i::INT) (j::INT) :: INT
type instance Mul (P n) (P m) = P (Mul0 n m)
type instance Mul (P n) (N m) = Negate (P (Mul0 n (ToN0 m)))
type instance Mul (N n) (P m) = Negate (P (Mul0 (ToN0 n) m))
type instance Mul (N n) (N m) = P (Mul0 (ToN0 n) (ToN0 m))

type family   Div (i::INT) (j::INT) :: INT
type instance Div (P n) (P m) = P (Div0 n m)
type instance Div (P n) (N m) = Negate (P (Div0 n (ToN0 m)))
type instance Div (N n) (P m) = Negate (P (Div0 (ToN0 n) m))
type instance Div (N n) (N m) = P (Div0 (ToN0 n) (ToN0 m))


-- ----------
data INTRep :: INT -> * where
  IZero :: INTRep (P Z)
  Decr  :: INTRep i -> INTRep (Pred i)
  Incr  :: INTRep i -> INTRep (Succ i)

class ToNum i
  where toNum :: Num a => INTRep i -> a
instance ToNum (P Z)
  where toNum = const 0
instance (ToNum (P n)) => ToNum (PS n)
  where toNum = (Prelude.+ 1) . toNum . Decr
instance (ToNum (Negate (N n))) => ToNum (N n)
  where toNum = Prelude.negate . toNum . negate

instance (ToNum n) => Show (INTRep n) where show = (++"#") . show . toNum


(+) :: INTRep i -> INTRep j -> INTRep (Add i j); (+) = undefined
(-) :: INTRep i -> INTRep j -> INTRep (Sub i j); (-) = undefined
(*) :: INTRep i -> INTRep j -> INTRep (Mul i j); (*) = undefined
(/) :: INTRep i -> INTRep j -> INTRep (Div i j); (/) = undefined
negate :: INTRep i -> INTRep (Negate i); negate = undefined

zero  = IZero
neg1 = Decr IZero
neg2 = Decr neg1
neg3 = Decr neg2
neg4 = Decr neg3
neg5 = Decr neg4
neg6 = Decr neg5
pos1 = Incr IZero
pos2 = Incr pos1
pos3 = Incr pos2
pos4 = Incr pos3
pos5 = Incr pos4
pos6 = Incr pos5
