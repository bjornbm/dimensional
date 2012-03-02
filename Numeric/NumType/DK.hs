{-# LANGUAGE TypeFamilies
           , DataKinds
           , UndecidableInstances
           , GADTs
           , FlexibleInstances
           , ScopedTypeVariables
           , MultiParamTypeClasses
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
{-
class (ToN0 n1 ~ n0, ToN1 n0 ~ n1) => ToN0C (n1::Nat1) (n0::Nat0)
  where
    type ToN0 (n1::Nat1) :: Nat0

instance ToN0C O (S0 Z) where type ToN0 O = S0 Z
instance (ToN0C n1 n0, ToN1C (S0 n0) (S1 n1)) => ToN0C (S1 n1) (S0 n0)
  where
    type ToN0 (S1 n1) = S0 (ToN0 n1)

class (ToN1 n0 ~ n1, ToN0 n1 ~ n0) => ToN1C (n0::Nat0) (n1::Nat1)
  where
    type ToN1 (n0::Nat0) :: Nat1
instance ToN1C SZ O where type ToN1 SZ     = O
instance (ToN1C (S0 n0) n1) => ToN1C (SS n0) (S1 n1)
  where
    type ToN1 (SS n0) = S1 (ToN1 (S0 n0))
-}
type family   ToN0 (n1::Nat1) :: Nat0
type instance ToN0 O       = S0 Z
type instance ToN0 (S1 n1) = S0 (ToN0 n1)
type family   ToN1 (n0::Nat0) :: Nat1
type instance ToN1 SZ      = O
type instance ToN1 (SS n0) = S1 (ToN1 (S0 n0))

-- Operations on N0.

type family   Add0 (n::Nat0) (m::Nat0) :: Nat0
type instance Add0  Z  n    = n
type instance Add0 (S0 n) m = Add0 n (S0 m)

type family   Sub0 (n::Nat0) (m::Nat0) :: Nat0
type instance Sub0     n   Z     = n
type instance Sub0 (S0 n) (S0 m) = Sub0 n m

type family   Mul0 (n::Nat0) (m::Nat0) :: Nat0
type instance Mul0  Z  n    = Z
type instance Mul0 (S0 n) m = Add0 m (Mul0 n m)

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


-- Classes

class    (ToN0 n1 ~ n0, ToN1 n0 ~ n1) => NatConv n0 n1
instance (ToN0 n1 ~ n0, ToN1 n0 ~ n1) => NatConv n0 n1

class    (Add0 n m ~ l, Add0 m n ~ l, Sub0 l n ~ m, Sub0 l m ~ n) => Sum0 n m l
instance (Add0 n m ~ l, Add0 m n ~ l, Sub0 l n ~ m, Sub0 l m ~ n) => Sum0 n m l

class    (Add1 n m ~ l, Add1 m n ~ l, Sub1 l n ~ m, Sub1 l m ~ n) => Sum1 n m l
instance (Add1 n m ~ l, Add1 m n ~ l, Sub1 l n ~ m, Sub1 l m ~ n) => Sum1 n m l


{-
The integers are formed by the natural numbers (including 0) (0, 1, 2, 3, ...) together with the negatives of the non-zero natural numbers (−1, −2, −3, ...). They are known as Positive and Negative Integers respectively.
-}
data INT = P Nat0 | N Nat1

--instance NegateC (N n) (P (ToN0 n)) where
  --type Negates (N n) = P (ToN0 n)

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


class (Negate x ~ y, Negate y ~ x) => NegateC x y

class    (Add i j ~ k, Add j i ~ k, Sub k j ~ i, Sub k i ~ j) => Sum i j k
instance (Add i j ~ k, Add j i ~ k, Sub k j ~ i, Sub k i ~ j) => Sum i j k

class    (Mul i j ~ k, Mul j i ~ k, Div k j ~ i, Div k i ~ j) => DivC i j k
instance (Mul i j ~ k, Mul j i ~ k, Div k j ~ i, Div k i ~ j) => DivC i j k
{-
class (AddS a b ~ c, AddS b a ~ c, SubS c b ~ a, SubS c a ~ b)
  => Sum (a::INT) (b::INT) (c::INT) where
    type AddS a b :: INT
    type SubS c b :: INT

instance Sum PZ i i where
  type AddS PZ i = i
  type SubS i  i = PZ
instance Sum i PZ i where
  type AddS i PZ = i
  type SubS i PZ = i
-}
{-
instance Sum (PS n) i  (Add (Pred (PS n)) (Succ i)) where
    type Add (PS n) i = Add (Pred (PS n)) (Succ i)
    type Sub i  i = PZ
-}
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
