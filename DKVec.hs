{-# LANGUAGE DataKinds
           , TypeFamilies
           , TypeOperators
           , GADTs
           , ScopedTypeVariables
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , RankNTypes
           , UndecidableInstances
  #-}

import Numeric.Units.Dimensional.DK (Dimensional (Dimensional))
import Numeric.Units.Dimensional.DK.Prelude hiding (Length)
import qualified Data.HList as H
import Data.List (intercalate)

import Numeric.NumType.DK

-- Kind level list.

data DimList = Cons DimK (DimList) | Sing DimK
type a :*  b = Cons a b
type a :*. b = Cons a (Sing b)

type family   Head (l::DimList) :: DimK
type instance Head (Sing a) = a
type instance Head (a :* b) = a

type family   Tail (l::DimList) :: DimList
type instance Tail (a :* b) = b

type family   Length (l::DimList) :: Nat1
type instance Length (Sing a) = O
type instance Length (a :* b) = S1 (Length b)

-- Zero-based indexing.
type family   ElemAt (n::Nat0) (l::DimList) :: DimK
type instance ElemAt Z (Sing a) = a
type instance ElemAt Z (a :* b) = a
type instance ElemAt (S0 n) (a :* b) = ElemAt n b

type family   Map (f :: DimK -> DimK) (l::DimList) :: DimList
type instance Map f (Sing a) = Sing (f a)
type instance Map f (a :* b) = (f a :* Map f b)



-- Data family for Vectors
-- =======================

class VecImp i a where
  data VecI :: DimList -> * -> * -> *
  -- Construction.
  vSing :: Quantity d a -> VecI (Sing d) i a
  vCons :: Quantity d a -> VecI ds i a -> VecI (d :* ds) i a
  -- Deconstruction
  vHead :: VecI ds i a -> Quantity (Head ds) a
  vTail :: VecI ds i a -> VecI (Tail ds) i a
  -- Elements
  vElemAt :: ToNum (P n) => INTRep (P n) -> VecI ds i a -> Quantity (ElemAt n ds) a
  -- Map
  --vMap :: (forall d. Quantity d a -> Quantity (f d) a) -> VecI ds i -> VecI (Map f ds) i

(.*) :: VecImp i a => Quantity d a -> VecI ds i a -> VecI (d:*ds) i a
(.*) = vCons
(.*.) :: VecImp i a => Quantity d0 a -> Quantity d1 a -> VecI (d0:*Sing d1) i a
x .*. y = vCons x $ vSing y

-- Implementation based on lists
-- -----------------------------
instance VecImp [a] a where
  data VecI (ds::DimList) [a] a = ListVec [a]
  vSing (Dimensional x) = ListVec [x]
  vCons (Dimensional x) (ListVec xs) = ListVec (x:xs)
  vHead (ListVec xs) = Dimensional (head xs)
  vTail (ListVec xs) = ListVec (tail xs)
  vElemAt n (ListVec xs) = Dimensional (xs!!toNum n)

type Vec ds a = VecI ds [a] a  -- Synonym for ListVec.


-- Mapping vectors??
-- =================

type family   VMap f v :: *
type instance VMap (Quantity d1 a -> Quantity d2 a) (VecI (Sing d1) i a) = VecI (Sing d2) i a
type instance VMap (Quantity d a -> Quantity (f d) a) (VecI (d1:*ds) i a) = VecI (f d:*Map f ds) i a
--type instance VMap f (VecI (Cons d) i) = VecI (Sing (f d)) i



-- Conversion to/from tuples
-- =========================

-- To tuples.
class ToTupleC (ds::DimList) where
  type ToTuple ds a
  toTuple :: (VecImp i a) => VecI ds i a -> ToTuple ds a

{-
-- The singleton.
instance ToTupleC (Sing d) where
  type ToTupleT (Sing d) a = (Quantity d a)
  toTupleF v = (vElemAt zero v)
-}

instance ToTupleC (d0:*Sing d1) where
  type ToTuple (d0:*Sing d1) a = (Quantity d0 a, Quantity d1 a)
  toTuple v = (vElemAt zero v, vElemAt pos1 v)


-- From tuples.
class FromTupleC t a where
  type FromTuple t :: DimList
  fromTuple :: (VecImp i a) => t -> VecI (FromTuple t) i a

instance FromTupleC (Quantity d0 a, Quantity d1 a) a where
  type FromTuple (Quantity d0 a, Quantity d1 a) = (d0:*Sing d1)
  fromTuple (x, y) = vCons x $ vSing y

fromTuple' :: (VecImp i a, FromTupleC t a) => VecI x i a -> t -> VecI (FromTuple t) i a
fromTuple' _ t = fromTuple t

{-
class (ToTuple ds a ~ t, FromTuple t ~ ds) => VTuple (ds::DimList) a t
  where
    type ToTuple (ds::DimList) a :: *
    type FromTuple t :: DimList
    toTuple   :: (VecImp i a) => VecI ds i a -> t
    fromTuple :: (VecImp i a) => t -> VecI ds i a

instance VTuple (d1:*.d2) a (Quantity d1 a, Quantity d2 a)
  where
    type ToTuple (d1:*.d2) a = (Quantity d1 a, Quantity d2 a)
    type FromTuple (Quantity d1 a, Quantity d2 a) = (d1:*.d2)
    toTuple v = (vElemAt zero v, vElemAt pos1 v)
    fromTuple (x, y) = x .*. y

fromTuple' :: (VecImp i a, VTuple ds a t) => VecI x i a -> t -> VecI ds i a
fromTuple' _ t = fromTuple t
-}

-- Conversion to/from HLists
-- =========================

class VHList ds i a where
  type ToHList (ds::DimList) i a
  toHList :: (VecImp i a) => VecI ds i a -> ToHList ds i a

instance VHList (Sing d) i a where
  type ToHList (Sing d) i a = H.HCons (Quantity (Head (Sing d)) a) H.HNil
  toHList v = H.HCons (vHead v) H.HNil

instance VHList l i a => VHList (d:*l) i a where
  type ToHList (d:*l) i a = H.HCons (Quantity (Head (d:*l)) a) (ToHList (Tail (d:*l)) i a)
  toHList v = H.HCons (vHead v) (toHList $ vTail v)


class HListV l i a where
  type FromHList l :: DimList
  fromHList :: (VecImp i a) => l -> VecI (FromHList l) i a

instance HListV (H.HCons (Quantity d a) H.HNil) i a where
  type FromHList (H.HCons (Quantity d a) H.HNil) = Sing d
  fromHList (H.HCons x _) = vSing x

instance (HListV (H.HCons e l) i a)
      => HListV (H.HCons (Quantity d a) (H.HCons e l)) i a where
  type FromHList (H.HCons (Quantity d a) (H.HCons e l)) = d :* FromHList (H.HCons e l)
  fromHList (H.HCons x l) = vCons x $ fromHList l

fromHList' :: (VecImp i a, HListV l i a) => VecI x i a -> l -> VecI (FromHList l) i a
fromHList' _ l = fromHList l


-- Showing
-- -------
-- We implement a custom @Show@ instance, using ToHList.
data ShowElem = ShowElem
instance Show a => H.Apply ShowElem a String where apply _ = show

instance (VHList ds [a] a, H.HMapOut ShowElem (ToHList ds [a] a) String) => Show (Vec ds a)
  where show = (\s -> "< " ++ s ++ " >")
             . intercalate ", "
             . H.hMapOut ShowElem
             . toHList

-- -}


-- Test stuff
-- ==========
a = vSing ((1::Double)*~meter) :: Vec ('Sing DLength) Double
b = vCons ((3::Double)*~newton) a
double = undefined :: Double
doubles = [double]
vtype = undefined :: Vec ds Double
