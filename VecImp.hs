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

module VecImp where

import Numeric.Units.Dimensional.DK (Dimensional (Dimensional))
import Numeric.Units.Dimensional.DK.Prelude hiding (Length)
import qualified Data.HList as H
import Data.List (intercalate)
import qualified Prelude as P

import Numeric.NumType.DK hiding ((*), (+), (-), (/), Mul)


-- Kind level list of Dimensions.

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

-- Lookup with zero-based indexing.
type family   ElemAt (n::Nat0) (l::DimList) :: DimK
type instance ElemAt Z (Sing a) = a
type instance ElemAt Z (a :* b) = a
type instance ElemAt (S0 n) (a :* b) = ElemAt n b


-- Higher level functions
-- ----------------------

-- Apply an unary operator to one dimension or a binary operator to two
-- dimensions. What a given operator does is captured by a type instance.
type family   AppUn op (d::DimK) :: DimK
type family   AppBi op (d1::DimK) (d2::DimK) :: DimK

type family   ZipWith op (ds1::DimList) (ds2::DimList) :: DimList
type instance ZipWith op (Sing d1) (Sing d2) = Sing (AppBi op d1 d2)
type instance ZipWith op (d1:*ds1) (d2:*ds2) = AppBi op d1 d2:*ZipWith op ds1 ds2

type family   Map op (ds::DimList) :: DimList
type instance Map op (Sing d) = Sing (AppUn op d)
type instance Map op (d:*ds)  = AppUn op d:*Map op ds

type family   Fold op (d::DimK) (ds::DimList) :: DimK
type instance Fold op d1 (Sing d2) = AppBi op d1 d2
type instance Fold op d1 (d2:*ds) = Fold op (AppBi op d1 d2) ds



-- Data family for Vectors
-- =======================

class VecImp i a
  where
    data VecI :: DimList -> * -> * -> *

    -- Construction.
    vSing :: Quantity d a -> VecI (Sing d) i a
    vCons :: Quantity d a -> VecI ds i a -> VecI (d:*ds) i a

    -- Deconstruction
    vHead :: VecI ds i a -> Quantity (Head ds) a
    vTail :: VecI ds i a -> VecI (Tail ds) i a
    vElemAt :: ToNum (P n) => INTRep (P n) -> VecI ds i a -> Quantity (ElemAt n ds) a

    -- | Elementwise addition of vectors. The vectors must have the
    -- same size and element types.
    elemAdd :: Num a => VecI ds i a -> VecI ds i a -> VecI ds i a

    -- | Elementwise subraction of vectors. The vectors must have the
    -- same size and element types.
    elemSub :: Num a => VecI ds i a -> VecI ds i a -> VecI ds i a


-- Mapping operations to vectors.
class (VecImp i a) => VecMap op ds i a where
  vMap :: op -> VecI ds i a -> VecI (Map op ds) i a


-- Operators for convenient vector building
-- ----------------------------------------

(.*) :: VecImp i a => Quantity d a -> VecI ds i a -> VecI (d:*ds) i a
(.*) = vCons
(.*.) :: VecImp i a => Quantity d0 a -> Quantity d1 a -> VecI (d0:*Sing d1) i a
x .*. y = vCons x $ vSing y


-- ****************************************************************
-- * EXPERIMENTAL *************************************************
-- ****************************************************************



-- Generic implementations
class AppUnC op a where
  appUn :: op -> Quantity d a -> Quantity (AppUn op d) a

-- Generic implementations (not specialized for implementations).
class GenericVMap ds where
  genericVMap :: (AppUnC op a, VecImp i a) => op -> VecI ds i a -> VecI (Map op ds) i a
instance GenericVMap (Sing d) where
  genericVMap op = vSing . appUn op . vHead
instance (GenericVMap ds) => GenericVMap (d:*ds) where
  genericVMap op v = vCons (appUn op $ vHead v) $ genericVMap op $ vTail v



data EMul = EMul
type instance AppUn EMul d = Mul d d
type instance AppBi EMul d1 d2 = Mul d1 d2

instance Num a => AppUnC EMul a where appUn _ x = x*x

{-
class FoldC ds where
  vFold :: op -> Quantity d a -> VecI ds i a -> Quantity (Fold op d ds) a
instance FoldC (Sing d) where
  vFold f x v = f x (vHead v)
-- -}




-- ****************************************************************
-- ****************************************************************
-- ****************************************************************


-- Conversion to/from tuples
-- =========================

-- To tuples.
class ToTupleC (ds::DimList) where
  type ToTuple ds a
  toTuple :: (VecImp i a) => VecI ds i a -> ToTuple ds a

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

-- Convenience, typed by example.
fromTuple' :: (VecImp i a, FromTupleC t a) => VecI x i a -> t -> VecI (FromTuple t) i a
fromTuple' _ t = fromTuple t


-- Conversion to/from HLists
-- =========================

class (VecImp i a) => ToHListC ds i a where
  type ToHList (ds::DimList) i a
  toHList :: VecI ds i a -> ToHList ds i a

instance (VecImp i a) => ToHListC (Sing d) i a where
  type ToHList (Sing d) i a = H.HCons (Quantity (Head (Sing d)) a) H.HNil
  toHList v = H.HCons (vHead v) H.HNil

instance (ToHListC l i a) => ToHListC (d:*l) i a where
  type ToHList (d:*l) i a = H.HCons (Quantity (Head (d:*l)) a) (ToHList (Tail (d:*l)) i a)
  toHList v = H.HCons (vHead v) (toHList $ vTail v)


class (VecImp i a) => FromHListC l i a where
  type FromHList l :: DimList
  fromHList :: l -> VecI (FromHList l) i a

instance (VecImp i a) => FromHListC (H.HCons (Quantity d a) H.HNil) i a where
  type FromHList (H.HCons (Quantity d a) H.HNil) = Sing d
  fromHList (H.HCons x _) = vSing x

instance (FromHListC (H.HCons e l) i a)
      => FromHListC (H.HCons (Quantity d a) (H.HCons e l)) i a where
  type FromHList (H.HCons (Quantity d a) (H.HCons e l)) = d :* FromHList (H.HCons e l)
  fromHList (H.HCons x l) = vCons x $ fromHList l

-- Convenience, typed by example.
fromHList' :: (FromHListC l i a) => VecI x i a -> l -> VecI (FromHList l) i a
fromHList' _ l = fromHList l


-- Showing
-- =======
-- We implement a custom @Show@ instance, using ToHList.
-- This was copied from dimensional-vectors.
--
-- TODO: reimplement without HMapOut and remove dependency on HList.
data ShowElem = ShowElem
instance Show a => H.Apply ShowElem a String where apply _ = show

instance (ToHListC ds i a, H.HMapOut ShowElem (ToHList ds i a) String) => Show (VecI ds i a)
  where show = (\s -> "< " ++ s ++ " >")
             . intercalate ", "
             . H.hMapOut ShowElem
             . toHList

