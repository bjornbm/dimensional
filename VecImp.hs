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
           , ConstraintKinds
  #-}

module VecImp where

import Numeric.Units.Dimensional.DK (Dimensional (Dimensional))
import Numeric.Units.Dimensional.DK.Prelude hiding (Length)
import qualified Data.HList as H
import Data.List (intercalate)
import qualified Prelude as P

import Numeric.NumType.DK hiding ((*), (+), (-), (/), Mul, Div)


infixr 2 :*, :*.

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
--type family   AppUn op (d::DimK) :: DimK
--type family   AppBi op (d1::DimK) (d2::DimK) :: DimK

type family   ZipWith op (ds1::DimList) (ds2::DimList) :: DimList
type instance ZipWith op (Sing d1) (Sing d2) = Sing (AppBi op d1 d2)
type instance ZipWith op (d1:*ds1) (d2:*ds2) = AppBi op d1 d2:*ZipWith op ds1 ds2

class ZipWithC op ds1 ds2 where
  vZipWith :: (VecImp i a, AppBiC op a)
           => op -> VecI ds1 i a -> VecI ds2 i a -> VecI (ZipWith op ds1 ds2) i a

instance ZipWithC op (Sing d1) (Sing d2) where
  vZipWith op v1 v2 = vSing $ appBi op (vHead v1) (vHead v2)

instance (ZipWithC op ds1 ds2) => ZipWithC op (d1:*ds1) (d2:*ds2) where
  vZipWith op v1 v2 = vCons (appBi op (vHead v1) (vHead v2)) (vZipWith op (vTail v1) (vTail v2))

type family   Map op (ds::DimList) :: DimList
type instance Map op (Sing d) = Sing (AppUn op d)
type instance Map op (d:*ds)  = AppUn op d:*Map op ds

-- Left fold with seed element.

type family   Foldl op (d::DimK) (ds::DimList) :: DimK
type instance Foldl op d1 (Sing d2) = AppBi op d1 d2
type instance Foldl op d1 (d2:*ds)  = Foldl op (AppBi op d1 d2) ds

class FoldlC op (d::DimK) (ds::DimList)
  where
    vFoldl :: (VecImp i a, AppBiC op a)
           => op -> Quantity d a -> VecI ds i a -> Quantity (Foldl op d ds) a

instance FoldlC op d1 (Sing d2)
  where
    vFoldl op x v = appBi op x $ vHead v

instance (FoldlC op (AppBi op d1 d2) ds) => FoldlC op d1 (d2:*ds)
  where
    vFoldl op x v = vFoldl op (appBi op x $ vHead v) (vTail v)

-- Left fold without seed element.

type family   Foldl1 op (ds::DimList) :: DimK
type instance Foldl1 op (Sing d) = d
type instance Foldl1 op (d:*ds)  = Foldl op d ds

class Foldl1C op (ds::DimList)
  where
    vFoldl1 :: (VecImp i a, AppBiC op a)
            => op -> VecI ds i a -> Quantity (Foldl1 op ds) a

instance (FoldlC op d ds) => Foldl1C op (d:*ds)
  where
    vFoldl1 op v = vFoldl op (vHead v) (vTail v)


-- Homogeneous vectors.
class HomoC (ds::DimList) where
  type Homo ds :: DimK
  vFoldl' :: VecImp i a => (Quantity e a -> Quantity (Homo ds) a -> Quantity e a)
          -> Quantity e a -> VecI ds i a -> Quantity e a
  vFoldl1' :: VecImp i a => (Quantity (Homo ds) a -> Quantity (Homo ds) a -> Quantity (Homo ds) a)
           -> VecI ds i a -> Quantity (Homo ds) a

instance HomoC (Sing d) where
  type Homo (Sing d) = d
  vFoldl' f x v = f x $ vHead v
  vFoldl1' _ v = vHead v

instance (HomoC ds, Homo ds ~ d) => HomoC (d:*ds) where
  type Homo (d:*ds) = Homo ds
  vFoldl' f x v = vFoldl' f (f x $ vHead v) (vTail v)
  vFoldl1' f v = vFoldl' f (vHead v) (vTail v)


--type family   Cross (ds1::DimList) (ds2::DimList) :: DimK
--type instance (Mul b f ~ Mul e c, Mul c d ~ Mul f a, Mul a e ~ Mul d b) => Cross (a:*b:*.c) (d:*e:*.f) = (Mul b f:*Mul c d:*.Mul a e)

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
    elemAdd :: VecI ds i a -> VecI ds i a -> VecI ds i a

    -- | Elementwise subraction of vectors. The vectors must have the
    -- same size and element types.
    elemSub :: VecI ds i a -> VecI ds i a -> VecI ds i a

    crossProduct :: (Mul b f ~ Mul e c, Mul c d ~ Mul f a1, Mul a1 e ~ Mul d b)
                 => VecI (a1:*b:*.c) i a -> VecI (d:*e:*.f) i a
                 -> VecI (Mul b f:*Mul c d:*.Mul a1 e) i a

    vSum :: (HomoC ds, Num a) => VecI ds i a -> Quantity (Homo ds) a
    vSum = vFoldl1' (+)

    vNorm :: (CNorm ds i a) => VecI ds i a -> Quantity (Homo ds) a
    vNorm v = sqrt $ dotProduct v v

    vNormalize :: (CNorm ds i a) => VecI ds i a -> VecI (Normalize ds a) i a
    vNormalize v = (_1 / vNorm v) `scaleVec` v

    scaleVec :: Quantity d a -> VecI ds i a -> VecI (Map (Scale d a) ds) i a

-- Elements
class GenericElemAt (n::Nat0) (ds::DimList) where
  type GElemAt n ds :: DimK
  genericElemAt :: (VecImp i a)
                => INTRep (P n) -> VecI ds i a -> Quantity (GElemAt n ds) a

instance GenericElemAt Z ds where
  type GElemAt Z ds = Head ds
  genericElemAt _ = vHead

instance (GenericElemAt n ds) => GenericElemAt (S0 n) (d:*ds) where
  type GElemAt (S0 n) (d:*ds) = GElemAt n ds
  genericElemAt i = genericElemAt (Decr i) . vTail

class ElemAtC (n::Nat0) (ds::DimList) i a where
  vElemAtG :: (GenericElemAt n ds, VecImp i a)
           => INTRep (P n) -> VecI ds i a -> Quantity (GElemAt n ds) a
  vElemAtG = genericElemAt
-- Dot product.

class (CDotProduct ds1 ds2 i a) => DotProductC ds1 ds2 i a where
  dotProduct :: VecI ds1 i a -> VecI ds2 i a -> Quantity (DotProduct ds1 ds2) a
  dotProduct v1 v2 = vSum $ vZipWith EMul v1 v2

type DotProduct ds1 ds2 = Homo (ZipWith EMul ds1 ds2)
type CDotProduct ds1 ds2 i a = (ZipWithC EMul ds1 ds2, VecImp i a, Num a
    , HomoC (ZipWith EMul ds1 ds2) -- inferable?
    )

-- VNorm.

type Norm ds = Root (DotProduct ds ds) Pos2
type CNorm ds i a = (DotProductC ds ds i a, Floating a, Norm ds ~ Homo ds)

-- Mapping operations to vectors.
class (VecImp i a) => VecMap op ds i a where
  vMap :: op -> VecI ds i a -> VecI (Map op ds) i a

type Normalize ds a = Map (Scale (Div DOne (Homo ds)) a) ds

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
  type AppUn op (d::DimK) :: DimK
  appUn :: op -> Quantity d a -> Quantity (AppUn op d) a

class AppBiC op a where
  type AppBi op (d1::DimK) (d2::DimK) :: DimK
  appBi :: op -> Quantity d1 a -> Quantity d2 a -> Quantity (AppBi op d1 d2) a

-- Generic implementations (not specialized for implementations).
class GenericVMap ds where
  genericVMap :: (AppUnC op a, VecImp i a) => op -> VecI ds i a -> VecI (Map op ds) i a
instance GenericVMap (Sing d) where
  genericVMap op = vSing . appUn op . vHead
instance (GenericVMap ds) => GenericVMap (d:*ds) where
  genericVMap op v = vCons (appUn op $ vHead v) $ genericVMap op $ vTail v


data EMul = EMul
instance Num a => AppBiC EMul a where
  type AppBi EMul d1 d2 = Mul d1 d2
  appBi EMul x y = x * y
--type instance AppBi EMul d1 d2 = Mul d1 d2

data EDiv = EDiv
--type instance AppBi EDiv d1 d2 = Div d1 d2
instance Fractional a => AppBiC EDiv a where
  type AppBi EDiv d1 d2 = Div d1 d2
  appBi EDiv x y = x / y


data Scale (d::DimK) a = Scale (Quantity d a)
instance Num a => AppUnC (Scale d a) a where
  type AppUn (Scale d a) d' = Mul d d'
  appUn (Scale x) y = x * y




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

instance ToTupleC (d0:*d1:*Sing d2) where
  type ToTuple (d0:*d1:*Sing d2) a = (Quantity d0 a, Quantity d1 a, Quantity d2 a)
  toTuple v = (vElemAt zero v, vElemAt pos1 v, vElemAt pos2 v)

-- From tuples.
class FromTupleC t a where
  type FromTuple t :: DimList
  fromTuple :: (VecImp i a) => t -> VecI (FromTuple t) i a

instance FromTupleC (Quantity d0 a, Quantity d1 a) a where
  type FromTuple (Quantity d0 a, Quantity d1 a) = (d0:*Sing d1)
  fromTuple (x, y) = vCons x $ vSing y

instance FromTupleC (Quantity d0 a, Quantity d1 a, Quantity d2 a) a where
  type FromTuple (Quantity d0 a, Quantity d1 a, Quantity d2 a) = (d0:*d1:*Sing d2)
  fromTuple (x, y, z) = vCons x $ vCons y $ vSing z

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

