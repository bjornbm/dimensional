{-# LANGUAGE DataKinds
           , KindSignatures
           , FlexibleInstances
           , MultiParamTypeClasses
           , TypeFamilies
  #-}
module ListVec where

import VecImp

import Numeric.Units.Dimensional.DK (Dimensional (Dimensional))
import Numeric.Units.Dimensional.DK.Prelude hiding (Length)
import qualified Data.HList as H
import Data.List (intercalate)
import qualified Prelude as P

import Numeric.NumType.DK hiding ((*), (+), (-), (/), Mul)


-- Implementation based on lists
-- -----------------------------
instance VecImp [a] a
  where
    data VecI (ds::DimList) [a] a = ListVec [a]
    vSing (Dimensional x) = ListVec [x]
    vCons (Dimensional x) (ListVec xs) = ListVec (x:xs)

    vHead (ListVec xs) = Dimensional (head xs)
    vTail (ListVec xs) = ListVec (tail xs)
    vElemAt n (ListVec xs) = Dimensional (xs!!toNum n)

    elemAdd (ListVec xs) (ListVec ys) = ListVec (zipWith (P.+) xs ys)
    elemSub (ListVec xs) (ListVec ys) = ListVec (zipWith (P.-) xs ys)

instance (AppUnC op a) => VecMap op ds [a] a where
  vMap f (ListVec xs) = ListVec $ map (unDim . appUn f . Dimensional) xs
    where unDim (Dimensional x) = x

--instance (GenericVMap ds, AppUnC op a) => VecMap op ds [a] a
  --where vMap = genericVMap

type Vec ds a = VecI ds [a] a  -- Synonym for ListVec.


elemMul :: Num a => Vec ds1 a -> Vec ds2 a -> Vec (ZipWith EMul ds1 ds2) a
elemMul (ListVec xs) (ListVec ys) = ListVec (zipWith (P.*) xs ys)


-- Test stuff
-- ==========
a = vSing ((1::Double)*~meter) :: Vec ('Sing DLength) Double
b = vCons ((3::Double)*~newton) a
double = undefined :: Double
doubles = [double]
vtype = undefined :: Vec ds Double
