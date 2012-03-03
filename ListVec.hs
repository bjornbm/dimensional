{-# LANGUAGE DataKinds
           , KindSignatures
           , FlexibleInstances
           , FlexibleContexts
           , MultiParamTypeClasses
           , TypeFamilies
           , ConstraintKinds
           , UndecidableInstances
  #-}
module ListVec where

import VecImp

import Numeric.Units.Dimensional.DK (Dimensional (Dimensional))
import Numeric.Units.Dimensional.DK.Prelude hiding (Length)
import qualified Data.HList as H
import Data.List (intercalate)
import qualified Prelude as P
import qualified Orthogonals as O

import Numeric.NumType.DK hiding ((*), (+), (-), (/), Mul)


-- Implementation based on lists
-- -----------------------------
instance Floating a => VecImp [a] a
  where
    data VecI (ds::DimList) [a] a = ListVec [a]
    vSing (Dimensional x) = ListVec [x]
    vCons (Dimensional x) (ListVec xs) = ListVec (x:xs)

    vHead (ListVec xs) = Dimensional (head xs)
    vTail (ListVec xs) = ListVec (tail xs)

    --vElemAt n (ListVec xs) = Dimensional (xs!!toNum n)

    elemAdd (ListVec xs) (ListVec ys) = ListVec (zipWith (P.+) xs ys)
    elemSub (ListVec xs) (ListVec ys) = ListVec (zipWith (P.-) xs ys)

    --dotProduct (ListVec xs) (ListVec ys) = Dimensional $ O.sum_product xs ys
{-
    crossProduct (ListVec [a,b,c]) (ListVec [d,e,f]) = ListVec
        [ b P.* f P.- e P.* c
        , c P.* d P.- f P.* a
        , a P.* e P.- d P.* b
        ]
-}
    -- Generic implementation.
    crossProduct v1 v2 = vCons (b * f - e * c)
         $ vCons (c * d - f * a)
         $ vSing (a * e - d * b)
         where (a,b,c) = toTuple v1
               (d,e,f) = toTuple v2

    vSum (ListVec xs) = Dimensional $ P.sum xs
    --vNorm (ListVec xs) = Dimensional $ P.sqrt $ O.sum_product xs xs
    --vNorm v = sqrt $ dotProduct v v
    --vNormalize v = (_1 / vNorm v) `scaleVec` v
    scaleVec (Dimensional x) (ListVec xs) = ListVec $ P.map (x P.*) xs
    --scaleVec x v = vMap (Scale x) v

instance (CDotProduct ds1 ds2 [a] a) => DotProductC ds1 ds2 [a] a
  --where dotProduct (ListVec xs) (ListVec ys) = Dimensional $ O.sum_product xs ys

instance (AppUnC op a, Floating a) => VecMap op ds [a] a where
  vMap f (ListVec xs) = ListVec $ map (unDim . appUn f . Dimensional) xs
    where unDim (Dimensional x) = x

instance ElemAtC [a] a

--instance (GenericVMap ds, AppUnC op a) => VecMap op ds [a] a
  --where vMap = genericVMap

type Vec ds a = VecI ds [a] a  -- Synonym for ListVec.


elemMul :: Num a => Vec ds1 a -> Vec ds2 a -> Vec (ZipWith EMul ds1 ds2) a
elemMul (ListVec xs) (ListVec ys) = ListVec (zipWith (P.*) xs ys)

elemDiv :: Fractional a => Vec ds1 a -> Vec ds2 a -> Vec (ZipWith EDiv ds1 ds2) a
elemDiv (ListVec xs) (ListVec ys) = ListVec (zipWith (P./) xs ys)


-- Test stuff
-- ==========
a1 = vSing ((1::Double)*~meter) :: Vec ('Sing DLength) Double
a2 = vCons (3*~newton) a1
a3 = vCons (3*~one) a2
b1 = vSing ((2::Double)*~newton) :: Vec ('Sing DForce) Double
b2 = vCons (4.4*~meter) b1
b3 = vCons (3*~(newton*meter)) b2
double = undefined :: Double
doubles = [double]
vtype = undefined :: Vec ds Double
vc3 = vCons (3.0 *~ meter)            $ vCons (2 *~ one)   $ vSing (1 *~ one)
vc4 = vCons (1   *~ (meter / second)) $ vCons (2 *~ hertz) $ vSing (3 *~ hertz)
vbad = elemAdd vtype vc3
vc34 = crossProduct vc3 vc4
