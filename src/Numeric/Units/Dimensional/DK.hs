{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
   Copyright  : Copyright (C) 2006-2015 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn@buckwalter.se
   Stability  : Stable
   Portability: GHC only

= Summary

In this module we provide data types for performing arithmetic with
physical quantities and units. Information about the physical
dimensions of the quantities/units is embedded in their types and
the validity of operations is verified by the type checker at compile
time. The boxing and unboxing of numerical values as quantities is
done by multiplication and division of units, of which an incomplete
set is provided.

We limit ourselves to \"Newtonian\" physics. We do not attempt to
accommodate relativistic physics in which e.g. addition of length
and time would be valid.

As far as possible and/or practical the conventions and guidelines
of NIST's "Guide for the Use of the International System of Units
(SI)" <#note1 [1]> are followed. Occasionally we will reference specific
sections from the guide and deviations will be explained.

== Disclaimer

Merely an engineer, the author doubtlessly uses a language and
notation that makes mathematicians and physicist cringe. He does
not mind constructive criticism (or darcs patches).

The sets of functions and units defined herein are incomplete and
reflect only the author's needs to date. Again, patches are welcome.

= Usage

== Preliminaries

This module requires GHC 7.8 or later. We utilize Data Kinds, TypeNats,
Closed Type Families, etc. Clients of the module are generally not
required to use these extensions.

Clients probably will want to use the NegativeLiterals extension.

== Examples

We have defined operators and units that allow us to define and
work with physical quantities. A physical quantity is defined by
multiplying a number with a unit (the type signature is optional).

> v :: Velocity Prelude.Double
> v = 90 *~ (kilo meter / hour)

It follows naturally that the numerical value of a quantity is
obtained by division by a unit.

> numval :: Prelude.Double
> numval = v /~ (meter / second)

The notion of a quantity as the product of a numerical value and a
unit is supported by 7.1 "Value and numerical value of a quantity" of
<#note1 [1]>. While the above syntax is fairly natural it is unfortunate that
it must violate a number of the guidelines in <#note1 [1]>, in particular 9.3
"Spelling unit names with prefixes", 9.4 "Spelling unit names obtained
by multiplication", 9.5 "Spelling unit names obtained by division".

As a more elaborate example of how to use the module we define a
function for calculating the escape velocity of a celestial body
<#note2 [2]>.

> escapeVelocity :: (Floating a) => Mass a -> Length a -> Velocity a
> escapeVelocity m r = sqrt (two * g * m / r)
>   where
>       two = 2 *~ one
>       g = 6.6720e-11 *~ (newton * meter ^ pos2 / kilo gram ^ pos2)

The following is an example GHC session where the above function
is used to calculate the escape velocity of Earth in kilometer per
second.

>>> :set +t
>>> let me = 5.9742e24 *~ kilo gram -- Mass of Earth.
me :: Quantity DMass GHC.Float.Double
>>> let re = 6372.792 *~ kilo meter -- Mean radius of Earth.
re :: Quantity DLength GHC.Float.Double
>>> let ve = escapeVelocity me re   -- Escape velocity of Earth.
ve :: Velocity GHC.Float.Double
>>> ve /~ (kilo meter / second)
11.184537332296259
it :: GHC.Float.Double

For completeness we should also show an example of the error messages
we will get from GHC when performing invalid arithmetic. In the
best case GHC will be able to use the type synonyms we have defined
in its error messages.

>>> x = 1 *~ meter + 1 *~ second
Couldn't match expected type `Pos1' against inferred type `Zero'
    Expected type: Unit DLength t
    Inferred type: Unit DTime a
  In the second argument of `(*~)', namely `second'
  In the second argument of `(+)', namely `1 *~ second'

In other cases the error messages aren't very friendly.

>>> x = 1 *~ meter / (1 *~ second) + 1 *~ kilo gram
Couldn't match expected type `Zero'
    against inferred type `Neg Zero'
  When using functional dependencies to combine
    Sub Zero (Pos Zero) (Neg Zero),
      arising from use of `/' at ...
    Sub Zero (Pos Zero) Zero,
      arising from use of `/' at ...

It is the author's experience that the usefullness of the compiler
error messages is more often than not limited to pinpointing the
location of errors.

= Notes

== Future work

While there is an insane amount of units in use around the world
it is reasonable to provide at least all SI units. Units outside
of SI will most likely be added on an as-needed basis.

There are also plenty of elementary functions to add. The 'Floating'
class can be used as reference.

Another useful addition would be decent 'Show' and 'Read' instances.
The 'show' implementation could output the numerical value and the
unit expressed in (base?) SI units, along the lines of:

> instance (Fractional a, Show a) => Show (Length a)
>   where show x = show (x /~ meter) ++ " m"

Additional functions could be provided for "showing" with any unit
and prefix.  The 'read' implementation should be able to read values
with any unit and prefix. It is not clear to the author how to best
implement these.

Additional physics models could be implemented. See <#note3 [3]> for ideas.

== Related work

Henning Thielemann numeric prelude has a physical units library,
however, checking of dimensions is dynamic rather than static.
Aaron Denney has created a toy example of statically checked
physical dimensions covering only length and time. HaskellWiki
has pointers <#note4 [4]> to these.

Also see Samuel Hoffstaetter's blog post <#note5 [5]> which uses techniques
similar to this library.

Libraries with similar functionality exist for other programming
languages and may serve as inspiration. The author has found the
Java library JScience <#note6 [6]> and the Fortress programming language <#note7 [7]>
particularly noteworthy.

== References

1. #note1# http://physics.nist.gov/Pubs/SP811/
2. #note2# http://en.wikipedia.org/wiki/Escape_velocity
3. #note3# http://jscience.org/api/org/jscience/physics/models/package-summary.html
4. #note4# http://www.haskell.org/haskellwiki/Physical_units
5. #note5# http://liftm.wordpress.com/2007/06/03/scientificdimension-type-arithmetic-and-physical-units-in-haskell/
6. #note6# http://jscience.org/
7. #note7# http://research.sun.com/projects/plrg/fortress.pdf

-}

module Numeric.Units.Dimensional.DK
  ( 
    -- * Types
    -- $types
    Dimensional,
    Unit, Quantity,
    Metricality(..),
    -- * Physical Dimensions
    -- $dimensions
    Dimension (Dim),
    -- ** Dimension Arithmetic
    -- $dimension-arithmetic
    type (*), type (/), type (^), Root, Recip,
    -- ** Term Level Representation of Dimensions
    -- $dimension-terms
    Dimension' (Dim'), HasDimension(..), KnownDimension,
    -- * Dimensional Arithmetic
    (*~), (/~),
    (^), (^/), (**), (*), (/), (+), (-),
    negate, abs, nroot, sqrt, cbrt,
    -- ** Transcendental Functions
    exp, log, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh, atan2,
    -- ** Operations on Collections
    -- $collections
    (*~~), (/~~), sum, mean, dimensionlessLength, nFromTo,
    -- * Dimension Synonyms
    -- $dimension-synonyms
    DOne, DLength, DMass, DTime, DElectricCurrent, DThermodynamicTemperature, DAmountOfSubstance, DLuminousIntensity,
    -- * Quantity Synonyms
    -- $quantity-synonyms
    Dimensionless, Length, Mass, Time, ElectricCurrent, ThermodynamicTemperature, AmountOfSubstance, LuminousIntensity,
    -- * Constants
    -- $constants
    _0, _1, _2, _3, _4, _5, _6, _7, _8, _9, pi, tau,
    -- * Constructing Units
    siUnit, one, composite, compositeFrac, compositeNum, compositeFrac', compositeNum', name,
    -- * Pretty Printing
    showIn,
    -- * On 'Functor', and Conversion Between Number Representations
    -- $functor
    KnownVariant(dmap), changeRep,
  )
  where

import Prelude
  ( Show, Eq(..), Ord, Bounded(..), Num, Fractional, Floating, Real, RealFloat, Functor, fmap
  , (.), flip, show, (++), fromIntegral, fromInteger, fromRational, error, max, succ
  , Int, Integer, Integral, ($), zipWith, uncurry, realToFrac, otherwise, String
  )
import qualified Prelude
import Numeric.NumType.DK.Integers
  ( TypeInt (Pos2, Pos3)
  , pos2, pos3
  , KnownTypeInt, toNum
  )
import Control.Applicative
import Data.Dynamic
import Data.ExactPi
import Data.Foldable (Foldable(foldr, foldl'))
import Data.Maybe
import Data.Monoid (Monoid(..))
import Data.Ratio
import Numeric.Units.Dimensional.DK.Dimensions
import Numeric.Units.Dimensional.DK.UnitNames hiding ((*), (/), (^), weaken)
import qualified Numeric.Units.Dimensional.DK.UnitNames.Internal as Name
import Numeric.Units.Dimensional.DK.UnitNames.InterchangeNames (HasInterchangeName(..))
import Numeric.Units.Dimensional.DK.Variants hiding (type (*))
import qualified Numeric.Units.Dimensional.DK.Variants as V

{-
We will reuse the operators and function names from the Prelude.
To prevent unpleasant surprises we give operators the same fixity
as the Prelude.
-}

infixr 8  ^, ^/, **
infixl 7  *, /
infixl 6  +, -


{- $types
Our primary objective is to define a data type that can be used to
represent (while still differentiating between) units and quantities.
There are two reasons for consolidating units and quantities in one
data type. The first being to allow code reuse as they are largely
subject to the same operations. The second being that it allows
reuse of operators (and functions) between the two without resorting
to occasionally cumbersome type classes.

The relationship between (the value of) a 'Quantity', its numerical
value and its 'Unit' is described in 7.1 "Value and numerical value
of a quantity" of <#note1 [1]>. In short a 'Quantity' is the product of a
number and a 'Unit'. We define the '(*~)' operator as a convenient
way to declare quantities as such a product.

-}

-- | A physical quantity or unit.
--
-- We call this data type 'Dimensional' to capture the notion that the
-- units and quantities it represents have physical dimensions.
-- 
-- The type variable 'a' is the only non-phantom type variable and
-- represents the numerical value of a quantity or the scale (w.r.t.
-- SI units) of a unit. For SI units the scale will always be 1. For
-- non-SI units the scale is the ratio of the unit to the SI unit with
-- the same physical dimension.
--
-- Since 'a' is the only non-phantom type we were able to define
-- 'Dimensional' as a newtype, avoiding boxing at runtime.
class KnownVariant (v :: Variant) where
  -- | A dimensional value, either a 'Quantity' or a 'Unit', parameterized by its 'Dimension' and representation.
  data Dimensional v :: Dimension -> * -> *
  extractValue :: Dimensional v d a -> (a, Maybe ExactPi)
  extractName :: Dimensional v d a -> Maybe (UnitName 'NonMetric)
  injectValue :: (Maybe (UnitName 'NonMetric)) -> (a, Maybe ExactPi) -> Dimensional v d a
  -- | Maps over the underlying representation of a dimensional value.
  -- The caller is responsible for ensuring that the supplied function respects the dimensional abstraction.
  -- This means that the function must preserve numerical values, or linearly scale them while preserving the origin.
  dmap :: (a1 -> a2) -> Dimensional v d a1 -> Dimensional v d a2

deriving instance Typeable Dimensional

instance KnownVariant 'DQuantity where
  newtype Dimensional 'DQuantity d a = Quantity' a
    deriving (Eq, Ord)
  extractValue (Quantity' x) = (x, Nothing)
  extractName _ = Nothing
  injectValue _ (x, _) = Quantity' x
  dmap f (Quantity' x) = Quantity' (f x)

instance (Typeable m) => KnownVariant ('DUnit m) where
  data Dimensional ('DUnit m) d a = Unit' (UnitName m) ExactPi (Maybe (Unit 'NonMetric d ExactPi)) a
  extractValue (Unit' _ e _ x) = (x, Just e)
  extractName (Unit' n _ _ _) = Just . Name.weaken $ n
  injectValue (Just n) (x, Just e) = let n' = relax n
                                      in case n' of
                                         Just n'' -> Unit' n'' e Nothing x
                                         _        -> Prelude.error "Shouldn't be reachable. Needed a metric name but got a non-metric one."
  injectValue _        _ = Prelude.error "Shouldn't be reachable. Needed to name a quantity."
  dmap f (Unit' n e _ x) = Unit' n e Nothing (f x)

-- | A unit of measurement.
type Unit (m :: Metricality) = Dimensional ('DUnit m)

-- | A dimensional quantity.
type Quantity = Dimensional 'DQuantity

-- GHC is somewhat unclear about why, but it won't derive this instance, so we give it explicitly.
instance (Bounded a) => Bounded (Quantity d a) where
  minBound = Quantity' minBound
  maxBound = Quantity' maxBound

instance HasInterchangeName (Unit m d a) where
  interchangeName (Unit' n _ _ _) = interchangeName n

name :: Unit m d a -> UnitName m
name (Unit' n _ _ _) = n

weaken :: Unit m d a -> Unit 'NonMetric d ExactPi
weaken (Unit' n e d _) = Unit' (Name.weaken n) e d e

-- Operates on a dimensional value using a unary operation on values, possibly yielding a Unit.
liftUntyped :: (KnownVariant v, KnownVariant (Weaken v)) => (ExactPi -> ExactPi) -> (a -> a) -> UnitNameTransformer -> (Dimensional v d1 a) -> (Dimensional (Weaken v) d2 a)
liftUntyped fe f nt x = let (x', e') = extractValue x
                            n = extractName x
                            n' = (liftA nt) n
                         in injectValue n' (f x', fmap fe e')

-- Operates on a dimensional value using a unary operation on values, yielding a Quantity.
liftUntypedQ :: (KnownVariant v) => (a -> a) -> Dimensional v d1 a -> Quantity d2 a
liftUntypedQ f x = let (x', _) = extractValue x
                    in Quantity' (f x')

-- Combines two dimensional values using a binary operation on values, possibly yielding a Unit.
liftUntyped2 :: (KnownVariant v1, KnownVariant v2, KnownVariant (v1 V.* v2)) => (ExactPi -> ExactPi -> ExactPi) -> (a -> a -> a) -> UnitNameTransformer2 -> Dimensional v1 d1 a -> Dimensional v2 d2 a -> Dimensional (v1 V.* v2) d3 a
liftUntyped2 fe f nt x1 x2 = let (x1', e1') = extractValue x1
                                 (x2', e2') = extractValue x2
                                 n1 = extractName x1
                                 n2 = extractName x2
                                 n' = (liftA2 nt) n1 n2
                              in injectValue n' (f x1' x2', fe <$> e1' <*> e2') 

-- Combines two dimensional values using a binary operation on values, yielding a Quantity.
liftUntyped2Q :: (KnownVariant v1, KnownVariant v2) => (a -> a -> a) -> Dimensional v1 d1 a -> Dimensional v2 d2 a -> Quantity d3 a
liftUntyped2Q f x1 x2 = let (x1', _) = extractValue x1
                            (x2', _) = extractValue x2
                         in Quantity' (f x1' x2') 

-- | Forms a 'Quantity' by multipliying a number and a unit.
(*~) :: Num a => a -> Unit m d a -> Quantity d a
x *~ (Unit' _ _ _ y) = Quantity' (x Prelude.* y)

-- | Divides a 'Quantity' by a 'Unit' of the same physical dimension, obtaining the
-- numerical value of the quantity expressed in that unit.
(/~) :: Fractional a => Quantity d a -> Unit m d a -> a
(Quantity' x) /~ (Unit' _ _ _ y) = (x Prelude./ y)

{-
We give '*~' and '/~' the same fixity as '*' and '/' defined below.
Note that this necessitates the use of parenthesis when composing
units using '*' and '/', e.g. "1 *~ (meter / second)".
-}

infixl 7  *~, /~

{- $dimensions
The phantom type variable d encompasses the physical dimension of
a 'Dimensional'. As detailed in <#note5 [5]> there are seven base dimensions,
which can be combined in integer powers to a given physical dimension.
We represent physical dimensions as the powers of the seven base
dimensions that make up the given dimension. The powers are represented
using NumTypes. For convenience we collect all seven base dimensions
in a data kind 'Dimension'.

We could have chosen to provide type variables for the seven base
dimensions in 'Dimensional' instead of creating a new data kind
'Dimension'. However, that would have made any type signatures involving
'Dimensional' very cumbersome.  By encompassing the physical dimension
in a single type variable we can "hide" the cumbersome type arithmetic
behind convenient type classes as will be seen later.

-}

{- $dimension-synonyms
Using our 'Dimension' data kind we define some type synonyms for convenience.
We start with the base dimensions, others can be found in "Numeric.Units.Dimensional.DK.Quantities".

-}

{- $quantity-synonyms
Using the above type synonyms we can define type synonyms for
quantities of particular physical dimensions.

Again we limit ourselves to the base dimensions, others can be found in "Numeric.Units.Dimensional.DK.Quantities".

-}

type Dimensionless            = Quantity DOne
type Length                   = Quantity DLength
type Mass                     = Quantity DMass
type Time                     = Quantity DTime
type ElectricCurrent          = Quantity DElectricCurrent
type ThermodynamicTemperature = Quantity DThermodynamicTemperature
type AmountOfSubstance        = Quantity DAmountOfSubstance
type LuminousIntensity        = Quantity DLuminousIntensity

{- $dimension-arithmetic
When performing arithmetic on units and quantities the arithmetics
must be applied to both the numerical values of the Dimensionals
but also to their physical dimensions. The type level arithmetic
on physical dimensions is governed by closed type families expressed
as type operators.

We could provide the 'Mul' and 'Div' classes with full functional
dependencies but that would be of limited utility as there is no
obvious use for "backwards" type inference and would also limit
what we can achieve overlapping instances. (In particular, it breaks
the 'Extensible' module.)

-}

{-
= Arithmetic on units and quantities =

Thanks to the arithmetic on physical dimensions having been sorted
out separately a lot of the arithmetic on Dimensionals is straight
forward. In particular the type signatures are much simplified.

Multiplication, division and powers apply to both units and quantities.
-}

-- | Multiplies two 'Quantity's or two 'Unit's.
--
-- The intimidating type signature captures the similarity between these operations
-- and ensures that composite 'Unit's are 'NonMetric'.
(*) :: (KnownVariant v1, KnownVariant v2, KnownVariant (v1 V.* v2), Num a) => Dimensional v1 d1 a -> Dimensional v2 d2 a -> Dimensional (v1 V.* v2) (d1 * d2) a
(*) = liftUntyped2 (Prelude.*) (Prelude.*) (Name.*)

-- | Divides one 'Quantity' by another or one 'Unit' by another.
--
-- The intimidating type signature captures the similarity between these operations
-- and ensures that composite 'Unit's are 'NotPrefixable'.
(/) :: (KnownVariant v1, KnownVariant v2, KnownVariant (v1 V.* v2), Fractional a) => Dimensional v1 d1 a -> Dimensional v2 d2 a -> Dimensional (v1 V.* v2) (d1 / d2) a
(/) = liftUntyped2 (Prelude./) (Prelude./) (Name./)

-- | Raises a 'Quantity' or 'Unit' to an integer power.
--
-- The intimidating type signature captures the similarity between these operations
-- and ensures that composite 'Unit's are 'NotPrefixable'.
(^) :: (Fractional a, KnownTypeInt i, KnownVariant v, KnownVariant (Weaken v))
    => Dimensional v d1 a -> Proxy i -> Dimensional (Weaken v) (d1 ^ i) a
x ^ n = let n' = (toNum n) :: Int
         in liftUntyped (Prelude.^^ n') (Prelude.^^ n') (Name.^ n') x

{-
A special case is that dimensionless quantities are not restricted
to integer exponents. This is accommodated by the '**' operator
defined later.


= Quantity operations =

Some additional operations obviously only make sense for quantities.
Of these, negation, addition and subtraction are particularly simple
as they are done in a single physical dimension.
-}

-- | Negates a quantity using 'Prelude.negate'.
negate :: Num a => Quantity d a -> Quantity d a
negate = liftUntypedQ Prelude.negate

-- | Adds two quantities using 'Prelude.+'. 
(+) :: Num a => Quantity d a -> Quantity d a -> Quantity d a
(+) = liftUntyped2Q (Prelude.+)

-- | Subtracts one quantity from another using 'Prelude.-'.
(-) :: Num a => Quantity d a -> Quantity d a -> Quantity d a
x - y = x + negate y

-- | Computes the absolute value of a quantity using 'Prelude.abs'.
abs :: Num a => Quantity d a -> Quantity d a
abs = liftUntypedQ Prelude.abs

{-
Roots of arbitrary (integral) degree. Appears to occasionally be useful
for units as well as quantities.
-}

-- | Computes the nth root of a quantity using 'Prelude.**'.
-- The 'Root' type family will prevent application of this operator where the result would have a fractional dimension or where n is zero.
nroot :: (KnownTypeInt n, Floating a)
      => Proxy n -> Quantity d a -> Quantity (Root d n) a
nroot n = let n' = 1 Prelude./ toNum n
           in liftUntypedQ (Prelude.** n')

{-
We provide short-hands for the square and cubic roots.
-}

-- | Computes the square root of a quantity using 'Prelude.**'.
-- The 'Root' type family will prevent application where the supplied quantity does not have a square dimension.
--
-- prop> sqrt x == nroot pos2 x
sqrt :: Floating a => Quantity d a -> Quantity (Root d 'Pos2) a
sqrt = nroot pos2

-- | Computes the cube root of a quantity using 'Prelude.**'.
-- The 'Root' type family will prevent application where the supplied quantity does not have a cubic dimension.
--
-- prop> cbrt x == nroot pos3 x
cbrt :: Floating a => Quantity d a -> Quantity (Root d 'Pos3) a
cbrt = nroot pos3

{-
We also provide an operator alternative to nroot for those that
prefer such.
-}

-- | Computes the nth root of a quantity using 'Prelude.**'.
-- The 'Root' type family will prevent application of this operator where the result would have a fractional dimension or where n is zero.
--
-- prop> x ^/ y == nroot y x
(^/) :: (KnownTypeInt n, Floating a)
     => Quantity d a -> Proxy n -> Quantity (Root d n) a
(^/) = flip nroot

{-
Since quantities form a monoid under addition, but not under multiplication unless they are dimensionless,
we will define a monoid instance that adds.
-}
instance (Num a) => Monoid (Quantity d a) where
  mempty = _0
  mappend = (+)

{- $collections
Here we define operators and functions to make working with homogenuous
lists of dimensionals more convenient.

We define two convenience operators for applying units to all
elements of a functor (e.g. a list).
-}

-- | Applies '(*~)' to all values in a functor.
(*~~) :: (Functor f, Num a) => f a -> Unit m d a -> f (Quantity d a)
xs *~~ u = fmap (*~ u) xs

-- | Applies '(/~)' to all values in a functor.
(/~~) :: (Functor f, Fractional a) => f (Quantity d a) -> Unit m d a -> f a
xs /~~ u = fmap (/~ u) xs

infixl 7  *~~, /~~

-- | The sum of all elements in a list.
sum :: (Num a, Foldable f) => f (Quantity d a) -> Quantity d a
sum = foldr (+) _0

-- | The arithmetic mean of all elements in a list.
mean :: (Fractional a, Foldable f) => f (Quantity d a) -> Quantity d a
mean = uncurry (/) . foldr accumulate (_0, _0)
  where
    accumulate val (accum, count) = (accum + val, count + _1)

-- | The length of the foldable data structure as a 'Dimensionless'.
-- This can be useful for purposes of e.g. calculating averages.
dimensionlessLength :: (Num a, Foldable f) => f (Dimensional v d a) -> Dimensionless a
dimensionlessLength x = (fromIntegral $ length x) *~ one
  where
    -- As in base-4.8 Data.Foldable for GHC 7.8 (base-4.6) compatibility.
    -- Once base-4.6. compatibility is abandoned this where clause can
    -- be deleted (and imports adjusted).
    length :: Foldable t => t a -> Int
    length = foldl' (\c _ -> c Prelude.+ 1) 0 

-- | Returns a list of quantities between given bounds.
nFromTo :: (Fractional a, Integral b) => Quantity d a -- ^ The initial value.
                                      -> Quantity d a -- ^ The final value.
                                      -> b -- ^ The number of intermediate values. If less than one, no intermediate values will result.
                                      -> [Quantity d a]
nFromTo xi xf n = fmap f [0..n'] ++ [xf]
  where
    n' = max 0 n
    f i = xi + realToFrac (i % succ n') *~ one * (xf - xi)

{-

= Dimensionless =

For dimensionless quantities pretty much any operation is applicable.
We provide this freedom by making 'Dimensionless' an instance of
'Functor'.
-}

instance Functor Dimensionless where
  fmap = dmap

{-
We continue by defining elementary functions on 'Dimensionless'
that may be obviously useful.
-}

exp, log, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh
  :: Floating a => Dimensionless a -> Dimensionless a
exp   = fmap Prelude.exp
log   = fmap Prelude.log
sin   = fmap Prelude.sin
cos   = fmap Prelude.cos
tan   = fmap Prelude.tan
asin  = fmap Prelude.asin
acos  = fmap Prelude.acos
atan  = fmap Prelude.atan
sinh  = fmap Prelude.sinh
cosh  = fmap Prelude.cosh
tanh  = fmap Prelude.tanh
asinh = fmap Prelude.asinh
acosh = fmap Prelude.acosh
atanh = fmap Prelude.atanh

-- | Raises a dimensionless quantity to a floating power using 'Prelude.**'.
(**) :: Floating a => Dimensionless a -> Dimensionless a -> Dimensionless a
(**) = liftUntyped2Q (Prelude.**)

-- | The standard two argument arctangent function.
-- Since it interprets its two arguments in comparison with one another, the input may have any dimension.
atan2 :: (RealFloat a) => Quantity d a -> Quantity d a -> Dimensionless a
atan2 = liftUntyped2Q Prelude.atan2

-- | A polymorphic 'Unit' which can be used in place of the coherent
-- SI base unit of any dimension. This allows polymorphic quantity
-- creation and destruction without exposing the 'Dimensional' constructor.
siUnit :: forall d a.(KnownDimension d, Num a) => Unit 'NonMetric d a
siUnit = Unit' (siBaseName (Proxy :: Proxy d)) 1 Nothing 1

{-
The only unit we will define in this module is 'one'.
-}

-- | The unit 'one' has dimension 'DOne' and is the base unit of dimensionless values. 
--
-- As detailed in 7.10 "Values of quantities expressed simply as numbers:
-- the unit one, symbol 1" of <#note1 [1]> the unit one generally does not
-- appear in expressions. However, for us it is necessary to use 'one'
-- as we would any other unit to perform the "boxing" of dimensionless values.
one :: Num a => Unit 'NonMetric DOne a
one = Unit' nOne 1 Nothing 1

{- $constants
For convenience we define some constants for small integer values
that often show up in formulae. We also throw in 'pi' and 'tau' for
good measure.

-}

-- | The constant for zero is polymorphic, allowing
-- it to express zero 'Length' or 'Capacitance' or 'Velocity' etc, in addition
-- to the 'Dimensionless' value zero.
_0 :: Num a => Quantity d a
_0 = Quantity' 0

_1, _2, _3, _4, _5, _6, _7, _8, _9 :: (Num a) => Dimensionless a
_1 = 1 *~ one
_2 = 2 *~ one
_3 = 3 *~ one
_4 = 4 *~ one
_5 = 5 *~ one
_6 = 6 *~ one
_7 = 7 *~ one
_8 = 8 *~ one
_9 = 9 *~ one

pi :: Floating a => Dimensionless a
pi = Prelude.pi *~ one

-- | Twice 'pi'.
--
-- For background on 'tau' see http://tauday.com/tau-manifesto (but also
-- feel free to review http://www.thepimanifesto.com).
tau :: Floating a => Dimensionless a
tau = _2 * pi

{- $functor
We intentionally decline to provide a 'Functor' instance for 'Dimensional' because its use breaks the
abstraction of physical dimensions.

If you feel your work requires this instance, it is provided as an orphan in "Numeric.Units.Dimensional.DK.Functor".

-}

-- | Convenient conversion between numerical types while retaining dimensional information.
changeRep :: (KnownVariant v, Real a, Fractional b) => Dimensional v d a -> Dimensional v d b
changeRep = dmap realToFrac

{- $dimension-terms
To facilitate parsing and pretty-printing functions that may wish to operate on term-level representations of dimension,
we provide a means for converting from type-level dimensions to term-level dimensions.

-}

instance (KnownDimension d) => HasDimension (Dimensional v d a) where
  dimension _ = dimension (Proxy :: Proxy d)

{-
We will conclude by providing a reasonable 'Show' instance for
quantities. The “normalized” unit of the quantity is inferred
from its dimension.

We neglect units since it is unclear how to represent them
in a way that distinguishes them from quantities, or whether that is
even a requirement.
-}
instance (KnownDimension d, Show a, Fractional a) => Show (Quantity d a) where
  show = showIn siUnit

showIn :: (KnownDimension d, Show a, Fractional a) => Unit m d a -> Quantity d a -> String
showIn (Unit' n _ _ y) q@(Quantity' x) | dimension q == dOne = show (x Prelude./ y)
                                       | otherwise           = (show (x Prelude./ y)) ++ " " ++ (show n)

instance (KnownDimension d, Show a) => Show (Unit m d a) where
  show (Unit' n e d x) = "The unit " ++ show n ++ ", with value " ++ show e ++ " (or " ++ show x ++ ")" ++ definingClause d
    where
      definingClause Nothing = ""
      definingClause (Just (Unit' n' e' _ _)) = ", defined to be " ++ show (e Prelude./ e') ++ " * " ++ show n'

siBaseName :: HasDimension d => d -> UnitName 'NonMetric
siBaseName d = let powers = asList $ dimension d
                in reduce . product $ zipWith (Name.^) baseUnitNames powers

-- | Applies a scale factor to a 'Unit'.
-- The 'prefix' function will be used by other modules to
-- define the SI prefixes and non-SI units.
-- 
-- Note that supplying zero as a definining quantity is invalid, as the library relies
-- upon units forming a group under multiplication. We do not raise an 'error' because
-- doing so would require an additional 'Eq' context.
-- 
-- Supplying negative defining quantities is allowed and handled gracefully, but is discouraged
-- on the grounds that it may be unexpected by other readers.
composite :: Floating a => UnitName m -> ExactPi -> Unit m1 d a -> Unit m d a
composite n s' u@(Unit' _ s _ x) = Unit' n (s' Prelude.* s) (Just . weaken $ u) (approximateValue s' Prelude.* x)

compositeFrac :: Fractional a => UnitName m -> Rational -> Unit m1 d ExactPi -> Unit m d a
compositeFrac n s' u@(Unit' _ s@(Exact 0 q) _ _) = Unit' n ((fromRational s') Prelude.* s) (Just . weaken $ u) (fromRational $ s' Prelude.* fromRational q)
compositeFrac _ _  _                             = error "The underlying unit does not have an exact rational conversion factor."

compositeFrac' :: Fractional a => UnitName m -> Rational -> Unit m1 d a -> Unit m d a
compositeFrac' n s' u@(Unit' _ s _ x) = Unit' n ((fromRational s') Prelude.* s) (Just . weaken $ u) (fromRational s' Prelude.* x)

compositeNum :: Num a => UnitName m -> Integer -> Unit m1 d ExactPi -> Unit m d a
compositeNum n s' u@(Unit' _ s@(Exact 0 q) _ _) | denominator q == 1 = Unit' n ((fromInteger s') Prelude.* s) (Just . weaken $ u) (fromInteger $ s' Prelude.* numerator q)
compositeNum _ _  _                                                  = error "The underlying unit does not have an exact integer conversion factor."

compositeNum' :: Num a => UnitName m -> Integer -> Unit m1 d a -> Unit m d a
compositeNum' n s' u@(Unit' _ s _ x) = Unit' n ((fromInteger s') Prelude.* s) (Just . weaken $ u) (fromInteger s' Prelude.* x)
