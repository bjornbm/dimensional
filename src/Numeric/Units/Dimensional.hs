{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-} -- for Vector instances only
{-# LANGUAGE PatternGuards #-}
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
not mind constructive criticism (or pull requests).

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

For completeness we should also show an example of the error messages
we will get from GHC when performing invalid arithmetic. In the
best case GHC will be able to use the type synonyms we have defined
in its error messages.

> let x = 1 *~ meter + 1 *~ second
>
> Couldn't match type 'Numeric.NumType.DK.Integers.Zero
>                with 'Numeric.NumType.DK.Integers.Pos1
> Expected type: Unit 'Metric DLength a
>   Actual type: Unit 'Metric DTime a
> In the second argument of `(*~)', namely `second'
> In the second argument of `(+)', namely `1 *~ second'

In other cases the error messages aren't very friendly.

> let x = 1 *~ meter / (1 *~ second) + 1 *~ kilo gram
>
> Couldn't match type 'Numeric.NumType.DK.Integers.Zero
>                with 'Numeric.NumType.DK.Integers.Neg1
> Expected type: Quantity DMass a
>   Actual type: Dimensional
>                  ('DQuantity V.* 'DQuantity) (DLength / DTime) a
> In the first argument of `(+)', namely `1 *~ meter / (1 *~ second)'
> In the expression: 1 *~ meter / (1 *~ second) + 1 *~ kilo gram
> In an equation for `x':
>       x = 1 *~ meter / (1 *~ second) + 1 *~ kilo gram

It is the author's experience that the usefullness of the compiler
error messages is more often than not limited to pinpointing the
location of errors.

= Notes

== Future work

While there is an insane amount of units in use around the world
it is reasonable to provide those in relatively widespread use. Units outside
of SI will most likely be added on an as-needed basis.

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

module Numeric.Units.Dimensional
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
    negate, abs, recip, nroot, sqrt, cbrt,
    -- ** Transcendental Functions
    exp, log, logBase, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh, atan2,
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
    siUnit, one, mkUnitR, mkUnitQ, mkUnitZ,
    -- * Unit Metadata
    name, exactValue, weaken, strengthen, exactify,
    -- * Pretty Printing
    showIn,
    -- * On 'Functor', and Conversion Between Number Representations
    -- $functor
    KnownVariant(dmap), changeRep, changeRepApproximate,
    -- * Lenses
    -- $lenses
    asLens
  )
  where

import Prelude
  ( Eq(..), Num, Fractional, Floating, Real, RealFloat, Functor, fmap
  , (.), flip, (++), fromIntegral, fromInteger, fromRational, error, max, succ
  , Int, Integer, Integral, ($), uncurry, realToFrac, otherwise
  )
import qualified Prelude
import Numeric.NumType.DK.Integers
  ( TypeInt (Pos2, Pos3)
  , pos2, pos3
  , KnownTypeInt, toNum
  )
import Data.Data
import Data.ExactPi
import Data.Foldable (Foldable(foldr, foldl'))
import Data.Maybe
import Data.Ratio
import Numeric.Units.Dimensional.Dimensions
import Numeric.Units.Dimensional.Internal
import Numeric.Units.Dimensional.UnitNames hiding ((*), (/), (^), weaken, strengthen)
import qualified Numeric.Units.Dimensional.UnitNames.Internal as Name
import Numeric.Units.Dimensional.Variants hiding (type (*))
import qualified Numeric.Units.Dimensional.Variants as V

-- $setup
-- >>> :set -XFlexibleInstances
-- >>> :set -XNoImplicitPrelude
-- >>> import Test.QuickCheck.Arbitrary
-- >>> import Numeric.Units.Dimensional.Prelude
-- >>> import Numeric.Units.Dimensional.Float
-- >>> instance Arbitrary a => Arbitrary (Quantity d a) where arbitrary = fmap Quantity arbitrary

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
number and a 'Unit'. We define the '*~' operator as a convenient
way to declare quantities as such a product.

-}

-- | Extracts the 'UnitName' of a 'Unit'.
name :: Unit m d a -> UnitName m
name (Unit n _ _) = n

-- | Extracts the exact value of a 'Unit', expressed in terms of the SI coherent derived unit (see 'siUnit') of the same 'Dimension'.
--
-- Note that the actual value may in some cases be approximate, for example if the unit is defined by experiment.
exactValue :: Unit m d a -> ExactPi
exactValue (Unit _ e _) = e

-- | Discards potentially unwanted type level information about a 'Unit'.
weaken :: Unit m d a -> Unit 'NonMetric d a
weaken (Unit n e v) = Unit (Name.weaken n) e v

-- | Attempts to convert a 'Unit' which may or may not be 'Metric' to one
-- which is certainly 'Metric'.
strengthen :: Unit m d a -> Maybe (Unit 'Metric d a)
strengthen (Unit n e v) | Just n' <- Name.strengthen n = Just $ Unit n' e v
                        | otherwise                    = Nothing

-- | Forms the exact version of a 'Unit'.
exactify :: Unit m d a -> Unit m d ExactPi
exactify (Unit n e _) = Unit n e e

-- | Forms a 'Quantity' by multipliying a number and a unit.
(*~) :: Num a => a -> Unit m d a -> Quantity d a
x *~ (Unit _ _ y) = Quantity (x Prelude.* y)

-- | Divides a 'Quantity' by a 'Unit' of the same physical dimension, obtaining the
-- numerical value of the quantity expressed in that unit.
(/~) :: Fractional a => Quantity d a -> Unit m d a -> a
(Quantity x) /~ (Unit _ _ y) = (x Prelude./ y)

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
We start with the base dimensions, others can be found in "Numeric.Units.Dimensional.Quantities".

-}

{- $quantity-synonyms
Using the above type synonyms we can define type synonyms for
quantities of particular physical dimensions.

Again we limit ourselves to the base dimensions, others can be found in "Numeric.Units.Dimensional.Quantities".

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
limited use for "backwards" type inference. Efforts are underway to
develop a type-checker plugin that does enable these scenarios, e.g.
for linear algebra.

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
(*) = liftD2 (Prelude.*) (Prelude.*) (Name.*)

-- | Divides one 'Quantity' by another or one 'Unit' by another.
--
-- The intimidating type signature captures the similarity between these operations
-- and ensures that composite 'Unit's are 'NotPrefixable'.
(/) :: (KnownVariant v1, KnownVariant v2, KnownVariant (v1 V.* v2), Fractional a) => Dimensional v1 d1 a -> Dimensional v2 d2 a -> Dimensional (v1 V.* v2) (d1 / d2) a
(/) = liftD2 (Prelude./) (Prelude./) (Name./)

-- | Forms the reciprocal of a 'Quantity', which has the reciprocal dimension.
recip :: (Fractional a) => Quantity d a -> Quantity (Recip d) a
recip = liftQ Prelude.recip

-- | Raises a 'Quantity' or 'Unit' to an integer power.
--
-- Because the power chosen impacts the 'Dimension' of the result, it is necessary to supply a type-level representation
-- of the exponent in the form of a 'Proxy' to some 'TypeInt'. Convenience values 'pos1', 'pos2', 'neg1', ... 
-- are supplied by the "Numeric.NumType.DK.Integers" module. The most commonly used ones are
-- also reexported by "Numeric.Units.Dimensional.Prelude".
--
-- The intimidating type signature captures the similarity between these operations
-- and ensures that composite 'Unit's are 'NotPrefixable'.
(^) :: (Fractional a, KnownTypeInt i, KnownVariant v, KnownVariant (Weaken v))
    => Dimensional v d1 a -> Proxy i -> Dimensional (Weaken v) (d1 ^ i) a
x ^ n = let n' = (toNum n) :: Int
         in liftD (Prelude.^^ n') (Prelude.^^ n') (Name.^ n') x

{-
A special case is that dimensionless quantities are not restricted
to integer exponents. This is accommodated by the '**' operator
defined later.


= Quantity operations =

Some additional operations obviously only make sense for quantities.
Of these, negation, addition and subtraction are particularly simple
as they are done in a single physical dimension.
-}

-- | Negates the value of a 'Quantity'.
negate :: Num a => Quantity d a -> Quantity d a
negate = liftQ Prelude.negate

-- | Adds two 'Quantity's.
(+) :: Num a => Quantity d a -> Quantity d a -> Quantity d a
(+) = liftQ2 (Prelude.+)

-- | Subtracts one 'Quantity' from another.
(-) :: Num a => Quantity d a -> Quantity d a -> Quantity d a
(-) = liftQ2 (Prelude.-)

-- | Takes the absolute value of a 'Quantity'.
abs :: Num a => Quantity d a -> Quantity d a
abs = liftQ Prelude.abs

{-
Roots of arbitrary (integral) degree. Appears to occasionally be useful
for units as well as quantities.
-}

-- | Computes the nth root of a 'Quantity' using 'Prelude.**'.
-- 
-- The 'Root' type family will prevent application of this operator where the result would have a fractional dimension or where n is zero.
--
-- Because the root chosen impacts the 'Dimension' of the result, it is necessary to supply a type-level representation
-- of the root in the form of a 'Proxy' to some 'TypeInt'. Convenience values 'pos1', 'pos2', 'neg1', ... 
-- are supplied by the "Numeric.NumType.DK.Integers" module. The most commonly used ones are
-- also reexported by "Numeric.Units.Dimensional.Prelude".
--
-- n must not be zero. Negative roots are defined such that @nroot (Proxy :: Proxy (Negate n)) x == nroot (Proxy :: Proxy n) (recip x)@.
--
-- Also available in operator form, see '^/'.
nroot :: (KnownTypeInt n, Floating a)
      => Proxy n -> Quantity d a -> Quantity (Root d n) a
nroot n = let n' = 1 Prelude./ toNum n
           in liftQ (Prelude.** n')

{-
We provide short-hands for the square and cubic roots.
-}

-- | Computes the square root of a 'Quantity' using 'Prelude.**'.
--
-- The 'Root' type family will prevent application where the supplied quantity does not have a square dimension.
--
-- prop> (x :: Area Double) >= _0 ==> sqrt x == nroot pos2 x
sqrt :: Floating a => Quantity d a -> Quantity (Root d 'Pos2) a
sqrt = nroot pos2

-- | Computes the cube root of a 'Quantity' using 'Prelude.**'.
--
-- The 'Root' type family will prevent application where the supplied quantity does not have a cubic dimension.
--
-- prop> (x :: Volume Double) >= _0 ==> cbrt x == nroot pos3 x
cbrt :: Floating a => Quantity d a -> Quantity (Root d 'Pos3) a
cbrt = nroot pos3

{-
We also provide an operator alternative to nroot for those that
prefer such.
-}

-- | Computes the nth root of a 'Quantity' using 'Prelude.**'.
-- 
-- The 'Root' type family will prevent application of this operator where the result would have a fractional dimension or where n is zero.
--
-- Because the root chosen impacts the 'Dimension' of the result, it is necessary to supply a type-level representation
-- of the root in the form of a 'Proxy' to some 'TypeInt'. Convenience values 'pos1', 'pos2', 'neg1', ... 
-- are supplied by the "Numeric.NumType.DK.Integers" module. The most commonly used ones are
-- also reexported by "Numeric.Units.Dimensional.Prelude".
--
-- Also available in prefix form, see 'nroot'.
(^/) :: (KnownTypeInt n, Floating a)
     => Quantity d a -> Proxy n -> Quantity (Root d n) a
(^/) = flip nroot

{- $collections
Here we define operators and functions to make working with homogenuous
lists of dimensionals more convenient.

We define two convenience operators for applying units to all
elements of a functor (e.g. a list).
-}

-- | Applies '*~' to all values in a functor.
(*~~) :: (Functor f, Num a) => f a -> Unit m d a -> f (Quantity d a)
xs *~~ u = fmap (*~ u) xs

-- | Applies '/~' to all values in a functor.
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

-- | Raises a dimensionless quantity to a dimensionless power.
(**) :: Floating a => Dimensionless a -> Dimensionless a -> Dimensionless a
(**) = liftQ2 (Prelude.**)

-- | Takes the logarithm of the second argument in the base of the first.
logBase :: Floating a => Dimensionless a -> Dimensionless a -> Dimensionless a
logBase = liftQ2 Prelude.logBase

-- | The standard two argument arctangent function.
-- Since it interprets its two arguments in comparison with one another, the input may have any dimension.
atan2 :: (RealFloat a) => Quantity d a -> Quantity d a -> Dimensionless a
atan2 = liftQ2 Prelude.atan2

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
one = Unit nOne 1 1

{- $constants
For convenience we define some constants for small integer values
that often show up in formulae. We also throw in 'pi' and 'tau' for
good measure.

-}

-- | The constant for zero is polymorphic, allowing
-- it to express zero 'Length' or 'Capacitance' or 'Velocity' etc, in addition
-- to the 'Dimensionless' value zero.
_0 :: Num a => Quantity d a
_0 = Quantity 0

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

If you feel your work requires this instance, it is provided as an orphan in "Numeric.Units.Dimensional.Functor".

-}

-- | Convenient conversion between numerical types while retaining dimensional information.
changeRep :: (KnownVariant v, Real a, Fractional b) => Dimensional v d a -> Dimensional v d b
changeRep = dmap realToFrac

-- | Convenient conversion from exactly represented values while retaining dimensional information.
changeRepApproximate :: (KnownVariant v, Floating b) => Dimensional v d ExactPi -> Dimensional v d b
changeRepApproximate = dmap approximateValue

{- $lenses
These functions are compatible with the lens library.

-}

-- | Converts a 'Unit' into a lens from 'Quantity's to values.
asLens :: (Fractional a) => Unit m d a 
                         -> (forall f.Functor f => (a -> f a)
                                                -> Quantity d a
                                                -> f (Quantity d a))
asLens u f q = fmap (\v' -> v' *~ u) (f (q /~ u))

{- $dimension-terms
To facilitate parsing and pretty-printing functions that may wish to operate on term-level representations of dimension,
we provide a means for converting from type-level dimensions to term-level dimensions.

-}

-- | Forms a new atomic 'Unit' by specifying its 'UnitName' and its definition as a multiple of another 'Unit'.
-- 
-- Use this variant when the scale factor of the resulting unit is irrational or 'Approximate'. See 'mkUnitQ' for when it is rational
-- and 'mkUnitZ' for when it is an integer.
--
-- Note that supplying zero as a definining quantity is invalid, as the library relies
-- upon units forming a group under multiplication.
-- 
-- Supplying negative defining quantities is allowed and handled gracefully, but is discouraged
-- on the grounds that it may be unexpected by other readers.
mkUnitR :: Floating a => UnitName m -> ExactPi -> Unit m1 d a -> Unit m d a
mkUnitR n s' (Unit _ s x) | isExactZero s = error "Supplying zero as a conversion factor is not valid."
                          | otherwise     = Unit n (s' Prelude.* s) (approximateValue s' Prelude.* x)

-- | Forms a new atomic 'Unit' by specifying its 'UnitName' and its definition as a multiple of another 'Unit'.
--
-- Use this variant when the scale factor of the resulting unit is rational. See 'mkUnitZ' for when it is an integer
-- and 'mkUnitR' for the general case.
--
-- For more information see 'mkUnitR'.
mkUnitQ :: Fractional a => UnitName m -> Rational -> Unit m1 d a -> Unit m d a
mkUnitQ n s' (Unit _ s _) | s' == 0                       = error "Supplying zero as a conversion factor is not valid."
                          | Just q <- toExactRational s'' = Unit n s'' (fromRational q)
                          | otherwise                     = error "The resulting conversion factor is not an exact rational." 
  where
    s'' = fromRational s' Prelude.* s                               

-- | Forms a new atomic 'Unit' by specifying its 'UnitName' and its definition as a multiple of another 'Unit'.
--
-- Use this variant when the scale factor of the resulting unit is an integer. See 'mkUnitQ' for when it is rational
-- and 'mkUnitR' for the general case.
--
-- For more information see 'mkUnitR'.
mkUnitZ :: Num a => UnitName m -> Integer -> Unit m1 d a -> Unit m d a
mkUnitZ n s' (Unit _ s _) | s' == 0                      = error "Supplying zero as a conversion factor is not valid."
                          | Just z <- toExactInteger s'' = Unit n s'' (fromInteger z)
                          | otherwise                    = error "The resulting conversion factor is not an exact integer."
  where
    s'' = fromInteger s' Prelude.* s
