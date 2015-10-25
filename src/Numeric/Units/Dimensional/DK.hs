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
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    -- * Physical Dimensions
    -- $dimensions
    Dimension (Dim),
    -- ** Dimension Arithmetic
    -- $dimension-arithmetic
    type (*), type (/), type (^), Root, Recip, type SIDim, type ToSIDim,
    -- ** Term Level Representation of Dimensions
    -- $dimension-terms
    Dimension' (Dim'), HasDimension(..), KnownDimension,
    -- * Dimensional Arithmetic
    (*~), (/~),
    (^), (^/), (**), (*), (/), (+), (-), (~*), (~/),
    negate, abs, nroot, sqrt, cbrt,
    -- ** Transcendental Functions
    exp, log, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh, atan2,
    -- ** Dealing with Angles
    removeAngles, coerceAngles,
    -- ** Operations on Collections
    -- $collections
    (*~~), (/~~), sum, mean, dimensionlessLength, nFromTo,
    -- * Dimension Synonyms
    -- $dimension-synonyms
    DOne, DLength, DMass, DTime, DElectricCurrent, DThermodynamicTemperature, DAmountOfSubstance, DLuminousIntensity, DPlaneAngle,
    -- * Quantity Synonyms
    -- $quantity-synonyms
    Dimensionless, Length, Mass, Time, ElectricCurrent, ThermodynamicTemperature, AmountOfSubstance, LuminousIntensity, PlaneAngle,
    -- * Constants
    -- $constants
    _0, _1, _2, _3, _4, _5, _6, _7, _8, _9, pi, tau,
    -- * Constructing Units
    prefix, baseUnit, siUnit, one,
    -- * On 'Functor', and Conversion Between Number Representations
    -- $functor
    dmap, changeRep
  )
  where

import Prelude
  ( Show, Eq, Ord(..), Bounded, Num, Fractional, Floating, Real, RealFloat, Integral, Functor, fmap
  , (.), flip, show, (++), String, fromIntegral
  , Int, ($), zipWith, uncurry, concat, realToFrac, succ, undefined
  )
import qualified Prelude
import Numeric.NumType.DK.Integers
  ( TypeInt (Pos2, Pos3)
  , pos2, pos3
  , KnownTypeInt, toNum
  )
import Control.DeepSeq
import Control.Monad (liftM)
import Data.Coerce (coerce)
import Data.Data
import Data.Foldable (Foldable(foldr, foldl'))
import Data.Monoid (Monoid(..))
import Data.Ratio ((%))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable(..))
import GHC.Generics
import Numeric.Units.Dimensional.DK.Dimensions
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Base as U

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
type role Dimensional nominal phantom representational
newtype Dimensional (v::Variant) (d::Dimension) a
      = Dimensional a deriving (Eq, Ord, Bounded, Typeable, Data, Generic, Generic1)

{-
The variety 'v' of 'Dimensional'

The phantom type variable v is used to distinguish between units
and quantities. It must be one of the following:
-}

data Variant = DUnit | DQuantity

-- | A unit of measurement.
type Unit     = Dimensional 'DUnit

-- | A dimensional quantity.
type Quantity = Dimensional 'DQuantity


-- | Forms a 'Quantity' by multipliying a number and a unit.
(*~) :: Num a => a -> Dimensional v d a -> Quantity d a
x *~ Dimensional y = Dimensional (x Prelude.* y)

-- | Divides a 'Quantity' by a 'Unit' of the same physical dimension, obtaining the
-- numerical value of the quantity expressed in that unit.
(/~) :: Fractional a => Quantity d a -> Dimensional v d a -> a
Dimensional x /~ Dimensional y = x Prelude./ y

{-
'~*' and '~/' can be used to scale quantities or units. '~/'
cannot be used to create a 'Quantity' from a 'Unit', for that use
e.g. @0.5 *~ meter@ rather than @meter ~/ 2@.
-}

(~*) :: Num a => Dimensional v d a -> a -> Dimensional v d a
Dimensional x ~* y = Dimensional (x Prelude.* y)

(~/) :: Fractional a => Dimensional v d a -> a -> Dimensional v d a
Dimensional x ~/ y = Dimensional (x Prelude./ y)

{-
We give '*~' and '/~' the same fixity as '*' and '/' defined below.
Note that this necessitates the use of parenthesis when composing
units using '*' and '/', e.g. "1 *~ (meter / second)".
-}

infixl 7  *~, /~, ~*, ~/

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
type PlaneAngle               = Quantity DPlaneAngle

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

-- | Forms the product of two 'Quantity's or of two 'Unit's.
(*) :: Num a
    => Dimensional v d a -> Dimensional v d' a -> Dimensional v (d * d') a
Dimensional x * Dimensional y = Dimensional (x Prelude.* y)

-- | Forms the quotient of one 'Quantity' with another or of one 'Unit' with another.
(/) :: Fractional a
    => Dimensional v d a -> Dimensional v d' a -> Dimensional v (d / d') a
Dimensional x / Dimensional y = Dimensional (x Prelude./ y)

-- | Raises a 'Quantity' or 'Unit' to an integer power.
--
-- Because the power chosen impacts the 'Dimension' of the result, it is necessary to supply a type-level representation
-- of the exponent in the form of a 'Proxy' to some 'TypeInt'. Convenience values 'pos1', 'pos2', 'neg1', ... 
-- are supplied by the "Numeric.NumType.DK.Integers" module. The most commonly used ones are
-- also reexported by "Numeric.Units.Dimensional.DK.Prelude".
(^) :: (KnownTypeInt i, Fractional a)
    => Dimensional v d a -> Proxy i -> Dimensional v (d ^ i) a
Dimensional x ^ n = Dimensional (x Prelude.^^ (toNum n :: Int))

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
negate (Dimensional x) = Dimensional (Prelude.negate x)

-- | Adds two 'Quantity's.
(+) :: Num a => Quantity d a -> Quantity d a -> Quantity d a
Dimensional x + Dimensional y = Dimensional (x Prelude.+ y)

-- | Subtracts one 'Quantity' from another.
(-) :: Num a => Quantity d a -> Quantity d a -> Quantity d a
x - y = x + negate y

{-
Absolute value.
-}

-- | Takes the absolute value of a 'Quantity'.
abs :: Num a => Quantity d a -> Quantity d a
abs (Dimensional x) = Dimensional (Prelude.abs x)

{-
Roots of arbitrary (integral) degree. Appears to occasionally be useful
for units as well as quantities.
-}

-- | Takes the nth root of a 'Quantity' or 'Unit'.
-- 
-- Because the root chosen impacts the 'Dimension' of the result, it is necessary to supply a type-level representation
-- of the root in the form of a 'Proxy' to some 'TypeInt'. Convenience values 'pos1', 'pos2', 'neg1', ... 
-- are supplied by the "Numeric.NumType.DK.Integers" module. The most commonly used ones are
-- also reexported by "Numeric.Units.Dimensional.DK.Prelude".
--
-- Also available in operator form, see '^/'.
nroot :: (Floating a, KnownTypeInt n)
      => Proxy n -> Dimensional v d a -> Dimensional v (Root d n) a
nroot n (Dimensional x) = Dimensional (x Prelude.** (1 Prelude./ toNum n))

{-
We provide short-hands for the square and cubic roots.
-}

-- | Takes the square root of a 'Quantity' or 'Unit'.
sqrt :: Floating a => Dimensional v d a -> Dimensional v (Root d 'Pos2) a
sqrt = nroot pos2

-- | Takes the cube root of a 'Quantity' or 'Unit'.
cbrt :: Floating a => Dimensional v d a -> Dimensional v (Root d 'Pos3) a
cbrt = nroot pos3

-- | Takes the nth root of a 'Quantity' or 'Unit'.
-- 
-- Because the root chosen impacts the 'Dimension' of the result, it is necessary to supply a type-level representation
-- of the root in the form of a 'Proxy' to some 'TypeInt'. Convenience values 'pos1', 'pos2', 'neg1', ... 
-- are supplied by the "Numeric.NumType.DK.Integers" module. The most commonly used ones are
-- also reexported by "Numeric.Units.Dimensional.DK.Prelude".
--
-- Also available in prefix form, see 'nroot'.
(^/) :: (KnownTypeInt n, Floating a)
     => Dimensional v d a -> Proxy n -> Dimensional v (Root d n) a
(^/) = flip nroot

{-
Since quantities form a monoid under addition, but not under multiplication unless they are dimensionless,
we will define a monoid instance that adds.
-}

-- | 'Quantity's of a given 'Dimension' form a 'Monoid' under addition.
instance (Num a) => Monoid (Quantity d a) where
  mempty = _0
  mappend = (+)

{- $collections
Here we define operators and functions to make working with homogenuous
lists of dimensionals more convenient.

We define two convenience operators for applying units to all
elements of a functor (e.g. a list).
-}

-- | Applies '*~' to all values in a functor.
(*~~) :: (Functor f, Num a) => f a -> Unit d a -> f (Quantity d a)
xs *~~ u = fmap (*~ u) xs

-- | Applies '/~' to all values in a functor.
(/~~) :: (Functor f, Fractional a) => f (Quantity d a) -> Unit d a -> f a
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
dimensionlessLength = Dimensional . fromIntegral . length
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

exp, log, sinh, cosh, tanh, asinh, acosh, atanh
  :: Floating a => Dimensionless a -> Dimensionless a
exp   = fmap Prelude.exp
log   = fmap Prelude.log
sinh  = fmap Prelude.sinh
cosh  = fmap Prelude.cosh
tanh  = fmap Prelude.tanh
asinh = fmap Prelude.asinh
acosh = fmap Prelude.acosh
atanh = fmap Prelude.atanh

sin, cos, tan :: Floating a => PlaneAngle a -> Dimensionless a
sin   = (*~ one) . Prelude.sin . (/~ baseUnit)
cos   = (*~ one) . Prelude.cos . (/~ baseUnit)
tan   = (*~ one) . Prelude.tan . (/~ baseUnit)

asin, acos, atan :: Floating a => Dimensionless a -> PlaneAngle a
asin  = (*~ baseUnit) . Prelude.asin . (/~ one)
acos  = (*~ baseUnit) . Prelude.acos . (/~ one)
atan  = (*~ baseUnit) . Prelude.atan . (/~ one)

-- | Removes angular dimensions from a dimensional value by equating radians
-- with the dimensionless quantity one.
removeAngles :: Dimensional v ('Dim l m t i th n j pa) a -> Dimensional v (SIDim l m t i th n j) a
removeAngles = coerceAngles

-- | Equates values whose dimensions differ in plane angles by equating
-- radians with the dimensionless quantity one.
--
-- See `removeAngles`, which offers a more specific result type, if you are only interested
-- in ignoring angle dimensions.
coerceAngles :: Dimensional v ('Dim l m t i th n j pa) a -> Dimensional v ('Dim l m t i th n j pa') a
coerceAngles = coerce

(**) :: Floating a => Dimensionless a -> Dimensionless a -> Dimensionless a
Dimensional x ** Dimensional y = Dimensional (x Prelude.** y)

{-
For 'atan2' the operands need not be dimensionless but they must be
of the same type. The result will of course always be a plane angle.
-}

atan2 :: RealFloat a => Quantity d a -> Quantity d a -> PlaneAngle a
atan2 (Dimensional y) (Dimensional x) = Dimensional (Prelude.atan2 y x)

-- | A polymorphic 'Unit' which can be used in place of the coherent
-- base unit of any dimension. This allows polymorphic quantity
-- creation and destruction without exposing the 'Dimensional' constructor.
--
-- `siUnit` is similar but does not include the radians associated
-- with plane angles.
baseUnit :: Num a => Unit d a
baseUnit = Dimensional 1

-- | A polymorphic 'Unit' which can be used in place of the coherent
-- SI base unit of any dimension. This allows polymorphic quantity
-- creation and destruction without exposing the 'Dimensional' constructor.
--
-- `baseUnit` is similar but includes the radians associated
-- with plane angles.
siUnit :: Num a => Unit (SIDim l m t i th n j) a
siUnit = removeAngles baseUnit

{-
The only unit we will define in this module is 'one'.
-}

-- | The unit 'one' has dimension 'DOne' and is the base unit of dimensionless values. 
--
-- As detailed in 7.10 "Values of quantities expressed simply as numbers:
-- the unit one, symbol 1" of <#note1 [1]> the unit one generally does not
-- appear in expressions. However, for us it is necessary to use 'one'
-- as we would any other unit to perform the "boxing" of dimensionless values.
one :: Num a => Unit DOne a
one = baseUnit

{- $constants
For convenience we define some constants for small integer values
that often show up in formulae. We also throw in 'pi' and 'tau' for
good measure.

-}

-- | The constant for zero is polymorphic, allowing
-- it to express zero Length or Capacitance or Velocity etc, in addition
-- to the dimensionless value zero.
_0 :: Num a => Quantity d a
_0 = 0 *~ baseUnit

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

-- | Maps over 'Unit's or 'Quantity's.
--
-- The caller is responsible for ensuring that the supplied function respects the dimensional abstraction.
-- This means that the function must preserve numerical values, or linearly scale them while preserving the origin.
dmap :: (a -> b) -> Dimensional v d a -> Dimensional v d b
dmap f (Dimensional x) = Dimensional (f x)

-- | Convenient conversion between numerical types while retaining dimensional information.
changeRep :: (Real a, Fractional b) => Dimensional v d a -> Dimensional v d b
changeRep = dmap realToFrac

{- $dimension-terms
To facilitate parsing and pretty-printing functions that may wish to operate on term-level representations of dimension,
we provide a means for converting from type-level dimensions to term-level dimensions.

-}

instance (KnownDimension d) => HasDimension (Dimensional v d a) where
  dimension _ = dimension (Proxy :: Proxy d)

instance (KnownDimension d, Functor f) => HasDimension (f (Dimensional v d a)) where
  dimension _ = dimension (Proxy :: Proxy d)

{-
We will conclude by providing a reasonable 'Show' instance for
quantities. The “normalized” unit of the quantity is inferred
from its dimension.

We neglect units since it is unclear how to represent them
in a way that distinguishes them from quantities, or whether that is
even a requirement.
-}
instance (KnownDimension d, Show a) => Show (Quantity d a) where
      show q@(Dimensional x) = let powers = asList $ dimension q
                                   units = ["m", "kg", "s", "A", "K", "mol", "cd", "rad", "sr"]
                                   dims = concat $ zipWith dimUnit units powers
                               in show x ++ dims

{-
The helper function 'dimUnit' defined next conditions a 'String' (unit)
with an exponent, if appropriate.
-}
dimUnit :: String -> Int -> String
dimUnit u n = case n of
                0 -> ""
                1 -> " " ++ u
                n' -> " " ++ u ++ "^" ++ show n'

-- | Applies a scale factor to a 'Unit'.
-- The 'prefix' function will be used by other modules to
-- define the SI prefixes and non-SI units.
-- 
-- Note that supplying zero as a scale factor is invalid, as the library relies
-- upon units forming a group under multiplication. We do not raise an 'error' because
-- doing so would require an additional 'Eq' context.
-- 
-- Supplying negative scale factors is allowed and handled gracefully, but is discouraged
-- on the grounds that it may be unexpected by other readers.
prefix :: Num a => a -> Unit d a -> Unit d a
prefix x (Dimensional y) = Dimensional (x Prelude.* y)

instance NFData a => NFData (Dimensional v d a) -- instance is derived from Generic instance

instance Storable a => Storable (Quantity d a) where
  sizeOf _ = sizeOf (undefined::a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::a)
  {-# INLINE alignment #-}
  poke ptr = poke (castPtr ptr :: Ptr a) . coerce
  {-# INLINE poke #-}
  peek ptr = liftM coerce (peek (castPtr ptr :: Ptr a))
  {-# INLINE peek #-}

{-
Instances for vectors of quantities.
-}
newtype instance U.Vector (Quantity d a)    =  V_Quantity {unVQ :: U.Vector a}
newtype instance U.MVector s (Quantity d a) = MV_Quantity {unMVQ :: U.MVector s a}
instance U.Unbox a => U.Unbox (Quantity d a)

instance (M.MVector U.MVector a) => M.MVector U.MVector (Quantity d a) where
  basicLength          = M.basicLength . unMVQ
  {-# INLINE basicLength #-}
  basicUnsafeSlice m n = MV_Quantity . M.basicUnsafeSlice m n . unMVQ
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps u v    = M.basicOverlaps (unMVQ u) (unMVQ v)
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew       = liftM MV_Quantity . M.basicUnsafeNew
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeRead v    = liftM coerce . M.basicUnsafeRead (unMVQ v)
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite v i = M.basicUnsafeWrite (unMVQ v) i . coerce
  {-# INLINE basicUnsafeWrite #-}
#if MIN_VERSION_vector(0,11,0)
  basicInitialize      = M.basicInitialize . unMVQ
  {-# INLINE basicInitialize #-}
#endif

instance (G.Vector U.Vector a) => G.Vector U.Vector (Quantity d a) where
  basicUnsafeFreeze    = liftM V_Quantity  . G.basicUnsafeFreeze . unMVQ
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw      = liftM MV_Quantity . G.basicUnsafeThaw   . unVQ
  {-# INLINE basicUnsafeThaw #-}
  basicLength          = G.basicLength . unVQ
  {-# INLINE basicLength #-}
  basicUnsafeSlice m n = V_Quantity . G.basicUnsafeSlice m n . unVQ
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM v  = liftM coerce . G.basicUnsafeIndexM (unVQ v)
  {-# INLINE basicUnsafeIndexM #-}
