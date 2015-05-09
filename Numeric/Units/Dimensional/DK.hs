{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
   Copyright  : Copyright (C) 2006-2014 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn.buckwalter@gmail.com
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

We limit ourselves to "Newtonian" physics. We do not attempt to
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
    -- * Physical Dimensions
    -- $dimensions
    Dimension (Dim),
    -- ** Dimension Arithmetic
    -- $dimension-arithmetic
    type (*), type (/), type (^), Root, Recip,
    -- ** Term Level Representation of Dimensions
    -- $dimension-terms
    Dimension' (Dim'), KnownDimension, toSIBasis, getSIBasis,
    -- * Dimensional Arithmetic
    (*~), (/~),
    (^), (^/), (**), (*), (/), (+), (-), (~*), (~/),
    negate, abs, nroot, sqrt, cbrt,
    -- ** Transcendental Functions
    exp, log, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh, atan2,
    -- ** Operations on Collections
    -- $collections
    (*~~), (/~~), sum, mean, dimensionlessLength,
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
    prefix, siUnit, one,
    -- * On 'Functor', and Conversion Between Number Representations
    -- $functor
    dmap, changeRep
  )
  where

import Prelude
  ( Show, Eq, Ord, Enum, Num, Fractional, Floating, Real, RealFloat, Functor, fmap
  , (.), flip, show, (++), String
  , Int, ($), zipWith, uncurry, concat, realToFrac
  )
import qualified Prelude
import Data.List (genericLength)
import Numeric.NumType.DK
  ( NumType (Zero, Pos1, Pos2, Pos3), (+)(), (-)()
  , pos2, pos3
  , KnownNumType, toNum
  )
import qualified Numeric.NumType.DK as N
import Data.Foldable (Foldable(foldr))
import Data.Monoid (Monoid(..))
import Data.Typeable

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
newtype Dimensional (v::Variant) (d::Dimension) a
      = Dimensional a deriving (Eq, Ord, Enum, Typeable)

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
'(~*)' and '(~/)' can be used to scale quantities or units. '(~/)'
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

-- | Represents a physical dimension in the basis of the 7 SI base dimensions, 
-- where the respective dimensions are represented by type variables
-- using the following convention.
--
--  * l: Length
--  * m: Mass
--  * t: Time
--  * i: Electric current
--  * th: Thermodynamic temperature
--  * n: Amount of substance
--  * j: Luminous intensity
--
-- For the equivalent term-level representation, see 'Dimension''
data Dimension = Dim NumType NumType NumType NumType NumType NumType NumType

{- $dimension-synonyms
Using our 'Dimension' data kind we define some type synonyms for convenience.
We start with the base dimensions, others can be found in "Numeric.Units.Dimensional.DK.Quantities".

-}

type DOne                      = 'Dim 'Zero 'Zero 'Zero 'Zero 'Zero 'Zero 'Zero
type DLength                   = 'Dim 'Pos1 'Zero 'Zero 'Zero 'Zero 'Zero 'Zero
type DMass                     = 'Dim 'Zero 'Pos1 'Zero 'Zero 'Zero 'Zero 'Zero
type DTime                     = 'Dim 'Zero 'Zero 'Pos1 'Zero 'Zero 'Zero 'Zero
type DElectricCurrent          = 'Dim 'Zero 'Zero 'Zero 'Pos1 'Zero 'Zero 'Zero
type DThermodynamicTemperature = 'Dim 'Zero 'Zero 'Zero 'Zero 'Pos1 'Zero 'Zero
type DAmountOfSubstance        = 'Dim 'Zero 'Zero 'Zero 'Zero 'Zero 'Pos1 'Zero
type DLuminousIntensity        = 'Dim 'Zero 'Zero 'Zero 'Zero 'Zero 'Zero 'Pos1

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

-- | Multiplication of dimensions corresponds to adding of the base
-- dimensions' exponents.
type family (a::Dimension) * (b::Dimension) where
  DOne * d = d
  d * DOne = d
  ('Dim l  m  t  i  th  n  j) * ('Dim l' m' t' i' th' n' j')
    = 'Dim (l + l') (m + m') (t + t') (i + i') (th + th') (n + n') (j + j')

-- | Division of dimensions corresponds to subtraction of the base
-- dimensions' exponents.
type family (a::Dimension) / (d::Dimension) where
  d / DOne = d
  d / d = DOne
  ('Dim l  m  t  i  th  n  j) / ('Dim l' m' t' i' th' n' j')
    = 'Dim (l - l') (m - m') (t - t') (i - i') (th - th') (n - n') (j - j')

-- | The reciprocal of a dimension is defined as the result of dividing 'DOne' by it,
-- or of negating each of the base dimensions' exponents.
type Recip (d :: Dimension) = DOne / d

-- | Powers of dimensions corresponds to multiplication of the base
-- dimensions' exponents by the exponent.
-- 
-- We limit ourselves to integer powers of Dimensionals as fractional
-- powers make little physical sense.
type family (d::Dimension) ^ (x::NumType) where
  DOne ^ x = DOne
  d ^ 'Zero = DOne
  d ^ 'Pos1 = d
  ('Dim l  m  t  i  th  n  j) ^ x
    = 'Dim (l N.* x) (m N.* x) (t N.* x) (i N.* x) (th N.* x) (n N.* x) (j N.* x)

-- | Roots of dimensions corresponds to division of the base dimensions'
-- exponents by the order(?) of the root.
-- 
-- See 'sqrt', 'cbrt', and 'nroot' for the corresponding term-level operations.
type family Root (d::Dimension) (x::NumType) where
  Root DOne x = DOne
  Root d 'Pos1 = d
  Root ('Dim l  m  t  i  th  n  j) x
    = 'Dim (l N./ x) (m N./ x) (t N./ x) (i N./ x) (th N./ x) (n N./ x) (j N./ x)

{-

= Arithmetic on units and quantities =

Thanks to the arithmetic on physical dimensions having been sorted
out separately a lot of the arithmetic on Dimensionals is straight
forward. In particular the type signatures are much simplified.

Multiplication, division and powers apply to both units and quantities.
-}

(*) :: Num a
    => Dimensional v d a -> Dimensional v d' a -> Dimensional v (d * d') a
Dimensional x * Dimensional y = Dimensional (x Prelude.* y)

(/) :: Fractional a
    => Dimensional v d a -> Dimensional v d' a -> Dimensional v (d / d') a
Dimensional x / Dimensional y = Dimensional (x Prelude./ y)

(^) :: (KnownNumType i, Fractional a)
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

negate :: Num a => Quantity d a -> Quantity d a
negate (Dimensional x) = Dimensional (Prelude.negate x)

(+) :: Num a => Quantity d a -> Quantity d a -> Quantity d a
Dimensional x + Dimensional y = Dimensional (x Prelude.+ y)

(-) :: Num a => Quantity d a -> Quantity d a -> Quantity d a
x - y = x + negate y

{-
Absolute value.
-}

abs :: Num a => Quantity d a -> Quantity d a
abs (Dimensional x) = Dimensional (Prelude.abs x)

{-
Roots of arbitrary (integral) degree. Appears to occasionally be useful
for units as well as quantities.
-}

nroot :: (Floating a, KnownNumType n)
      => Proxy n -> Dimensional v d a -> Dimensional v (Root d n) a
nroot n (Dimensional x) = Dimensional (x Prelude.** (1 Prelude./ N.toNum n))

{-
We provide short-hands for the square and cubic roots.
-}

sqrt :: Floating a => Dimensional v d a -> Dimensional v (Root d 'Pos2) a
sqrt = nroot pos2
cbrt :: Floating a => Dimensional v d a -> Dimensional v (Root d 'Pos3) a
cbrt = nroot pos3

{-
We also provide an operator alternative to nroot for those that
prefer such.
-}

(^/) :: (KnownNumType n, Floating a)
     => Dimensional v d a -> Proxy n -> Dimensional v (Root d n) a
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
(*~~) :: (Functor f, Num a) => f a -> Unit d a -> f (Quantity d a)
xs *~~ u = fmap (*~ u) xs

-- | Applies '(/~)' to all values in a functor.
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

-- | The length of the list as a 'Dimensionless'. This can be useful for
-- purposes of e.g. calculating averages.
dimensionlessLength :: Num a => [Dimensional v d a] -> Dimensionless a
dimensionlessLength = Dimensional . genericLength

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

(**) :: Floating a => Dimensionless a -> Dimensionless a -> Dimensionless a
Dimensional x ** Dimensional y = Dimensional (x Prelude.** y)

{-
For 'atan2' the operands need not be dimensionless but they must be
of the same type. The result will of course always be dimensionless.
-}

atan2 :: RealFloat a => Quantity d a -> Quantity d a -> Dimensionless a
atan2 (Dimensional y) (Dimensional x) = Dimensional (Prelude.atan2 y x)

{-
We add a polymorphic @siUnit@ which can be used in place a concrete
SI unit (combination of SI base units). This allows polymorphic
quantity creation and destruction without exposing the `Dimensional`
constructor.
-}

siUnit :: Num a => Unit d a
siUnit = Dimensional 1

{-
The only unit we will define in this module is 'one'. The unit one
has dimension one and is the base unit of dimensionless values. As
detailed in 7.10 "Values of quantities expressed simply as numbers:
the unit one, symbol 1" of <#note1 [1]> the unit one generally does not
appear in expressions. However, for us it is necessary to use 'one'
as we would any other unit to perform the "boxing" of dimensionless
values.
-}

one :: Num a => Unit DOne a
one = siUnit

{- $constants
For convenience we define some constants for small integer values
that often show up in formulae. We also throw in 'pi' and 'tau' for
good measure.

-}

-- | The constant for zero is polymorphic, allowing
-- it to express zero Length or Capacitance or Velocity etc, in addition
-- to the dimensionless value zero.
_0 :: Num a => Quantity d a
_0 = 0 *~ siUnit

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

-- | A physical dimension, encoded as 7 integers, representing a factorization of the dimension into the
-- 7 SI base dimensions. By convention they are stored in the same order as in the 'Dimension' data kind.
data Dimension' = Dim' !Int !Int !Int !Int !Int !Int !Int deriving (Show,Eq,Ord)

-- | Provides context for converting type-level 'Dimension's into term-level 'Dimension''s.
-- 
-- All validly constructed types of kind 'Dimension' are instances of 'KnownDimension'.
class KnownDimension (d::Dimension) where 
  -- | Obtains a term-level representation of a type-level 'Dimension'.
  toSIBasis :: Proxy d -> Dimension'

instance ( KnownNumType l
         , KnownNumType m
         , KnownNumType t
         , KnownNumType i
         , KnownNumType th
         , KnownNumType n
         , KnownNumType j
         ) => KnownDimension ('Dim l m t i th n j)
  where 
    toSIBasis _ = Dim'
                (toNum (Proxy :: Proxy l))
                (toNum (Proxy :: Proxy m))
                (toNum (Proxy :: Proxy t))
                (toNum (Proxy :: Proxy i))
                (toNum (Proxy :: Proxy th))
                (toNum (Proxy :: Proxy n))
                (toNum (Proxy :: Proxy j))

-- | Obtains a term-level representation of the 'Dimension' of a 'Quantity' or 'Unit'.
getSIBasis :: forall v d a. KnownDimension d => Dimensional v d a -> Dimension'
getSIBasis _ = toSIBasis (Proxy :: Proxy d)

{-
We will conclude by providing a reasonable 'Show' instance for
quantities. The “normalized” unit of the quantity is inferred
from its dimension.

We neglect units since it is unclear how to represent them
in a way that distinguishes them from quantities, or whether that is
even a requirement.
-}
instance (KnownDimension d, Show a) => Show (Quantity d a) where
      show q@(Dimensional x) = let powers = asList $ getSIBasis q
                                   units = ["m", "kg", "s", "A", "K", "mol", "cd"]
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

{-
The helper function asList converts a Dimension' value to a list of integers which may be easier to manipulate.
-}
asList :: Dimension' -> [Int]
asList (Dim' l m t i th n j) = [l, m, t, i, th, n, j]

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
