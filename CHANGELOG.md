1.3
-------------
* Breaking: Changed the `Show` instances for `UnitName`, `Unit`, `SQuantity` to use non-breaking spaces
  within unit names and between values and their units.

1.2 (2018-11)
-------------
* Add `NoStarIsType` extension and import `Data.Kind.Type` for [GHC 8.6 compitbility](https://github.com/ghc-proposals/ghc-proposals/blob/05721788de9ab6538def68c3c2c9dec50c9f24a8/proposals/0020-no-type-in-type.rst). Abandon compatibility with GHC < 8.

1.1 (2018-03)
-------------
* Added `Semigroup` instances for [GHC 8.4 compatibility](https://ghc.haskell.org/trac/ghc/wiki/Migration/8.4#SemigroupMonoidsuperclasses).
* Breaking: Renamed `Root` type family to `NRoot`. Added `Sqrt` and `Cbrt` type
  synonyms. Added `sqrt` and `cbrt` for term level dimensions.
* Breaking: Changed `Numeric.Units.Dimensional.Prelude` to export dimensionally
  typed `signum`, `recip`, and `logBase` instead of the ones from `Prelude`.
* Breaking: Changed `Numeric.Units.Dimensional.Prelude` to export `(.)` and `id`
  from `Control.Category` instead of from `Prelude`.
* Breaking: Created a `product` function which take the product of a foldable structure of
  `Dimensionless` values. Exported this `product` function from Numeric.Units.Dimensional.Prelude
  instead of the one from `Prelude`.
* Breaking: Changed the `HasDimension` typeclass to require an instance of the new
  `HasDynamicDimension` typeclass.
* Breaking: Added operators for `AnyUnit` to the Numeric.Units.Dimensional.Dynamic
  module which may cause name collisions.
* Breaking: Added dynamic versions of `(*~)`, `(/~)`, and `siUnit` to the Numeric.Units.Dimensional.Dynamic
  module which may cause name collisions.
* Breaking: Removed exports of `nMeter`, `nSecond`, `kilo`, etc from Numeric.Units.Dimensional.UnitNames.
  Access these instead by inspecting the relevant units or prefixes.
* Breaking: Generalized the type of `dimensionlessLength` from
  `(Num a, Foldable f) => f (Dimensional v d a) -> Dimensionless a)` to
  `(Num a, Foldable f) => f b -> Dimensionless a`. This provides a weaker constraint on the type `a`
  and may result in ambiguous types in code that depends on the former less general type.
* Fixed a bug in the definition of the `inHg_NIST`.
* Fixed a bug in the interchange name of the Dalton.
* Added units for the US survey foot, yard, inch, mil, and mile.
* Added the short ton as a unit of mass.
* Clarified that the UCUM acre is based on the US survey foot.
* Added a version of the acre based on the international foot.
* Added `Data`, `Generic`, `Typeable` and `NFData` instances for many ancillary types.
* Added `unQuantity` to the Coercion module to ease unwrapping without
  introducing ambiguous type variables.
* Created explicit representation of metric `Prefix`es.
* Added a multiplicative `Monoid` instance for `AnyQuantity` and for `AnyUnit`.
* Added the `DynQuantity` type to represent possibly valid quantities of statically
  unknown dimension, suitable for performing arithmetic with such quantities.
* Added `nroot` function for term-level dimensions.
* Added the Numeric.Units.Dimensional.Float module with convenient wrappers around functions
  from RealFloat and IEEE for inspecting floating point quantities.
* Added an `AEq` instance for `Quantity`.
* Added `Eq1` and `Ord1` instances for `Quantity`.
* Exposed the name of an 'AnyUnit' without promoting it to a 'Unit' first.
* Exposed a way to convert atomic 'UnitName's back into 'NameAtom's.
* Added the `btu`, a unit of energy.
* Added the `gauss`, a unit of magnetic flux density.
* Added the `angstrom`, a unit of length.
* Relocated git repository to https://github.com/bjornbm/dimensional

1.0.1.3 (2016-09)
-----------------
* Fixed an issue with applying metric prefixes to units with non-rational conversion factors.

1.0.1.2 (2016-05)
-----------------
* Support for GHC 8.0.1-rc4, avoiding GHC Trac issue 12026.
* Added support for stack.

1.0.1.1 (2015-11)
-----------------
* Improved example in readme.

1.0.1.0 (2015-11)
-----------------
* Added Numeric.Units.Dimensional.Coercion module.
* Bumped exact-pi dependency to < 0.5.
* Restored changelog.
* Addressed issues with documentation.

1.0.0.0 (2015-11)
-----------------
* Changed to DataKinds and ClosedTypeFamilies encoding of dimensions.
* Added names and exact values to `Unit`s.
* Added `AnyUnit` and `AnyQuantity` for quantities whose dimension is statically unknown.
* Added Storable and Unbox instances for `Quantity`.
* Added dimensionally-polymorphic `siUnit` for the coherent SI base unit of any dimension.
* Added some additional units.

0.13.0.2 (2015-04)
------------------
*  Corrected definition of lumen.


0.13.0.1 (2014-09)
------------------
*  Bumped time dependency to < 1.6.


0.13 (2014-02)
--------------
*  Bump major version (should have been done in previous version).


0.12.3 (2014-02)
----------------
*  Bump numtype dependency to 1.1 (GHC 7.8.1 compatibility fix).
*  Added `Torque`.
*  Added D.. for the type synonym quantities (e.g., `Angle`).


0.12.2 (2013-11)
----------------
*  Added `FirstMassMoment`, `MomentOfInertia`, `AngularMomentum`.
*  Improved unit numerics.


0.12.1 (2013-07)
----------------
*  Typeable Dimensionals.


0.12 (2013-06)
--------------
*  Polymorphic `_0` (closes issue 39).
*  Added `astronomicalUnit`.
*  Added imperial volume units.
*  Added 'mil' (=inch/1000).
*  Added [`tau`][3].
*  Added `KinematicViscosity`.

[3]: http://tauday.com/tau-manifesto


0.10.1.2 (2011-09)
------------------
*  Bumped time dependency to < 1.5.


0.10.1.2 (2011-08)
------------------
*  Bumped time dependency to < 1.4.


0.10.1 (2011-08)
----------------
GHC 7.2.1 compatibility fix:

*  Increased CGS context-stack to 30.


0.10 (2011-05)
--------------
See the [announcement][2].

[2]: http://flygdynamikern.blogspot.se/2011/05/announce-dimensional-010.html


0.9 (2011-04)
-------------
See the [announcement][1].

[1]: http://flygdynamikern.blogspot.se/2011/04/announce-dimensional-09.html
