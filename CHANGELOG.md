vNext
-----
* Breaking: Changed Numeric.Units.Dimensional.Prelude to export a dimensionally
  typed `recip` instead of the one from `Prelude`.
* Breaking: Changed the `HasDimension` typeclass to require an instance of the new
  `HasDynamicDimension` typeclass.
* Added `Data`, `Generic`, `Typeable` and `NFData` instances for many ancillary types.
* Added `unQuantity` to the Coercion module to ease unwrapping without
  introducing ambiguous type variables.
* Created explicit representation of metric `Prefix`es.
* Added a multiplicative `Monoid` instance for `AnyQuantity` and for `AnyUnit`.
* Added the `DynQuantity` type to represent possibly valid quantities of statically
  unknown dimension, suitable for performing arithmetic with such quantities.
* Relocated git repository to https://github.com/bjornbm/dimensional

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
