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
