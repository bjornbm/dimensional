
= GM calculation =

Several representation can be used to describe a satellite's orbit. Two
of the most popular are the cartesian state vector (position and
velocity vectors) and the keplerian elements. Conversion between the two
representations is fairly straight-forward but requires an assumption
to be made about the universal gravitational constant 'G' and the mass
'M' of the body the satellite is orbiting. In practice they are often
combined into a parameter "mu = GM" where the magnitude of 'mu' is
empirically better known that the magnitudes of 'G' and 'M' individually.

*The problem:* Given two representations of the same satellite orbit -- one
using the cartesian state vector and using keplerian elements, both at the
same epoch -- determine the value of 'mu' used to convert between the two.
{{{

> {-# LANGUAGE NegativeLiterals #-}
> module GM where

> import Numeric.Units.Dimensional.DK.Prelude
> import qualified Prelude

}}}
The state vector describing the orbit at epoch.
{{{

> x     =   4383.9449203752        *~ kilo meter
> y     = -41940.917505092       *~ kilo meter
> z     =     22.790255916589      *~ kilo meter
> x_dot =      3.0575666627812     *~ (kilo meter / second)
> y_dot =      0.32047068607303    *~ (kilo meter / second)
> z_dot =      0.00084729371755294 *~ (kilo meter / second)

}}}
From the state vector we calculate the distance from the reference frame center at epoch and the velocity squared at epoch.
{{{

> r = sqrt (x ^ pos2 + y ^ pos2 + z ^ pos2)
> v = sqrt (x_dot ^ pos2 + y_dot ^ pos2 + z_dot ^ pos2)

}}}
The kinetic energy per unit mass at epoch is a function of the velocity.
{{{

> e_kin :: EnergyPerUnitMass Double
> e_kin = v ^ pos2 / _2

}}}
The only keplerian element we need for this calculation is the semi-major axis.
{{{

> semi_major_axis = 42165.221455 *~ kilo meter

}}}
The expression for 'mu' is obtained by solving the following equation system:

    e_pot = - mu / r,

    e_tot = - mu / 2a,

    e_tot = e_pot + e_kin,

which gives:

    mu = e_kin / (1 / r - 1 / 2a).

{{{

> mu = e_kin / (_1 / r - _1 / (_2 * semi_major_axis))

}}}
Wrap up with a main function showing the value of 'mu' in desired units.
{{{

> main = putStrLn $ "The value used for GM was " ++ show mu

}}}
Loading this module in 'ghci' and running 'main' produces the following output.
{{{
   ___         ___ _
  / _ \ /\  /\/ __(_)
 / /_\// /_/ / /  | |      GHC Interactive, version 6.6.1, for Haskell 98.
/ /_\\/ __  / /___| |      http://www.haskell.org/ghc/
\____/\/ /_/\____/|_|      Type :? for help.

Loading package base ... linking ... done.
[1 of 1] Compiling GM               ( GM.lhs, interpreted )
Ok, modules loaded: GM.
*GM> main
Loading package dimensional-0.5 ... linking ... done.
The value used for GM was 3.986004400008003e14 m^3 s^-2
*GM>
}}}
