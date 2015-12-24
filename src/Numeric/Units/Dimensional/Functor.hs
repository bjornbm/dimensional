{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,8,0)
-- OverlappingInstances was deprecated by GHC 7.10 in favor of OVERLAPPING pragmas.
#else
{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
#endif

{- |
   Copyright  : Copyright (C) 2006-2015 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn@buckwalter.se
   Stability  : Stable
   Portability: GHC only

Provides a 'Functor' instance for 'Dimensional'.

Note that this instance is dubious, because it allows you to break the dimensional abstraction. See 'dmap' for more information.
 
Note that, while this instance overlaps with that given for 'Dimensionless', it is confluent with that instance.

Note that this is an orphan instance.
-}
module Numeric.Units.Dimensional.Functor {-# DEPRECATED "This orphan instance is being eliminated, along with the module packaging it, in favor of a package flag, functor, which creates it as a proper instance." #-} where

-- If we already have this instance, we won't declare it again.
#if !(FUNCTOR || USE_LINEAR)
import Numeric.Units.Dimensional

-- | A 'Functor' instance for 'Dimensional'.
--
-- Note that this instance is dubious, because it allows you to break the dimensional abstraction. See 'dmap' for more information.
-- 
-- Note that, while this instance overlaps with that given for 'Dimensionless', it is confluent with that instance.
--
-- Note that this is an orphan instance.
instance {-# OVERLAPPING #-} (KnownVariant v) => Functor (Dimensional v d) where
  fmap = dmap
#endif
