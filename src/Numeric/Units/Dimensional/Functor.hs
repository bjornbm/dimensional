{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
   Copyright  : Copyright (C) 2006-2018 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn@buckwalter.se
   Stability  : Stable
   Portability: GHC only

Provides a 'Functor' instance for 'Dimensional'.

Note that this instance is dubious, because it allows you to break the dimensional abstraction. See 'dmap' for more information.

Note that, while this instance overlaps with that given for 'Dimensionless', it is confluent with that instance.

Note that this is an orphan instance.
-}
module Numeric.Units.Dimensional.Functor where

import Numeric.Units.Dimensional
import Prelude

-- | A 'Functor' instance for 'Dimensional'.
--
-- Note that this instance is dubious, because it allows you to break the dimensional abstraction. See 'dmap' for more information.
--
-- Note that, while this instance overlaps with that given for 'Dimensionless', it is confluent with that instance.
--
-- Note that this is an orphan instance.
instance {-# OVERLAPPING #-} (KnownVariant v) => Functor (Dimensional v d) where
  fmap = dmap
