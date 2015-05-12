{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,8,0)
-- OverlappingInstances was deprecated by GHC 7.10 in favor of OVERLAPPING pragmas.
#else
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Provides a 'Functor' instance for 'Dimensional'.
--
-- Note that this instance is dubious, because it allows you to break the dimensional abstraction. See 'dmap' for more information.
-- 
-- Note that this is an orphan instance.
module Numeric.Units.Dimensional.DK.Functor where

import Numeric.Units.Dimensional.DK

instance {-# OVERLAPPING #-} Functor (Dimensional v d) where
  fmap = dmap
