{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,8,0)
-- OverlappingInstances was deprecated by GHC 7.10 in favor of OVERLAPPING pragmas.
#else
{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
#endif

-- | Provides a 'Functor' instance for 'Dimensional'.
--
-- Note that this instance is dubious, because it allows you to break the dimensional abstraction. See 'dmap' for more information.
-- 
-- Note that, while this instance overlaps with that given for 'Dimensionless', it is confluent with that instance.
--
-- Note that this is an orphan instance.
module Numeric.Units.Dimensional.DK.Functor where

import Numeric.Units.Dimensional.DK

-- | A 'Functor' instance for 'Dimensional'.
--
-- Note that this instance is dubious, because it allows you to break the dimensional abstraction. See 'dmap' for more information.
-- 
-- Note that, while this instance overlaps with that given for 'Dimensionless', it is confluent with that instance.
--
-- Note that this is an orphan instance.
instance {-# OVERLAPPING #-} Functor (Dimensional v d) where
  fmap = dmap
