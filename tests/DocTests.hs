{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

#if MIN_VERSION_base(4,12,0)
doctestFlags = ["-XNoStarIsType"]
#else
doctestFlags = []
#endif

main :: IO ()
main = glob "src/**/*.hs" >>= (doctest . (doctestFlags++))
