{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Test code and properties for "Codec.Compression.Zlib.Internal"
--
module Test.Codec.Compression.Zlib.Internal where

import Codec.Compression.Zlib.Internal
import Test.Codec.Compression.Zlib.Stream ()
import Test.QuickCheck

import Control.Monad (ap, msum)


deriving instance Show CompressParams

instance Arbitrary CompressParams where
  arbitrary = return CompressParams `ap` arbitrary `ap` arbitrary
                                    `ap` arbitrary `ap` arbitrary
                                    `ap` arbitrary `ap` arbitraryBufferSize

  -- this definition (and the equivalent in DecompressParams below) could be
  -- made nicer using Data.Accessor, but it's probably not worth the
  -- dependency
  shrink cp = msum [
                return (\lv -> cp { compressLevel = lv }) `ap`
                                         shrink (compressLevel cp),
                return (\mt -> cp { compressMethod = mt }) `ap`
                                         shrink (compressMethod cp),
                return (\wb -> cp { compressWindowBits = wb }) `ap`
                                         shrink (compressWindowBits cp),
                return (\ml -> cp { compressMemoryLevel = ml }) `ap`
                                         shrink (compressMemoryLevel cp),
                return (\st -> cp { compressStrategy = st }) `ap`
                                         shrink (compressStrategy cp),
                return (\bs -> cp { compressBufferSize = bs }) `ap`
                                         shrink (compressBufferSize cp)
              ]

arbitraryBufferSize :: Gen Int
arbitraryBufferSize = frequency $ [(10, return n) | n <- [1..1024]] ++
                                  [(20, return n) | n <- [1025..8192]] ++
                                  [(40, return n) | n <- [8193..131072]] ++
                                  [(1, return n) | n <- [131072..1048576]]


deriving instance Show DecompressParams

instance Arbitrary DecompressParams where
  arbitrary = return DecompressParams `ap` arbitrary `ap` arbitraryBufferSize
  shrink dp = msum [
                return (\wb -> dp { decompressWindowBits = wb }) `ap`
                      shrink (decompressWindowBits dp),
                return (\bs -> dp { decompressBufferSize = bs }) `ap`
                      shrink (decompressBufferSize dp)
              ]
