{-# LANGUAGE StandaloneDeriving #-}

-- | Test code and properties for "Codec.Compression.Zlib.Stream"
--
module Test.Codec.Compression.Zlib.Stream where

import Codec.Compression.Zlib.Stream
import Test.QuickCheck


deriving instance Show Format
instance Arbitrary Format where
  -- GZipOrZlib omitted since it's not symmetric
  arbitrary = elements [gzipFormat, zlibFormat, rawFormat]


deriving instance Show Method
instance Arbitrary Method where
   arbitrary = return deflateMethod


deriving instance Show CompressionLevel
instance Arbitrary CompressionLevel where
  arbitrary = elements $ [defaultCompression, noCompression,
                          bestCompression, bestSpeed]
                      ++ map compressionLevel [1..9]


deriving instance Eq WindowBits
deriving instance Ord WindowBits
deriving instance Show WindowBits
instance Arbitrary WindowBits where
  arbitrary = elements $ defaultWindowBits:map windowBits [8..15]
  shrink DefaultWindowBits = []
  shrink (WindowBits n) = defaultWindowBits:map windowBits [n+1..15]


deriving instance Show MemoryLevel
instance Arbitrary MemoryLevel where
  arbitrary = elements $ [defaultMemoryLevel, minMemoryLevel, maxMemoryLevel]
                      ++ [memoryLevel n | n <- [1..9]]



deriving instance Show CompressionStrategy
instance Arbitrary CompressionStrategy where
  arbitrary = elements $ [defaultStrategy, filteredStrategy, huffmanOnlyStrategy]
                   -- These are disabled by default in the package
                   -- as they are only available with zlib >=1.2
                   -- ++ [RLE, Fixed]
