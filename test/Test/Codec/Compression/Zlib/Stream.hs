{-# LANGUAGE StandaloneDeriving #-}

-- | Test code and properties for "Codec.Compression.Zlib.Stream"
--
module Test.Codec.Compression.Zlib.Stream where

import Codec.Compression.Zlib.Stream
import Test.QuickCheck


deriving instance Show Format
instance Arbitrary Format where
  -- GZipOrZlib omitted since it's not symmetric
  arbitrary = elements [GZip, Zlib, Raw]


deriving instance Show Method
instance Arbitrary Method where
   arbitrary = return Deflated


deriving instance Show CompressionLevel
instance Arbitrary CompressionLevel where
  arbitrary = elements $ [DefaultCompression, NoCompression, BestCompression]
                      ++ map CompressionLevel [1..9]


deriving instance Eq WindowBits
deriving instance Ord WindowBits
deriving instance Show WindowBits
instance Arbitrary WindowBits where
  arbitrary = elements $ DefaultWindowBits:map WindowBits [8..15]
  shrink DefaultWindowBits = []
  shrink (WindowBits n) = DefaultWindowBits:map WindowBits [n+1..15]


deriving instance Show MemoryLevel
instance Arbitrary MemoryLevel where
  arbitrary = elements $ [DefaultMemoryLevel, MinMemoryLevel, MaxMemoryLevel]
                      ++ [MemoryLevel n | n <- [1..9]]



deriving instance Show CompressionStrategy
instance Arbitrary CompressionStrategy where
  arbitrary = elements $ [DefaultStrategy, Filtered, HuffmanOnly]
                   -- These are disabled by default in the package
                   -- as they are only available with zlib >=1.2
                   -- ++ [RLE, Fixed]

