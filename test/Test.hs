module Test where

import Codec.Compression.Zlib.Internal

import Test.Codec.Compression.Zlib.Internal
import Test.Codec.Compression.Zlib.Stream

import Control.Monad
import Data.Word
import Test.QuickCheck
import qualified Data.ByteString.Lazy as BL

import Test.Framework
import Test.Framework.Providers.QuickCheck2

instance Arbitrary Word8 where
  arbitrary = fmap fromInteger arbitrary
  shrink = map fromInteger . shrink . toInteger

maxStrSize = 10000

-- convert a QC size parameter into one for generating long lists,
-- growing inverse exponentially up to maxStrSize
strSize n = floor (maxStrSize * (1 - 2 ** (-fromIntegral n/100)))

instance Arbitrary BL.ByteString where
  arbitrary = sized $ \sz -> resize (strSize sz) $ fmap BL.pack $ listOf $ arbitrary
  shrink = map BL.pack . shrink . BL.unpack

prop_decompress_after_compress w cp dp =
   (w /= zlibFormat || decompressWindowBits dp >= compressWindowBits cp) &&
   -- Zlib decompression has been observed to fail with both compress and decompress
   -- window bits = 8. This seems to be contrary to the docs and to a quick reading
   -- of the zlib source code.
   (decompressWindowBits dp > compressWindowBits cp || decompressWindowBits dp > WindowBits 8) &&
   decompressBufferSize dp > 0 && compressBufferSize cp > 0 ==>
   liftM2 (==) (decompress w dp . compress w cp) id

prop_gziporzlib1 cp dp =
   decompressWindowBits dp > compressWindowBits cp &&
   decompressBufferSize dp > 0 && compressBufferSize cp > 0 ==>
   liftM2 (==) (decompress gzipOrZlibFormat dp . compress zlibFormat cp) id

prop_gziporzlib2 cp dp =
   decompressBufferSize dp > 0 && compressBufferSize cp > 0 ==>
   liftM2 (==) (decompress gzipOrZlibFormat dp . compress gzipFormat cp) id

main = defaultMain [
         testProperty "decompress . compress = id (standard)" prop_decompress_after_compress,
         testProperty "decompress . compress = id (Zlib -> GZipOrZLib)" prop_gziporzlib1,
         testProperty "decompress . compress = id (GZip -> GZipOrZlib)" prop_gziporzlib2
       ]
