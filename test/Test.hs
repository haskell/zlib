{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Codec.Compression.Zlib.Internal
import qualified Codec.Compression.Zlib     as Zlib
import qualified Codec.Compression.GZip     as GZip
import qualified Codec.Compression.Zlib.Raw as Raw

import Test.Codec.Compression.Zlib.Internal ()
import Test.Codec.Compression.Zlib.Stream ()

import Test.QuickCheck
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import Control.Monad
import Control.Exception
import Data.Word
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Char8 as BS.Char8
import Prelude hiding (catch)


main :: IO ()
main = defaultMain [
    testGroup "Main property tests" [
      testProperty "decompress . compress = id (standard)"           prop_decompress_after_compress,
      testProperty "decompress . compress = id (Zlib -> GZipOrZLib)" prop_gziporzlib1,
      testProperty "decompress . compress = id (GZip -> GZipOrZlib)" prop_gziporzlib2,
      testProperty "prefixes of valid stream detected as truncated"  prop_truncated
    ],
    testGroup "Special unit tests" [
      testCase "simple gzip case"          test_simple_gzip,
      testCase "detect bad crc"            test_bad_crc,
      testCase "detect non-gzip"           test_non_gzip,
      testCase "detect custom dictionary"  test_custom_dict,
      testCase "handle trailing data"      test_trailing_data,
      testCase "check small input chunks"  test_small_chunks,
      testCase "check exception raised"    test_exception
    ]
  ]


prop_decompress_after_compress :: Format
                               -> CompressParams
                               -> DecompressParams
                               -> Property
prop_decompress_after_compress w cp dp =
   (w /= zlibFormat || decompressWindowBits dp >= compressWindowBits cp) &&
   -- Zlib decompression has been observed to fail with both compress and decompress
   -- window bits = 8. This seems to be contrary to the docs and to a quick reading
   -- of the zlib source code.
   (decompressWindowBits dp > compressWindowBits cp || decompressWindowBits dp > WindowBits 8) &&
   decompressBufferSize dp > 0 && compressBufferSize cp > 0 ==>
   liftM2 (==) (decompress w dp . compress w cp) id


prop_gziporzlib1 :: CompressParams
                 -> DecompressParams
                 -> Property
prop_gziporzlib1 cp dp =
   decompressWindowBits dp > compressWindowBits cp &&
   decompressBufferSize dp > 0 && compressBufferSize cp > 0 ==>
   liftM2 (==) (decompress gzipOrZlibFormat dp . compress zlibFormat cp) id


prop_gziporzlib2 :: CompressParams
                 -> DecompressParams
                 -> Property
prop_gziporzlib2 cp dp =
   decompressBufferSize dp > 0 && compressBufferSize cp > 0 ==>
   liftM2 (==) (decompress gzipOrZlibFormat dp . compress gzipFormat cp) id


prop_truncated :: Format
               -> Property
prop_truncated w =
   forAll shortStrings $ \bs ->
     all (truncated . decomp)
         (init (BL.inits (comp bs)))
  -- All the initial prefixes of a valid compressed stream should be detected
  -- as truncated.
  where
    comp   = compress w defaultCompressParams
    decomp = decompressWithErrors w defaultDecompressParams
    truncated (StreamError TruncatedInput
                 "premature end of compressed stream") = True
    truncated (StreamChunk _ s)                        = truncated s
    truncated _                                        = False

    shortStrings = sized $ \sz -> resize (sz `div` 6) arbitrary


test_simple_gzip :: Assertion
test_simple_gzip =
  assertSampleData "data/hello.gz" gzipFormat $ \decomp ->
    assertDecompressOk decomp

test_bad_crc :: Assertion
test_bad_crc =
  assertSampleData "data/bad-crc.gz" gzipFormat $ \decomp -> do
    (code, msg) <- assertDecompressError decomp
    code @?= DataError
    msg  @?= "incorrect data check"

test_non_gzip :: Assertion
test_non_gzip = do
  assertSampleData "data/not-gzip" gzipFormat $ \decomp -> do
    (code, msg) <- assertDecompressError decomp
    code @?= DataError
    msg  @?= "incorrect header check"

  assertSampleData "data/not-gzip" zlibFormat $ \decomp -> do
    (code, msg) <- assertDecompressError decomp
    code @?= DataError
    msg  @?= "incorrect header check"

  assertSampleData "data/not-gzip" rawFormat $ \decomp -> do
    (code, msg) <- assertDecompressError decomp
    code @?= DataError
    msg  @?= "invalid code lengths set"

  assertSampleData "data/not-gzip" gzipOrZlibFormat $ \decomp -> do
    (code, msg) <- assertDecompressError decomp
    code @?= DataError
    msg  @?= "incorrect header check"

test_custom_dict :: Assertion
test_custom_dict =
  assertSampleData "data/custom-dict.zlib" zlibFormat $ \decomp -> do
    (code, msg) <- assertDecompressError decomp
    code @?= DictionaryRequired
    msg  @?= "custom dictionary needed"

test_trailing_data :: Assertion
test_trailing_data =
  assertSampleData "data/two-files.gzip" gzipFormat $ \decomp -> do
    assertDecompressOk decomp
    case decomp of
      StreamChunk chunk StreamEnd -> chunk @?= BS.Char8.pack "Test 1"
      _                           -> assertFailure "expected single chunk"

test_small_chunks :: Assertion
test_small_chunks = do
  uncompressedFile <- BL.readFile "data/not-gzip"
  GZip.compress (smallChunks uncompressedFile) @?= GZip.compress uncompressedFile
  Zlib.compress (smallChunks uncompressedFile) @?= Zlib.compress uncompressedFile
  Raw.compress  (smallChunks uncompressedFile) @?= Raw.compress uncompressedFile

  GZip.decompress (smallChunks (GZip.compress uncompressedFile)) @?= uncompressedFile
  Zlib.decompress (smallChunks (Zlib.compress uncompressedFile)) @?= uncompressedFile
  Raw.decompress  (smallChunks (Raw.compress  uncompressedFile)) @?= uncompressedFile

  compressedFile   <- BL.readFile "data/hello.gz"
  (GZip.decompress . smallChunks) compressedFile @?= GZip.decompress compressedFile

  where
    smallChunks :: BL.ByteString -> BL.ByteString
    smallChunks = BL.fromChunks . map (\c -> BS.pack [c]) . BL.unpack

test_exception :: Assertion
test_exception =
 (do
    compressedFile <- BL.readFile "data/bad-crc.gz"
    evaluate (BL.length (GZip.decompress compressedFile))
    assertFailure "expected exception")

  `catch` \(ErrorCall message) ->
      message @?= "Codec.Compression.Zlib: incorrect data check"


-------------------
-- QuickCheck Utils

instance Arbitrary Word8 where
  arbitrary = fmap fromInteger arbitrary
  shrink = map fromInteger . shrink . toInteger

maxStrSize :: Double
maxStrSize = 5000

-- convert a QC size parameter into one for generating long lists,
-- growing inverse exponentially up to maxStrSize
strSize :: Int -> Int
strSize n = floor (maxStrSize * (1 - 2 ** (-fromIntegral n/100)))

instance Arbitrary BL.ByteString where
  arbitrary = sized $ \sz -> fmap BL.fromChunks $ listOf $ resize (sz `div` 2) arbitrary
  shrink = map BL.pack . shrink . BL.unpack

instance Arbitrary BS.ByteString where
  arbitrary = sized $ \sz -> resize (strSize sz) $ fmap BS.pack $ listOf $ arbitrary
  shrink = map BS.pack . shrink . BS.unpack


--------------
-- HUnit Utils

assertSampleData :: FilePath -> Format -> (DecompressStream -> Assertion) -> Assertion
assertSampleData file format assertion = do
  compressedFile <- BL.readFile file
  let decomp = decompressWithErrors format defaultDecompressParams compressedFile
  assertion decomp

expected :: String -> String -> Assertion
expected e g = assertFailure ("expected: " ++ e ++ "\nbut got: " ++ g)

assertDecompressOk :: DecompressStream -> Assertion
assertDecompressOk StreamEnd              = return ()
assertDecompressOk (StreamChunk _ s)      = assertDecompressOk s
assertDecompressOk (StreamError code msg) = expected "decompress ok"
                                                     (show code ++ ": " ++ msg)

assertDecompressError :: DecompressStream -> IO (DecompressError, String)
assertDecompressError StreamEnd              = expected "StreamError" "StreamEnd" >> fail ""
assertDecompressError (StreamChunk _ s)      = assertDecompressError s
assertDecompressError (StreamError code msg) = return (code, msg)

deriving instance Show DecompressError
deriving instance Eq DecompressError
