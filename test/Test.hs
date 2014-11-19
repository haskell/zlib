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
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils ()

import Control.Monad
import Control.Exception
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString      as BS
import System.IO
import Prelude hiding (catch)


main :: IO ()
main = defaultMain $
  testGroup "zlib tests" [
    testGroup "property tests" [
      testProperty "decompress . compress = id (standard)"           prop_decompress_after_compress,
      testProperty "decompress . compress = id (Zlib -> GZipOrZLib)" prop_gziporzlib1,
      testProperty "decompress . compress = id (GZip -> GZipOrZlib)" prop_gziporzlib2,
      testProperty "prefixes of valid stream detected as truncated"  prop_truncated
    ],
    testGroup "unit tests" [
      testCase "simple gzip case"          test_simple_gzip,
      testCase "detect bad crc"            test_bad_crc,
      testCase "detect non-gzip"           test_non_gzip,
      testCase "detect custom dictionary"  test_custom_dict,
      testCase "dectect inflate with wrong dict"   test_wrong_dictionary,
      testCase "dectect inflate with right dict"   test_right_dictionary,
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

prop_truncated :: Format -> Property
prop_truncated format =
   forAll shortStrings $ \bs ->
     all (truncated decomp)
         (init (BL.inits (comp bs)))
  -- All the initial prefixes of a valid compressed stream should be detected
  -- as truncated.
  where
    comp   = compress format defaultCompressParams
    decomp = decompressST format defaultDecompressParams
    truncated = foldDecompressStreamWithInput (\_ r -> r) (\_ -> False)
                  (\code _ -> case code of TruncatedInput -> True; _ -> False)

    shortStrings = sized $ \sz -> resize (sz `div` 6) arbitrary


test_simple_gzip :: Assertion
test_simple_gzip =
  withSampleData "hello.gz" $ \hnd ->
    let decomp = decompressIO gzipFormat defaultDecompressParams
     in assertDecompressOk hnd decomp

test_bad_crc :: Assertion
test_bad_crc =
  withSampleData "bad-crc.gz" $ \hnd -> do
    let decomp = decompressIO gzipFormat defaultDecompressParams
    (code, msg) <- assertDecompressError hnd decomp
    code @?= DataError
    msg  @?= "incorrect data check"

test_non_gzip :: Assertion
test_non_gzip = do
  withSampleData "not-gzip" $ \hnd -> do
    let decomp = decompressIO gzipFormat defaultDecompressParams
    (code, msg) <- assertDecompressError hnd decomp
    code @?= DataError
    msg  @?= "incorrect header check"

  withSampleData "not-gzip" $ \hnd -> do
    let decomp = decompressIO zlibFormat defaultDecompressParams
    (code, msg) <- assertDecompressError hnd decomp
    code @?= DataError
    msg  @?= "incorrect header check"

  withSampleData "not-gzip" $ \hnd -> do
    let decomp = decompressIO rawFormat defaultDecompressParams
    (code, msg) <- assertDecompressError hnd decomp
    code @?= DataError
    msg  @?= "invalid code lengths set"

  withSampleData "not-gzip" $ \hnd -> do
    let decomp = decompressIO gzipOrZlibFormat defaultDecompressParams
    (code, msg) <- assertDecompressError hnd decomp
    code @?= DataError
    msg  @?= "incorrect header check"

test_custom_dict :: Assertion
test_custom_dict =
  withSampleData "custom-dict.zlib" $ \hnd -> do
    let decomp = decompressIO zlibFormat defaultDecompressParams
    (code, msg) <- assertDecompressError hnd decomp
    code @?= DictionaryRequired
    msg  @?= "custom dictionary needed"

test_wrong_dictionary :: Assertion
test_wrong_dictionary = do
  withSampleData "custom-dict.zlib" $ \hnd -> do
    let decomp = decompressIO zlibFormat defaultDecompressParams {
                                           decompressDictionary = -- wrong dict!
                                             Just (BS.pack [65,66,67])
                                         }

    (code, msg) <- assertDecompressError hnd decomp
    code @?= DictionaryRequired
    msg  @?= "given dictionary does not match the expected one"

test_right_dictionary :: Assertion
test_right_dictionary = do
  withSampleData "custom-dict.zlib" $ \hnd -> do
    dict <- readSampleData "custom-dict.zlib-dict"
    let decomp = decompressIO zlibFormat defaultDecompressParams {
                                           decompressDictionary =
                                             Just (BL.toStrict dict)
                                         }
    assertDecompressOk hnd decomp

test_trailing_data :: Assertion
test_trailing_data =
  withSampleData "two-files.gzip" $ \hnd -> do
    let decomp = decompressIO gzipFormat defaultDecompressParams
    chunks <- assertDecompressOkChunks hnd decomp
    case chunks of
      [chunk] -> chunk @?= BS.Char8.pack "Test 1"
      _       -> assertFailure "expected single chunk"

test_small_chunks :: Assertion
test_small_chunks = do
  uncompressedFile <- readSampleData "not-gzip"
  GZip.compress (smallChunks uncompressedFile) @?= GZip.compress uncompressedFile
  Zlib.compress (smallChunks uncompressedFile) @?= Zlib.compress uncompressedFile
  Raw.compress  (smallChunks uncompressedFile) @?= Raw.compress uncompressedFile

  GZip.decompress (smallChunks (GZip.compress uncompressedFile)) @?= uncompressedFile
  Zlib.decompress (smallChunks (Zlib.compress uncompressedFile)) @?= uncompressedFile
  Raw.decompress  (smallChunks (Raw.compress  uncompressedFile)) @?= uncompressedFile

  compressedFile   <- readSampleData "hello.gz"
  (GZip.decompress . smallChunks) compressedFile @?= GZip.decompress compressedFile

  where
    smallChunks :: BL.ByteString -> BL.ByteString
    smallChunks = BL.fromChunks . map (\c -> BS.pack [c]) . BL.unpack

test_exception :: Assertion
test_exception =
 (do
    compressedFile <- readSampleData "bad-crc.gz"
    _ <- evaluate (BL.length (GZip.decompress compressedFile))
    assertFailure "expected exception")

  `catch` \(ErrorCall message) ->
      message @?= "Codec.Compression.Zlib: incorrect data check"


--------------
-- HUnit Utils

readSampleData :: FilePath -> IO BL.ByteString
readSampleData file = BL.readFile ("test/data/" ++ file)

withSampleData :: FilePath -> (Handle -> IO a) -> IO a
withSampleData file = withFile ("test/data/" ++ file) ReadMode

expected :: String -> String -> IO a
expected e g = assertFailure ("expected: " ++ e ++ "\nbut got: " ++ g)
            >> fail ""

assertDecompressOk :: Handle -> DecompressStream IO -> Assertion
assertDecompressOk hnd =
    foldDecompressStream
      (BS.hGet hnd 4000 >>=)
      (\_ r -> r)
      (\_ -> return ())
      (\code msg -> expected "decompress ok" (show code ++ ": " ++ msg))

assertDecompressOkChunks :: Handle -> DecompressStream IO -> IO [BS.ByteString]
assertDecompressOkChunks hnd =
    foldDecompressStream
      (BS.hGet hnd 4000 >>=)
      (\chunk -> liftM (chunk:))
      (\_ -> return [])
      (\code msg -> expected "decompress ok" (show code ++ ": " ++ msg))

assertDecompressError :: Handle -> DecompressStream IO -> IO (DecompressError, String)
assertDecompressError hnd =
    foldDecompressStream
      (BS.hGet hnd 4000 >>=)
      (\_ r -> r)
      (\_ -> expected "StreamError" "StreamEnd")
      (\code msg -> return (code, msg))

deriving instance Show DecompressError
deriving instance Eq DecompressError
