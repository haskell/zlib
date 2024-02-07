-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006-2014 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan@community.haskell.org
--
-- Compression and decompression of data streams in the raw deflate format.
--
-- The format is described in detail in RFC #1951:
-- <http://www.ietf.org/rfc/rfc1951.txt>
--
-- See also the zlib home page: <http://zlib.net/>
--
-----------------------------------------------------------------------------
module Codec.Compression.Zlib.Raw (
  
  -- * Simple compression and decompression
  compress,
  decompress,
  DecompressError(..),

  -- * Extended API with control over compression parameters
  compressWith,
  decompressWith,

  CompressParams(..), defaultCompressParams,
  DecompressParams(..), defaultDecompressParams,

  -- ** The compression parameter types
  CompressionLevel(..),
    defaultCompression,
    noCompression,
    bestSpeed,
    bestCompression,
    compressionLevel,
  Method,
    deflateMethod,
  WindowBits(..),
    defaultWindowBits,
    windowBits,
  MemoryLevel(..),
    defaultMemoryLevel,
    minMemoryLevel,
    maxMemoryLevel,
    memoryLevel,
  CompressionStrategy,
    defaultStrategy,
    filteredStrategy,
    huffmanOnlyStrategy,
    rleStrategy,
    fixedStrategy,

  ) where

import Data.ByteString.Lazy (ByteString)

import qualified Codec.Compression.Zlib.Internal as Internal
import Codec.Compression.Zlib.Internal hiding (compress, decompress)

-- | Decompress a stream of data in the raw deflate format.
decompress :: ByteString -> ByteString
decompress = decompressWith defaultDecompressParams

-- | Like 'Codec.Compression.Zlib.Raw.decompress' but with the ability to specify various decompression
-- parameters.
decompressWith :: DecompressParams -> ByteString -> ByteString
decompressWith = Internal.decompress rawFormat

-- | Compress a stream of data into the raw deflate format.
compress :: ByteString -> ByteString
compress = compressWith defaultCompressParams

-- | Like 'Codec.Compression.Zlib.Raw.compress' but with the ability to specify various decompression
-- parameters.
compressWith :: CompressParams -> ByteString -> ByteString
compressWith = Internal.compress rawFormat
