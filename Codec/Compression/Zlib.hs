-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan.coutts@worc.ox.ac.uk
-- Stability   :  provisional
-- Portability :  portable (H98 + FFI)
--
-- Compression and decompression of data streams in the zlib format.
--
-- The format is described in detail in RFC #1950:
-- <http://www.ietf.org/rfc/rfc1950.txt>
--
-- See also the zlib home page: <http://zlib.net/>
--
-----------------------------------------------------------------------------
module Codec.Compression.Zlib (
  
  -- * Compression
  compress,
  compressWith,
  CompressionLevel(..),
  
  -- * Decompression
  decompress

  ) where

import Data.ByteString.Lazy (ByteString)

import qualified Codec.Compression.Zlib.Internal as Internal
import Codec.Compression.Zlib.Internal hiding (compress, decompress)

decompress :: ByteString -> ByteString
decompress = Internal.decompress Zlib defaultDecompressParams

compress :: ByteString -> ByteString
compress = Internal.compress Zlib defaultCompressParams

compressWith :: CompressionLevel -> ByteString -> ByteString
compressWith level = Internal.compress Zlib defaultCompressParams {
                       compressLevel = level
                     }
