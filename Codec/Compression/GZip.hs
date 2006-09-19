-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan.coutts@worc.ox.ac.uk
-- Stability   :  experimental
-- Portability :  portable (H98 + FFI)
--
-- Compression and decompression of data streams in the gzip format.
--
-- The format is described in detail in RFC #1952:
-- <http://www.ietf.org/rfc/rfc1952.txt>
--
-----------------------------------------------------------------------------
module Codec.Compression.GZip (
  
  -- * Compression
  compress,
  compressWith,
  CompressionLevel(..),
  
  -- * Decompression
  decompress
  
  ) where

import Data.ByteString.Lazy (ByteString)

import Codec.Compression.Zlib.Internal as Internal

decompress :: ByteString -> ByteString
decompress = Internal.decompressDefault GZip

compress :: ByteString -> ByteString
compress = Internal.compressDefault GZip DefaultCompression

compressWith ::CompressionLevel -> ByteString -> ByteString
compressWith = Internal.compressDefault GZip
