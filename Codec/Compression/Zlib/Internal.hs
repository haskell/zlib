-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006-2007 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan.coutts@worc.ox.ac.uk
-- Stability   :  provisional
-- Portability :  portable (H98 + FFI)
--
-- Pure stream based interface to lower level zlib wrapper
--
-----------------------------------------------------------------------------
module Codec.Compression.Zlib.Internal (

  -- * Compression with the full set of parameters
  compress,
  CompressParams(..),
  defaultCompressParams,

  -- * Decompression with the full set of parameters
  decompress,
  DecompressParams(..),
  defaultDecompressParams,

  -- * The compression parameter types
  Stream.Format(..),
  Stream.CompressionLevel(..),
  Stream.Method(..),
  Stream.WindowBits(..),
  Stream.MemoryLevel(..),
  Stream.CompressionStrategy(..),

  ) where

import Prelude hiding (length)
import Control.Monad (when)
import Control.Exception (assert)
import qualified Data.ByteString.Lazy as L
#ifdef BYTESTRING_IN_BASE
import qualified Data.ByteString.Base as S
#else
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString.Internal as S
#endif

import qualified Codec.Compression.Zlib.Stream as Stream
import Codec.Compression.Zlib.Stream (Stream)

-- | The full set of parameters for compression. The defaults are
-- 'defaultCompressParams'.
--
data CompressParams = CompressParams {
  compressLevel       :: Stream.CompressionLevel,
  compressMethod      :: Stream.Method,
  compressWindowBits  :: Stream.WindowBits,
  compressMemoryLevel :: Stream.MemoryLevel,
  compressStrategy    :: Stream.CompressionStrategy
}

-- | The full set of parameters for decompression. The defaults are
-- 'defaultCompressParams'.
--
data DecompressParams = DecompressParams {
  decompressWindowBits :: Stream.WindowBits
}

defaultCompressParams :: CompressParams
defaultCompressParams = CompressParams {
  compressLevel       = Stream.DefaultCompression,
  compressMethod      = Stream.Deflated,
  compressWindowBits  = Stream.DefaultWindowBits,
  compressMemoryLevel = Stream.DefaultMemoryLevel,
  compressStrategy    = Stream.DefaultStrategy
}

defaultDecompressParams :: DecompressParams
defaultDecompressParams = DecompressParams {
  decompressWindowBits = Stream.DefaultWindowBits
}

{-# NOINLINE compress #-}
compress
  :: Stream.Format
  -> CompressParams
  -> L.ByteString
  -> L.ByteString
compress format
  (CompressParams compLevel method bits memLevel strategy)
  input =
  L.fromChunks $ Stream.run $ do
    Stream.deflateInit format compLevel method bits memLevel strategy
    case L.toChunks input of
      [] -> fillBuffers []
      S.PS inFPtr offset length : chunks -> do
        Stream.pushInputBuffer inFPtr offset length
        fillBuffers chunks

  where
  outChunkSize :: Int
#ifdef BYTESTRING_IN_BASE
  outChunkSize = 16 * 1024 - 16
#else
  outChunkSize = 16 * 1024 - L.chunkOverhead
#endif

    -- we flick between two states:
    --   * where one or other buffer is empty
    --       - in which case we refill one or both
    --   * where both buffers are non-empty
    --       - in which case we compress until a buffer is empty

  fillBuffers ::
      [S.ByteString]
   -> Stream [S.ByteString]
  fillBuffers inChunks = do
    Stream.consistencyCheck

    -- in this state there are two possabilities:
    --   * no outbut buffer space is available
    --       - in which case we must make more available
    --   * no input buffer is available
    --       - in which case we must supply more
    inputBufferEmpty <- Stream.inputBufferEmpty
    outputBufferFull <- Stream.outputBufferFull

    assert (inputBufferEmpty || outputBufferFull) $ return ()

    when outputBufferFull $ do
      outFPtr <- Stream.unsafeLiftIO (S.mallocByteString outChunkSize)
      Stream.pushOutputBuffer outFPtr 0 outChunkSize

    if inputBufferEmpty
      then case inChunks of
             [] -> drainBuffers []
             S.PS inFPtr offset length : inChunks' -> do
                Stream.pushInputBuffer inFPtr offset length
                drainBuffers inChunks'
      else drainBuffers inChunks


  drainBuffers ::
      [S.ByteString]
   -> Stream [S.ByteString]
  drainBuffers inChunks = do

    inputBufferEmpty' <- Stream.inputBufferEmpty
    outputBufferFull' <- Stream.outputBufferFull
    assert(not outputBufferFull'
       && (null inChunks || not inputBufferEmpty')) $ return ()
    -- this invariant guarantees we can always make forward progress
    -- and that therefore a BufferError is impossible

    let flush = if null inChunks then Stream.Finish else Stream.NoFlush
    status <- Stream.deflate flush

    case status of
      Stream.Ok -> do
        outputBufferFull <- Stream.outputBufferFull
        if outputBufferFull
          then do (outFPtr, offset, length) <- Stream.popOutputBuffer
                  outChunks <- Stream.unsafeInterleave (fillBuffers inChunks)
                  return (S.PS outFPtr offset length : outChunks)
          else do fillBuffers inChunks

      Stream.StreamEnd -> do
        inputBufferEmpty <- Stream.inputBufferEmpty
        assert inputBufferEmpty $ return ()
        outputBufferBytesAvailable <- Stream.outputBufferBytesAvailable
        if outputBufferBytesAvailable > 0
          then do (outFPtr, offset, length) <- Stream.popOutputBuffer
                  Stream.finalise
                  return [S.PS outFPtr offset length]
          else do Stream.finalise
                  return []
      Stream.BufferError -> fail "BufferError should be impossible!"
      Stream.NeedDict    -> fail "NeedDict is impossible!"


{-# NOINLINE decompress #-}
decompress
  :: Stream.Format
  -> DecompressParams
  -> L.ByteString
  -> L.ByteString
decompress format (DecompressParams bits) input =
  L.fromChunks $ Stream.run $ do
    Stream.inflateInit format bits
    case L.toChunks input of
      [] -> fillBuffers []
      S.PS inFPtr offset length : chunks -> do
        Stream.pushInputBuffer inFPtr offset length
        fillBuffers chunks

  where
  outChunkSize :: Int
#ifdef BYTESTRING_IN_BASE
  outChunkSize = 32 * 1024 - 16
#else
  outChunkSize = 32 * 1024 - L.chunkOverhead
#endif

    -- we flick between two states:
    --   * where one or other buffer is empty
    --       - in which case we refill one or both
    --   * where both buffers are non-empty
    --       - in which case we compress until a buffer is empty

  fillBuffers ::
      [S.ByteString]
   -> Stream [S.ByteString]
  fillBuffers inChunks = do

    -- in this state there are two possabilities:
    --   * no outbut buffer space is available
    --       - in which case we must make more available
    --   * no input buffer is available
    --       - in which case we must supply more
    inputBufferEmpty <- Stream.inputBufferEmpty
    outputBufferFull <- Stream.outputBufferFull

    assert (inputBufferEmpty || outputBufferFull) $ return ()

    when outputBufferFull $ do
      outFPtr <- Stream.unsafeLiftIO (S.mallocByteString outChunkSize)
      Stream.pushOutputBuffer outFPtr 0 outChunkSize

    if inputBufferEmpty
      then case inChunks of
             [] -> drainBuffers []
             S.PS inFPtr offset length : inChunks' -> do
                Stream.pushInputBuffer inFPtr offset length
                drainBuffers inChunks'
      else drainBuffers inChunks


  drainBuffers ::
      [S.ByteString]
   -> Stream [S.ByteString]
  drainBuffers inChunks = do

    inputBufferEmpty' <- Stream.inputBufferEmpty
    outputBufferFull' <- Stream.outputBufferFull
    assert(not outputBufferFull'
       && (null inChunks || not inputBufferEmpty')) $ return ()
    -- this invariant guarantees we can always make forward progress or at
    -- least if a BufferError does occur that it must be due to a premature EOF

    status <- Stream.inflate Stream.NoFlush

    case status of
      Stream.Ok -> do
        outputBufferFull <- Stream.outputBufferFull
        if outputBufferFull
          then do (outFPtr, offset, length) <- Stream.popOutputBuffer
                  outChunks <- Stream.unsafeInterleave (fillBuffers inChunks)
                  return (S.PS outFPtr offset length : outChunks)
          else do fillBuffers inChunks

      Stream.StreamEnd -> do
        -- Note that there may be input bytes still available if the stream
        -- is embeded in some other data stream. Here we just silently discard
        -- any trailing data.
        outputBufferBytesAvailable <- Stream.outputBufferBytesAvailable
        if outputBufferBytesAvailable > 0
          then do (outFPtr, offset, length) <- Stream.popOutputBuffer
                  Stream.finalise
                  return [S.PS outFPtr offset length]
          else do Stream.finalise
                  return []
      Stream.BufferError -> fail "premature end of compressed stream"
      Stream.NeedDict    -> fail "compressed stream needs a custom dictionary"
