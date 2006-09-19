-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan.coutts@worc.ox.ac.uk
-- Stability   :  experimental
-- Portability :  portable (H98 + FFI)
--
-- Pure stream based interface to lower level zlib wrapper
--
-----------------------------------------------------------------------------
module Codec.Compression.Zlib.Internal (
  
  -- * Compression and decompression
  compressDefault,
  decompressDefault,
  Stream.Format(..),
  Stream.CompressionLevel(..),
  
  -- * The same but with the full set of parameters
  compressFull,
  decompressFull,
  Stream.Method(..),
  Stream.WindowBits(..),
  Stream.MemoryLevel(..),
  Stream.CompressionStrategy(..),

  ) where

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Base as Base
import Data.ByteString.Base (LazyByteString(LPS))

import qualified Codec.Compression.Zlib.Stream as Stream
import Codec.Compression.Zlib.Stream (Stream)

import Control.Monad (liftM, liftM2)
import Prelude hiding (length)

compressDefault
  :: Stream.Format
  -> Stream.CompressionLevel
  -> Lazy.ByteString
  -> Lazy.ByteString
compressDefault format compressionLevel =
  compressFull format
               compressionLevel
               Stream.Deflated
               Stream.DefaultWindowBits
               Stream.DefaultMemoryLevel
               Stream.DefaultStrategy               

decompressDefault
  :: Stream.Format
  -> Lazy.ByteString
  -> Lazy.ByteString
decompressDefault format =
  decompressFull format
                 Stream.DefaultWindowBits

{-# NOINLINE compressFull #-}
compressFull
  :: Stream.Format
  -> Stream.CompressionLevel
  -> Stream.Method
  -> Stream.WindowBits
  -> Stream.MemoryLevel
  -> Stream.CompressionStrategy
  -> Lazy.ByteString
  -> Lazy.ByteString
compressFull format compLevel method bits memLevel strategy (LPS chunks) =
  Stream.run $ do
    Stream.deflateInit format compLevel method bits memLevel strategy
    liftM LPS (fillBuffers Stream.NoFlush chunks)

  where
  outChunkSize :: Int
  outChunkSize = 32 * 1024 - 16
  
    -- we flick between two states:
    --   * where one or other buffer is empty
    --       - in which case we refill one or both
    --   * where both buffers are non-empty
    --       - in which case we compress until a buffer is empty

  fillBuffers ::
      Stream.Flush
   -> [Strict.ByteString]
   -> Stream [Strict.ByteString]
  fillBuffers flush inChunks = do
    Stream.trace "fillBuffers"
    Stream.dump

    -- in this state there are two possabilities:
    --   * no outbut buffer space is available
    --       - in which case we must make more available
    --   * no input buffer is available
    --       - in which case we must supply more
    Stream.assert "one or other buffer must be empty"
      (liftM2 (||) Stream.inputBufferEmpty Stream.outputBufferFull)

    outputBufferFull <- Stream.outputBufferFull
    if outputBufferFull
      then do -- make output space available and call deflate
              -- note that we must do this even if the input buffer is also empty
              -- since that's a normal circumstance when the final input buffer
              -- has been pushed. Since at that point we flush the compressor and
              -- so may produce a few more output chunks without providing any
              -- more input. The compressor adds a fair bit of latency.
              Stream.trace "no space in output buffer"
              outFPtr <- Stream.unsafeLiftIO (Base.mallocByteString outChunkSize)
              Stream.pushOutputBuffer outFPtr 0 outChunkSize
              Stream.trace "pushed output buffer"
              Stream.dump
              drainBuffers flush inChunks

      else do -- if there is output space available then it must
           -- be the case that we ran out of input space           
           Stream.trace "no space in input buffer"
           case inChunks of
             [] -> Stream.trace "inChunks empty" >> return []
             (Base.PS inFPtr offset length : inChunks') -> do

                Stream.pushInputBuffer inFPtr offset length
                let flush' = if null inChunks' then Stream.Finish else Stream.NoFlush
                Stream.trace "pushed input buffer"
                Stream.dump
                drainBuffers flush' inChunks'

  drainBuffers ::
      Stream.Flush
   -> [Strict.ByteString]
   -> Stream [Strict.ByteString]
  drainBuffers flush inChunks = do
    Stream.trace "drainBuffers"

    status <- Stream.deflate flush

    Stream.trace $ "deflated: " ++ show status
    Stream.dump

    outputBufferBytesAvailable <- Stream.outputBufferBytesAvailable
    if outputBufferBytesAvailable > 0
      then do Stream.trace "positive output bytes available, poping buffer"
              (outFPtr, offset, length) <- Stream.popOutputBuffer
              Stream.trace "poped buffer"
              Stream.dump
              Stream.trace "suspend outChunks"
              outChunks <- Stream.unsafeInterleave (Stream.trace "force outChunks"
                             >> fillBuffers flush inChunks)
              Stream.trace "return chunk"
              return (Base.PS outFPtr offset length : outChunks)
      else do Stream.trace "no output available, filling buffers again"
              fillBuffers flush inChunks

{-# NOINLINE decompressFull #-}
decompressFull
  :: Stream.Format
  -> Stream.WindowBits
  -> Lazy.ByteString
  -> Lazy.ByteString
decompressFull format bits (LPS chunks) =
  Stream.run $ do
    Stream.inflateInit format bits
    liftM LPS (fillBuffers chunks)

  where
  outChunkSize :: Int
  outChunkSize = 32 * 1024 - 16

  fillBuffers ::
      [Strict.ByteString]
   -> Stream [Strict.ByteString]
  fillBuffers inChunks = do
    Stream.trace "fillBuffers"
    Stream.dump

    -- in this state there are two possabilities:
    --   * no outbut buffer space is available
    --       - in which case we must make more available
    --   * no input buffer is available
    --       - in which case we must supply more
    Stream.assert "one or other buffer must be empty"
      (liftM2 (||) Stream.inputBufferEmpty Stream.outputBufferFull)

    outputBufferFull <- Stream.outputBufferFull
    if outputBufferFull
      then do -- make output space available and call inflate
              -- note that we must do this even if the input buffer is also empty
              -- since that's a normal circumstance when the final input buffer
              -- has been pushed. Since at that point we flush the compressor and
              -- so may produce a few more output chunks without providing any
              -- more input. The compressor adds a fair bit of latency.
              Stream.trace "no space in output buffer"
              outFPtr <- Stream.unsafeLiftIO (Base.mallocByteString outChunkSize)
              Stream.pushOutputBuffer outFPtr 0 outChunkSize
              Stream.trace "pushed output buffer"
              Stream.dump
              drainBuffers inChunks

      else do -- if there is output space available then it must
           -- be the case that we ran out of input space           
           Stream.trace "no space in input buffer"
           case inChunks of
             [] -> Stream.trace "inChunks empty" >> return []
             (Base.PS inFPtr offset length : inChunks') -> do

                Stream.pushInputBuffer inFPtr offset length
                Stream.trace "pushed input buffer"
                Stream.dump
                drainBuffers inChunks'

  drainBuffers ::
      [Strict.ByteString]
   -> Stream [Strict.ByteString]
  drainBuffers inChunks = do
    Stream.trace "drainBuffers"

    status <- Stream.inflate Stream.NoFlush

    Stream.trace $ "inflated: " ++ show status
    Stream.dump

    outputBufferBytesAvailable <- Stream.outputBufferBytesAvailable
    if outputBufferBytesAvailable > 0
      then do Stream.trace "positive output bytes available, poping buffer"
              (outFPtr, offset, length) <- Stream.popOutputBuffer
              Stream.trace "poped buffer"
              Stream.dump
              Stream.trace "suspend outChunks"
              outChunks <- Stream.unsafeInterleave (Stream.trace "force outChunks"
                             >> fillBuffers inChunks)
              Stream.trace "return chunk"
              return (Base.PS outFPtr offset length : outChunks)
      else do Stream.trace "no output available, filling buffers again"
              fillBuffers inChunks
