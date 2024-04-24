See also http://pvp.haskell.org/faq

0.7.1.0 Bodigrim <andrew.lelechenko@gmail.com> April 2024

* Split zlib C sources into `zlib-clib` package (thanks @hasufell).
* Use zlib-clib on Windows, unless pkg-config is available.

0.7.0.0 Bodigrim <andrew.lelechenko@gmail.com> February 2024

 * Bump bundled `zlib` to 1.3.1.
 * Hide deprecated constructors of `CompressionLevel`, `Method`, `WindowBits`,
   `MemoryLevel`, `CompressionStrategy` and `Format`.
 * Make `WindowBits`, `MemoryLevel` and `CompressionLevel` newtypes over `Int`.
 * Add smart constructors `rleStrategy` and `fixedStrategy`.
 * Add assorted `Eq`, `Ord`, `Typeable` and `Generic` instances.
 * Make flag `pkg-config` automatic and on by default.
 * Make flag `bundled-c-zlib` to take priority over `pkg-config`.
 * Do not force `bundled-c-zlib` on Windows, but force it for WASM.
 * Strip `install-includes`, do not install any headers.
 * Export `DecompressError` from non-internal modules.
 * Fix compression/decompression of `ByteString` chunks > 4G.
 * Flip flag `non-blocking-ffi` to be `True` be default.

0.6.3.0 Bodigrim <andrew.lelechenko@gmail.com> May 2022

 * Bump bundled zlib to 1.2.12, #48
 * Support base-4.17

0.6.2.3 Emily Pillmore <emilypi@cohomolo.gy> February 2021

 * Add support for bytestring-0.11.0.0

0.6.2.2 Julian Ospald <hasufell@posteo.de> August 2020

 * Bump bundled zlib to 1.2.11, fixes #26
 * New build flag to force use of the bundled zlib C sources, #31
 * Simpler build support for ghcjs, #25
 * Add support for GHC 8.10 / base-4.14, #29

0.6.2.1 Herbert Valerio Riedel <hvr@gnu.org> August 2019

 * Add support for GHC 8.8 / base-4.13

0.6.2 Herbert Valerio Riedel <hvr@gnu.org> March 2018

 * New cabal flag 'pkg-config' for discovering 'zlib` via pkg-config(1) (#16)
 * Use CApiFFI where available for cross-compile friendliness (#14)
 * Change the window bits range from 8..15 to 9..15 (#11)

0.6.1.2 Herbert Valerio Riedel <hvr@gnu.org> October 2016

 * Fix a segfault when reading the stream multithreaded, #7
 * New experimental cabal flag 'non-blocking-ffi' for 'safe' ffi calls

0.6.1.1 Duncan Coutts <duncan@community.haskell.org> April 2015

 * Fixed building with GHC 7.0 and 7.2

0.6.0.2 Duncan Coutts <duncan@community.haskell.org> April 2015

 * Fixed building with GHC 7.0 and 7.2

0.6.1.0 Duncan Coutts <duncan@community.haskell.org> April 2015

 * Support for concatenated gzip files (multiple back-to-back streams)

0.6.0.1 Duncan Coutts <duncan@community.haskell.org> April 2015

 * Fixed building with older GHC
 * Fixed warnings with new GHC
 * Fixed building on Windows
 * Fixed testsuite

0.6.0.0 Duncan Coutts <duncan@community.haskell.org> April 2015

 * New incremental interface for compression and decompression
 * Provide access to unconsumed trailing data
 * Simplified structured error type, and instance of Exception
 * Updated bundled zlib C code to 1.2.8 (used on Windows)
 * Fixed memory leak of zlib z_stream C structure
 * More derived instances (Eq, Show, Typeable, Generic)

0.5.4.2 Duncan Coutts <duncan@community.haskell.org> November 2014

 * Builds with GHC 7.10

0.5.4.1 Duncan Coutts <duncan@community.haskell.org> February 2013

 * Force tail of input when finished decompressing, to help lazy IO

0.5.4.0 Duncan Coutts <duncan@community.haskell.org> September 2012

 * New support for zlib custom dictionaries
