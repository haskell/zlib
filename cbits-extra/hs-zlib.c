#include "hs-zlib.h"

void _hs_zlib_inflateEnd(z_streamp strm) {
  inflateEnd(strm);
}

void _hs_zlib_deflateEnd(z_streamp strm) {
  deflateEnd(strm);
}
