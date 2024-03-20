#ifndef HS_ZLIB_EXTRAS
#define HS_ZLIB_EXTRAS

#include "zlib.h"

void _hs_zlib_inflateEnd(z_streamp strm);
void _hs_zlib_deflateEnd(z_streamp strm);
#endif
