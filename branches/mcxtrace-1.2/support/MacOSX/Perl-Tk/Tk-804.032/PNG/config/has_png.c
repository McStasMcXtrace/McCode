#include <string.h>
#include <stdlib.h>
#include <png.h>
#include <zlib.h>

int
main(int argc,char *argv[])
{
 png_uint_32 pver = png_access_version_number();
 const char *zver = zlibVersion();
 if (pver > 10200 && zver[0] == '1')
  return 0;
 return 1;
}
