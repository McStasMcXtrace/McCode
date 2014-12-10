#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <stdio.h>
#include <jconfig.h>
#include <jpeglib.h>

int
main(int argc,char *argv[])
{
 struct jpeg_decompress_struct cinfo;
 struct jpeg_error_mgr jerr;
 cinfo.err = jpeg_std_error(&jerr);
 jpeg_create_decompress(&cinfo);
 jpeg_destroy_decompress(&cinfo);
 return 0;
}
