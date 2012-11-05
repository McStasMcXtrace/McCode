#ifdef NEED_FT2BUILD_H
#include <ft2build.h>
#endif
#include <X11/Xlib.h>
#include <X11/Xft/Xft.h>

const char *name = "-*-fixed-medium-r-*-*-*-120-*-*-*-*-iso8859-1";

int main(int argc,char *argv[])
{
 FcPattern	*pattern = XftXlfdParse (name, FcFalse, FcTrue);
 char *family;
 if (XftPatternGetString (pattern, XFT_FAMILY, 0, &family) != XftResultMatch)
  {
   return 1;
  }
 return 0;
}
