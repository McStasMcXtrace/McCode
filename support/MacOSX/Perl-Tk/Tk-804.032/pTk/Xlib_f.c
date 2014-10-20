/* Pull in system's real include files if possible */
#include "Lang.h"
#include <X11/X.h>
#include <X11/Xlib.h>
#if !defined(_TKINTXLIBDECLS)
#include <X11/Xutil.h>
#ifndef _XLIB
#include "Xlib.h"
#endif
#include "Xlib_f.h"
static unsigned XlibVSize(void) { return sizeof(XlibVtab);}
static XlibVtab XlibVtable =
{
 XlibVSize,
#define VFUNC(type,name,mem,args) name,
#define VVAR(type,name,mem)      &name,
#include "Xlib.t"
#undef VFUNC
#undef VVAR
};
XlibVtab *XlibVptr;
XlibVtab *XlibVGet() { return XlibVptr = &XlibVtable;}
#endif
