#include <X11/Xlib.h>
#ifdef _TKINTXLIBDECLS
#include "Lang.h"
#include "tkIntXlibDecls.h"
#include "tkIntXlibDecls_f.h"
static unsigned TkintxlibdeclsVSize(void) { return sizeof(TkintxlibdeclsVtab);}
static TkintxlibdeclsVtab TkintxlibdeclsVtable =
{
 TkintxlibdeclsVSize,
#define VFUNC(type,name,mem,args) name,
#define VVAR(type,name,mem)      &name,
#include "tkIntXlibDecls.t"
#undef VFUNC
#undef VVAR
};
TkintxlibdeclsVtab *TkintxlibdeclsVptr;
TkintxlibdeclsVtab *TkintxlibdeclsVGet() { return TkintxlibdeclsVptr = &TkintxlibdeclsVtable;}
#endif
