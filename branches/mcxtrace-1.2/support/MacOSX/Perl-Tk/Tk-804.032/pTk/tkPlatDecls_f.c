#include "Lang.h"
#ifdef WIN32
#include "tkWin.h"
#include "tkPlatDecls_f.h"
static unsigned TkplatdeclsVSize(void) { return sizeof(TkplatdeclsVtab);}
static TkplatdeclsVtab TkplatdeclsVtable =
{
 TkplatdeclsVSize,
#define VFUNC(type,name,mem,args) name,
#define VVAR(type,name,mem)      &name,
#include "tkPlatDecls.t"
#undef VFUNC
#undef VVAR
};
TkplatdeclsVtab *TkplatdeclsVptr;
TkplatdeclsVtab *TkplatdeclsVGet() { return TkplatdeclsVptr = &TkplatdeclsVtable;}
#endif
