#include "tk.h"
#include "tkDecls_f.h"
static unsigned TkdeclsVSize(void) { return sizeof(TkdeclsVtab);}
static TkdeclsVtab TkdeclsVtable =
{
 TkdeclsVSize,
#define VFUNC(type,name,mem,args) name,
#define VVAR(type,name,mem)      &name,
#include "tkDecls.t"
#undef VFUNC
#undef VVAR
};
TkdeclsVtab *TkdeclsVptr;
TkdeclsVtab *TkdeclsVGet() { return TkdeclsVptr = &TkdeclsVtable;}
