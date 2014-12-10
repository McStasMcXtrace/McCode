#include "tkInt.h"
#include "tkIntDecls.h"
#include "tkIntDecls_f.h"
static unsigned TkintdeclsVSize(void) { return sizeof(TkintdeclsVtab);}
static TkintdeclsVtab TkintdeclsVtable =
{
 TkintdeclsVSize,
#define VFUNC(type,name,mem,args) name,
#define VVAR(type,name,mem)      &name,
#include "tkIntDecls.t"
#undef VFUNC
#undef VVAR
};
TkintdeclsVtab *TkintdeclsVptr;
TkintdeclsVtab *TkintdeclsVGet() { return TkintdeclsVptr = &TkintdeclsVtable;}
