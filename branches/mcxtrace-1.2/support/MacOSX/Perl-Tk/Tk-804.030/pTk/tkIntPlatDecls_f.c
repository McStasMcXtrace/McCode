#include "Lang.h"
#ifdef WIN32
#include "tkWinInt.h"
#include "tkIntPlatDecls.h"
#include "tkIntPlatDecls_f.h"
static unsigned TkintplatdeclsVSize(void) { return sizeof(TkintplatdeclsVtab);}
static TkintplatdeclsVtab TkintplatdeclsVtable =
{
 TkintplatdeclsVSize,
#define VFUNC(type,name,mem,args) name,
#define VVAR(type,name,mem)      &name,
#include "tkIntPlatDecls.t"
#undef VFUNC
#undef VVAR
};
TkintplatdeclsVtab *TkintplatdeclsVptr;
TkintplatdeclsVtab *TkintplatdeclsVGet() { return TkintplatdeclsVptr = &TkintplatdeclsVtable;}
#endif
