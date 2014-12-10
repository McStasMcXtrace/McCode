#include "tix.h"
#include "tix_f.h"
static unsigned TixVSize(void) { return sizeof(TixVtab);}
static TixVtab TixVtable =
{
 TixVSize,
#define VFUNC(type,name,mem,args) name,
#define VVAR(type,name,mem)      &name,
#include "tix.t"
#undef VFUNC
#undef VVAR
};
TixVtab *TixVptr;
TixVtab *TixVGet() { return TixVptr = &TixVtable;}
