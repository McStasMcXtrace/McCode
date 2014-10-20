#include "tixInt.h"
#include "tixInt_f.h"
static unsigned TixintVSize(void) { return sizeof(TixintVtab);}
static TixintVtab TixintVtable =
{
 TixintVSize,
#define VFUNC(type,name,mem,args) name,
#define VVAR(type,name,mem)      &name,
#include "tixInt.t"
#undef VFUNC
#undef VVAR
};
TixintVtab *TixintVptr;
TixintVtab *TixintVGet() { return TixintVptr = &TixintVtable;}
