#include "tkInt.h"
#include "tkInt_f.h"
static unsigned TkintVSize(void) { return sizeof(TkintVtab);}
static TkintVtab TkintVtable =
{
 TkintVSize,
#define VFUNC(type,name,mem,args) name,
#define VVAR(type,name,mem)      &name,
#include "tkInt.t"
#undef VFUNC
#undef VVAR
};
TkintVtab *TkintVptr;
TkintVtab *TkintVGet() { return TkintVptr = &TkintVtable;}
