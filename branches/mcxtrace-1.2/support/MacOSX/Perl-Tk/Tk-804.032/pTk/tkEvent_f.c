#define TCL_EVENT_IMPLEMENT
#include "Lang.h"
#include "tkEvent.h"
#include "tkEvent_f.h"
static unsigned TkeventVSize(void) { return sizeof(TkeventVtab);}
static TkeventVtab TkeventVtable =
{
 TkeventVSize,
#define VFUNC(type,name,mem,args) name,
#define VVAR(type,name,mem)      &name,
#include "tkEvent.t"
#undef VFUNC
#undef VVAR
};
TkeventVtab *TkeventVptr;
TkeventVtab *TkeventVGet() { return TkeventVptr = &TkeventVtable;}
