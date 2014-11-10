#include "tixPort.h"
#include "tixInt.h"
#include "tixImgXpm.h"
#include "tixImgXpm_f.h"
static unsigned TiximgxpmVSize(void) { return sizeof(TiximgxpmVtab);}
static TiximgxpmVtab TiximgxpmVtable =
{
 TiximgxpmVSize,
#define VFUNC(type,name,mem,args) name,
#define VVAR(type,name,mem)      &name,
#include "tixImgXpm.t"
#undef VFUNC
#undef VVAR
};
TiximgxpmVtab *TiximgxpmVptr;
TiximgxpmVtab *TiximgxpmVGet() { return TiximgxpmVptr = &TiximgxpmVtable;}
