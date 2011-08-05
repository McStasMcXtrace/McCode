#ifndef TIXINT_VT
#define TIXINT_VT
typedef struct TixintVtab
{
 unsigned (*tabSize)(void);
#define VFUNC(type,name,mem,args) type (*mem) args;
#define VVAR(type,name,mem)       type (*mem);
#include "tixInt.t"
#undef VFUNC
#undef VVAR
} TixintVtab;
extern TixintVtab *TixintVptr;
extern TixintVtab *TixintVGet(void);
#endif /* TIXINT_VT */
