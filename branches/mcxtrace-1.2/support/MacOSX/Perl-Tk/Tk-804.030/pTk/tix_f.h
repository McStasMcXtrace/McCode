#ifndef TIX_VT
#define TIX_VT
typedef struct TixVtab
{
 unsigned (*tabSize)(void);
#define VFUNC(type,name,mem,args) type (*mem) args;
#define VVAR(type,name,mem)       type (*mem);
#include "tix.t"
#undef VFUNC
#undef VVAR
} TixVtab;
extern TixVtab *TixVptr;
extern TixVtab *TixVGet(void);
#endif /* TIX_VT */
