#ifndef TKPLATDECLS_VT
#define TKPLATDECLS_VT
#include "tkPlatDecls.h"
typedef struct TkplatdeclsVtab
{
 unsigned (*tabSize)(void);
#define VFUNC(type,name,mem,args) type (*mem) args;
#define VVAR(type,name,mem)       type (*mem);
#include "tkPlatDecls.t"
#undef VFUNC
#undef VVAR
} TkplatdeclsVtab;
extern TkplatdeclsVtab *TkplatdeclsVptr;
extern TkplatdeclsVtab *TkplatdeclsVGet(void);
#endif /* TKPLATDECLS_VT */
