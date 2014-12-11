#ifndef TKINTPLATDECLS_VT
#define TKINTPLATDECLS_VT
typedef struct TkintplatdeclsVtab
{
 unsigned (*tabSize)(void);
#define VFUNC(type,name,mem,args) type (*mem) args;
#define VVAR(type,name,mem)       type (*mem);
#include "tkIntPlatDecls.t"
#undef VFUNC
#undef VVAR
} TkintplatdeclsVtab;
extern TkintplatdeclsVtab *TkintplatdeclsVptr;
extern TkintplatdeclsVtab *TkintplatdeclsVGet(void);
#endif /* TKINTPLATDECLS_VT */
