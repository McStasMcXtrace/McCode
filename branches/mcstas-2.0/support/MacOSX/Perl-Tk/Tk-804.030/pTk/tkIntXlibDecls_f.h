#ifndef TKINTXLIBDECLS_VT
#define TKINTXLIBDECLS_VT
typedef struct TkintxlibdeclsVtab
{
 unsigned (*tabSize)(void);
#define VFUNC(type,name,mem,args) type (*mem) args;
#define VVAR(type,name,mem)       type (*mem);
#include "tkIntXlibDecls.t"
#undef VFUNC
#undef VVAR
} TkintxlibdeclsVtab;
extern TkintxlibdeclsVtab *TkintxlibdeclsVptr;
extern TkintxlibdeclsVtab *TkintxlibdeclsVGet(void);
#endif /* TKINTXLIBDECLS_VT */
