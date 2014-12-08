#ifndef TKOPTION_VT
#define TKOPTION_VT
typedef struct TkoptionVtab
{
 unsigned (*tabSize)(void);
#define VFUNC(type,name,mem,args) type (*mem) args;
#define VVAR(type,name,mem)       type (*mem);
#include "tkOption.t"
#undef VFUNC
#undef VVAR
} TkoptionVtab;
extern TkoptionVtab *TkoptionVptr;
extern TkoptionVtab *TkoptionVGet(void);
#endif /* TKOPTION_VT */
