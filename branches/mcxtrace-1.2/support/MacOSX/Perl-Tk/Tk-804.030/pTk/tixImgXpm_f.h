#ifndef TIXIMGXPM_VT
#define TIXIMGXPM_VT
typedef struct TiximgxpmVtab
{
 unsigned (*tabSize)(void);
#define VFUNC(type,name,mem,args) type (*mem) args;
#define VVAR(type,name,mem)       type (*mem);
#include "tixImgXpm.t"
#undef VFUNC
#undef VVAR
} TiximgxpmVtab;
extern TiximgxpmVtab *TiximgxpmVptr;
extern TiximgxpmVtab *TiximgxpmVGet(void);
#endif /* TIXIMGXPM_VT */
