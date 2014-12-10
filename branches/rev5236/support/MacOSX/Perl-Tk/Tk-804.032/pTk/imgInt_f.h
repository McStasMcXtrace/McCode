#ifndef IMGINT_VT
#define IMGINT_VT
typedef struct ImgintVtab
{
 unsigned (*tabSize)(void);
#define VFUNC(type,name,mem,args) type (*mem) args;
#define VVAR(type,name,mem)       type (*mem);
#include "imgInt.t"
#undef VFUNC
#undef VVAR
} ImgintVtab;
extern ImgintVtab *ImgintVptr;
extern ImgintVtab *ImgintVGet(void);
#endif /* IMGINT_VT */
