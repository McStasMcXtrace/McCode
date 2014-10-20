#ifndef TKIMGPHOTO_VT
#define TKIMGPHOTO_VT
typedef struct TkimgphotoVtab
{
 unsigned (*tabSize)(void);
#define VFUNC(type,name,mem,args) type (*mem) args;
#define VVAR(type,name,mem)       type (*mem);
#include "tkImgPhoto.t"
#undef VFUNC
#undef VVAR
} TkimgphotoVtab;
extern TkimgphotoVtab *TkimgphotoVptr;
extern TkimgphotoVtab *TkimgphotoVGet(void);
#endif /* TKIMGPHOTO_VT */
