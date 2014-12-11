#include "imgInt.h"
#include "imgInt_f.h"
static unsigned ImgintVSize(void) { return sizeof(ImgintVtab);}
static ImgintVtab ImgintVtable =
{
 ImgintVSize,
#define VFUNC(type,name,mem,args) name,
#define VVAR(type,name,mem)      &name,
#include "imgInt.t"
#undef VFUNC
#undef VVAR
};
ImgintVtab *ImgintVptr;
ImgintVtab *ImgintVGet() { return ImgintVptr = &ImgintVtable;}
