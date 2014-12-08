#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>
#include "tkGlue.def"
#include "pTk/tkPort.h"
#include "pTk/tkInt.h"
#include "pTk/tkImgPhoto.h"
#include "pTk/Lang_f.h"
#include "pTk/Xlib.h"
#include "pTk/tk_f.h"
#include "pTk/tkInt_f.h"
#include "pTk/Xlib_f.h"
#include "tkGlue.h"
#include "tkGlue_f.h"
static unsigned TkglueVSize(void) { return sizeof(TkglueVtab);}
static TkglueVtab TkglueVtable =
{
 TkglueVSize,
#define VFUNC(type,name,mem,args) name,
#define VVAR(type,name,mem)      &name,
#include "tkGlue.t"
#undef VFUNC
#undef VVAR
};
TkglueVtab *TkglueVptr;
TkglueVtab *TkglueVGet() { return TkglueVptr = &TkglueVtable;}
