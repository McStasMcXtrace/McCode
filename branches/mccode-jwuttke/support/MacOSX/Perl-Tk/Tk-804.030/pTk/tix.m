#ifndef _TIX_VM
#define _TIX_VM
#include "tix_f.h"
#ifndef NO_VTABLES
#ifndef TixDItemParseProc
#  define TixDItemParseProc (*TixVptr->V_TixDItemParseProc)
#endif

#ifndef TixDItemPrintProc
#  define TixDItemPrintProc (*TixVptr->V_TixDItemPrintProc)
#endif

#ifndef TixDItemStyleParseProc
#  define TixDItemStyleParseProc (*TixVptr->V_TixDItemStyleParseProc)
#endif

#ifndef TixDItemStylePrintProc
#  define TixDItemStylePrintProc (*TixVptr->V_TixDItemStylePrintProc)
#endif

#ifndef TixGetStringFromObj
#  define TixGetStringFromObj (*TixVptr->V_TixGetStringFromObj)
#endif

#ifndef Tix_ArgcError
#  define Tix_ArgcError (*TixVptr->V_Tix_ArgcError)
#endif

#ifndef Tix_CreateSubWindow
#  define Tix_CreateSubWindow (*TixVptr->V_Tix_CreateSubWindow)
#endif

#ifndef Tix_DrawAnchorLines
#  define Tix_DrawAnchorLines (*TixVptr->V_Tix_DrawAnchorLines)
#endif

#ifndef Tix_GetRenderBuffer
#  define Tix_GetRenderBuffer (*TixVptr->V_Tix_GetRenderBuffer)
#endif

#ifndef Tix_HandleSubCmds
#  define Tix_HandleSubCmds (*TixVptr->V_Tix_HandleSubCmds)
#endif

#ifndef Tix_LinkListAppend
#  define Tix_LinkListAppend (*TixVptr->V_Tix_LinkListAppend)
#endif

#ifndef Tix_LinkListDelete
#  define Tix_LinkListDelete (*TixVptr->V_Tix_LinkListDelete)
#endif

#ifndef Tix_LinkListDeleteRange
#  define Tix_LinkListDeleteRange (*TixVptr->V_Tix_LinkListDeleteRange)
#endif

#ifndef Tix_LinkListFind
#  define Tix_LinkListFind (*TixVptr->V_Tix_LinkListFind)
#endif

#ifndef Tix_LinkListFindAndDelete
#  define Tix_LinkListFindAndDelete (*TixVptr->V_Tix_LinkListFindAndDelete)
#endif

#ifndef Tix_LinkListInit
#  define Tix_LinkListInit (*TixVptr->V_Tix_LinkListInit)
#endif

#ifndef Tix_LinkListInsert
#  define Tix_LinkListInsert (*TixVptr->V_Tix_LinkListInsert)
#endif

#ifndef Tix_LinkListIteratorInit
#  define Tix_LinkListIteratorInit (*TixVptr->V_Tix_LinkListIteratorInit)
#endif

#ifndef Tix_LinkListNext
#  define Tix_LinkListNext (*TixVptr->V_Tix_LinkListNext)
#endif

#ifndef Tix_LinkListStart
#  define Tix_LinkListStart (*TixVptr->V_Tix_LinkListStart)
#endif

#ifndef Tix_SimpleListAppend
#  define Tix_SimpleListAppend (*TixVptr->V_Tix_SimpleListAppend)
#endif

#ifndef Tix_SimpleListDelete
#  define Tix_SimpleListDelete (*TixVptr->V_Tix_SimpleListDelete)
#endif

#ifndef Tix_SimpleListDeleteRange
#  define Tix_SimpleListDeleteRange (*TixVptr->V_Tix_SimpleListDeleteRange)
#endif

#ifndef Tix_SimpleListFind
#  define Tix_SimpleListFind (*TixVptr->V_Tix_SimpleListFind)
#endif

#ifndef Tix_SimpleListFindAndDelete
#  define Tix_SimpleListFindAndDelete (*TixVptr->V_Tix_SimpleListFindAndDelete)
#endif

#ifndef Tix_SimpleListInit
#  define Tix_SimpleListInit (*TixVptr->V_Tix_SimpleListInit)
#endif

#ifndef Tix_SimpleListInsert
#  define Tix_SimpleListInsert (*TixVptr->V_Tix_SimpleListInsert)
#endif

#ifndef Tix_SimpleListIteratorInit
#  define Tix_SimpleListIteratorInit (*TixVptr->V_Tix_SimpleListIteratorInit)
#endif

#ifndef Tix_SimpleListNext
#  define Tix_SimpleListNext (*TixVptr->V_Tix_SimpleListNext)
#endif

#ifndef Tix_SimpleListStart
#  define Tix_SimpleListStart (*TixVptr->V_Tix_SimpleListStart)
#endif

#endif /* NO_VTABLES */
#endif /* _TIX_VM */
