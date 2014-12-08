#ifndef _TKGLUE_VM
#define _TKGLUE_VM
#include "tkGlue_f.h"
#ifndef NO_VTABLES
#ifndef Call_Tk
#  define Call_Tk (*TkglueVptr->V_Call_Tk)
#endif

#ifndef EnterWidgetMethods
#  define EnterWidgetMethods (*TkglueVptr->V_EnterWidgetMethods)
#endif

#ifndef FindTkVarName
#  define FindTkVarName (*TkglueVptr->V_FindTkVarName)
#endif

#ifndef InterpHv
#  define InterpHv (*TkglueVptr->V_InterpHv)
#endif

#ifndef Lang_TkCommand
#  define Lang_TkCommand (*TkglueVptr->V_Lang_TkCommand)
#endif

#ifndef Lang_TkSubCommand
#  define Lang_TkSubCommand (*TkglueVptr->V_Lang_TkSubCommand)
#endif

#ifndef MakeReference
#  define MakeReference (*TkglueVptr->V_MakeReference)
#endif

#ifndef ObjectRef
#  define ObjectRef (*TkglueVptr->V_ObjectRef)
#endif

#ifndef SVtoFont
#  define SVtoFont (*TkglueVptr->V_SVtoFont)
#endif

#ifndef SVtoHWND
#  define SVtoHWND (*TkglueVptr->V_SVtoHWND)
#endif

#ifndef SVtoWindow
#  define SVtoWindow (*TkglueVptr->V_SVtoWindow)
#endif

#ifndef TkToMainWindow
#  define TkToMainWindow (*TkglueVptr->V_TkToMainWindow)
#endif

#ifndef TkToWidget
#  define TkToWidget (*TkglueVptr->V_TkToWidget)
#endif

#ifndef WidgetRef
#  define WidgetRef (*TkglueVptr->V_WidgetRef)
#endif

#ifndef WindowCommand
#  define WindowCommand (*TkglueVptr->V_WindowCommand)
#endif

#ifndef XSTkCommand
#  define XSTkCommand (*TkglueVptr->V_XSTkCommand)
#endif

#ifndef install_vtab
#  define install_vtab (*TkglueVptr->V_install_vtab)
#endif

#endif /* NO_VTABLES */
#endif /* _TKGLUE_VM */
