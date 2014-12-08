#ifndef _TKOPTION_VM
#define _TKOPTION_VM
#include "tkOption_f.h"
#ifndef NO_VTABLES
#ifndef TkOptionClassChanged
#  define TkOptionClassChanged (*TkoptionVptr->V_TkOptionClassChanged)
#endif

#ifndef TkOptionDeadWindow
#  define TkOptionDeadWindow (*TkoptionVptr->V_TkOptionDeadWindow)
#endif

#ifndef Tk_AddOption
#  define Tk_AddOption (*TkoptionVptr->V_Tk_AddOption)
#endif

#ifndef Tk_GetOption
#  define Tk_GetOption (*TkoptionVptr->V_Tk_GetOption)
#endif

#ifndef Tk_OptionObjCmd
#  define Tk_OptionObjCmd (*TkoptionVptr->V_Tk_OptionObjCmd)
#endif

#endif /* NO_VTABLES */
#endif /* _TKOPTION_VM */
