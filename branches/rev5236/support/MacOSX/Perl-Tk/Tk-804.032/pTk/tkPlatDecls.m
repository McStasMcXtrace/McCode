#ifndef _TKPLATDECLS_VM
#define _TKPLATDECLS_VM
#include "tkPlatDecls_f.h"
#ifndef NO_VTABLES
#ifndef TkGenWMConfigureEvent
#  define TkGenWMConfigureEvent (*TkplatdeclsVptr->V_TkGenWMConfigureEvent)
#endif

#ifndef TkMacConvertEvent
#  define TkMacConvertEvent (*TkplatdeclsVptr->V_TkMacConvertEvent)
#endif

#ifndef TkMacConvertTkEvent
#  define TkMacConvertTkEvent (*TkplatdeclsVptr->V_TkMacConvertTkEvent)
#endif

#ifndef TkMacGetDrawablePort
#  define TkMacGetDrawablePort (*TkplatdeclsVptr->V_TkMacGetDrawablePort)
#endif

#ifndef TkMacHaveAppearance
#  define TkMacHaveAppearance (*TkplatdeclsVptr->V_TkMacHaveAppearance)
#endif

#ifndef TkMacInitAppleEvents
#  define TkMacInitAppleEvents (*TkplatdeclsVptr->V_TkMacInitAppleEvents)
#endif

#ifndef TkMacInitMenus
#  define TkMacInitMenus (*TkplatdeclsVptr->V_TkMacInitMenus)
#endif

#ifndef TkMacInvalClipRgns
#  define TkMacInvalClipRgns (*TkplatdeclsVptr->V_TkMacInvalClipRgns)
#endif

#ifndef TkMacOSXGetDrawablePort
#  define TkMacOSXGetDrawablePort (*TkplatdeclsVptr->V_TkMacOSXGetDrawablePort)
#endif

#ifndef TkMacOSXGetRootControl
#  define TkMacOSXGetRootControl (*TkplatdeclsVptr->V_TkMacOSXGetRootControl)
#endif

#ifndef TkMacOSXInitAppleEvents
#  define TkMacOSXInitAppleEvents (*TkplatdeclsVptr->V_TkMacOSXInitAppleEvents)
#endif

#ifndef TkMacOSXInitMenus
#  define TkMacOSXInitMenus (*TkplatdeclsVptr->V_TkMacOSXInitMenus)
#endif

#ifndef TkMacOSXInvalClipRgns
#  define TkMacOSXInvalClipRgns (*TkplatdeclsVptr->V_TkMacOSXInvalClipRgns)
#endif

#ifndef Tk_AttachHWND
#  define Tk_AttachHWND (*TkplatdeclsVptr->V_Tk_AttachHWND)
#endif

#ifndef Tk_GetHINSTANCE
#  define Tk_GetHINSTANCE (*TkplatdeclsVptr->V_Tk_GetHINSTANCE)
#endif

#ifndef Tk_GetHWND
#  define Tk_GetHWND (*TkplatdeclsVptr->V_Tk_GetHWND)
#endif

#ifndef Tk_HWNDToWindow
#  define Tk_HWNDToWindow (*TkplatdeclsVptr->V_Tk_HWNDToWindow)
#endif

#ifndef Tk_MacOSXIsAppInFront
#  define Tk_MacOSXIsAppInFront (*TkplatdeclsVptr->V_Tk_MacOSXIsAppInFront)
#endif

#ifndef Tk_MacOSXSetEmbedHandler
#  define Tk_MacOSXSetEmbedHandler (*TkplatdeclsVptr->V_Tk_MacOSXSetEmbedHandler)
#endif

#ifndef Tk_MacOSXSetupTkNotifier
#  define Tk_MacOSXSetupTkNotifier (*TkplatdeclsVptr->V_Tk_MacOSXSetupTkNotifier)
#endif

#ifndef Tk_MacOSXTkOwnsCursor
#  define Tk_MacOSXTkOwnsCursor (*TkplatdeclsVptr->V_Tk_MacOSXTkOwnsCursor)
#endif

#ifndef Tk_MacOSXTurnOffMenus
#  define Tk_MacOSXTurnOffMenus (*TkplatdeclsVptr->V_Tk_MacOSXTurnOffMenus)
#endif

#ifndef Tk_MacSetEmbedHandler
#  define Tk_MacSetEmbedHandler (*TkplatdeclsVptr->V_Tk_MacSetEmbedHandler)
#endif

#ifndef Tk_MacTkOwnsCursor
#  define Tk_MacTkOwnsCursor (*TkplatdeclsVptr->V_Tk_MacTkOwnsCursor)
#endif

#ifndef Tk_MacTurnOffMenus
#  define Tk_MacTurnOffMenus (*TkplatdeclsVptr->V_Tk_MacTurnOffMenus)
#endif

#ifndef Tk_PointerEvent
#  define Tk_PointerEvent (*TkplatdeclsVptr->V_Tk_PointerEvent)
#endif

#ifndef Tk_TranslateWinEvent
#  define Tk_TranslateWinEvent (*TkplatdeclsVptr->V_Tk_TranslateWinEvent)
#endif

#endif /* NO_VTABLES */
#endif /* _TKPLATDECLS_VM */
