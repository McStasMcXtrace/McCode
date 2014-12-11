#ifdef _TKPLATDECLS
#ifndef TkGenWMConfigureEvent
#ifdef MAC_OSX_TK
VFUNC(void,TkGenWMConfigureEvent,V_TkGenWMConfigureEvent,_ANSI_ARGS_((Tk_Window tkwin,
				int x, int y, int width, int height,
				int flags)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,TkGenWMConfigureEvent,V_TkGenWMConfigureEvent,_ANSI_ARGS_((Tk_Window tkwin,
				int x, int y, int width, int height,
				int flags)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkGenWMConfigureEvent */

#ifndef TkMacConvertEvent
#ifdef MAC_TCL
VFUNC(int,TkMacConvertEvent,V_TkMacConvertEvent,_ANSI_ARGS_((
				EventRecord * eventPtr)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacConvertEvent */

#ifndef TkMacConvertTkEvent
#ifdef MAC_TCL
VFUNC(int,TkMacConvertTkEvent,V_TkMacConvertTkEvent,_ANSI_ARGS_((
				EventRecord * eventPtr, Window window)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacConvertTkEvent */

#ifndef TkMacGetDrawablePort
#ifdef MAC_TCL
VFUNC(GWorldPtr,TkMacGetDrawablePort,V_TkMacGetDrawablePort,_ANSI_ARGS_((Drawable drawable)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacGetDrawablePort */

#ifndef TkMacHaveAppearance
#ifdef MAC_TCL
VFUNC(int,TkMacHaveAppearance,V_TkMacHaveAppearance,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacHaveAppearance */

#ifndef TkMacInitAppleEvents
#ifdef MAC_TCL
VFUNC(void,TkMacInitAppleEvents,V_TkMacInitAppleEvents,_ANSI_ARGS_((
				Tcl_Interp * interp)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacInitAppleEvents */

#ifndef TkMacInitMenus
#ifdef MAC_TCL
VFUNC(void,TkMacInitMenus,V_TkMacInitMenus,_ANSI_ARGS_((Tcl_Interp * interp)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacInitMenus */

#ifndef TkMacInvalClipRgns
#ifdef MAC_TCL
VFUNC(void,TkMacInvalClipRgns,V_TkMacInvalClipRgns,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacInvalClipRgns */

#ifndef TkMacOSXGetDrawablePort
#ifdef MAC_OSX_TK
VFUNC(GWorldPtr,TkMacOSXGetDrawablePort,V_TkMacOSXGetDrawablePort,_ANSI_ARGS_((
				Drawable drawable)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXGetDrawablePort */

#ifndef TkMacOSXGetRootControl
#ifdef MAC_OSX_TK
VFUNC(ControlRef,TkMacOSXGetRootControl,V_TkMacOSXGetRootControl,_ANSI_ARGS_((
				Drawable drawable)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXGetRootControl */

#ifndef TkMacOSXInitAppleEvents
#ifdef MAC_OSX_TK
VFUNC(void,TkMacOSXInitAppleEvents,V_TkMacOSXInitAppleEvents,_ANSI_ARGS_((
				Tcl_Interp * interp)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXInitAppleEvents */

#ifndef TkMacOSXInitMenus
#ifdef MAC_OSX_TK
VFUNC(void,TkMacOSXInitMenus,V_TkMacOSXInitMenus,_ANSI_ARGS_((Tcl_Interp * interp)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXInitMenus */

#ifndef TkMacOSXInvalClipRgns
#ifdef MAC_OSX_TK
VFUNC(void,TkMacOSXInvalClipRgns,V_TkMacOSXInvalClipRgns,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXInvalClipRgns */

#ifndef Tk_AttachHWND
#ifdef __WIN32__
VFUNC(Window,Tk_AttachHWND,V_Tk_AttachHWND,_ANSI_ARGS_((Tk_Window tkwin,
				HWND hwnd)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef Tk_AttachHWND */

#ifndef Tk_GetHINSTANCE
#ifdef __WIN32__
VFUNC(HINSTANCE,Tk_GetHINSTANCE,V_Tk_GetHINSTANCE,_ANSI_ARGS_((void)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef Tk_GetHINSTANCE */

#ifndef Tk_GetHWND
#ifdef __WIN32__
VFUNC(HWND,Tk_GetHWND,V_Tk_GetHWND,_ANSI_ARGS_((Window window)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef Tk_GetHWND */

#ifndef Tk_HWNDToWindow
#ifdef __WIN32__
VFUNC(Tk_Window,Tk_HWNDToWindow,V_Tk_HWNDToWindow,_ANSI_ARGS_((HWND hwnd)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef Tk_HWNDToWindow */

#ifndef Tk_MacOSXIsAppInFront
#ifdef MAC_OSX_TK
VFUNC(int,Tk_MacOSXIsAppInFront,V_Tk_MacOSXIsAppInFront,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef Tk_MacOSXIsAppInFront */

#ifndef Tk_MacOSXSetEmbedHandler
#ifdef MAC_OSX_TK
VFUNC(void,Tk_MacOSXSetEmbedHandler,V_Tk_MacOSXSetEmbedHandler,_ANSI_ARGS_((
				Tk_MacOSXEmbedRegisterWinProc * registerWinProcPtr,
				Tk_MacOSXEmbedGetGrafPortProc * getPortProcPtr,
				Tk_MacOSXEmbedMakeContainerExistProc * containerExistProcPtr,
				Tk_MacOSXEmbedGetClipProc * getClipProc,
				Tk_MacOSXEmbedGetOffsetInParentProc * getOffsetProc)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef Tk_MacOSXSetEmbedHandler */

#ifndef Tk_MacOSXSetupTkNotifier
#ifdef MAC_OSX_TK
VFUNC(void,Tk_MacOSXSetupTkNotifier,V_Tk_MacOSXSetupTkNotifier,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef Tk_MacOSXSetupTkNotifier */

#ifndef Tk_MacOSXTkOwnsCursor
#ifdef MAC_OSX_TK
VFUNC(void,Tk_MacOSXTkOwnsCursor,V_Tk_MacOSXTkOwnsCursor,_ANSI_ARGS_((int tkOwnsIt)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef Tk_MacOSXTkOwnsCursor */

#ifndef Tk_MacOSXTurnOffMenus
#ifdef MAC_OSX_TK
VFUNC(void,Tk_MacOSXTurnOffMenus,V_Tk_MacOSXTurnOffMenus,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef Tk_MacOSXTurnOffMenus */

#ifndef Tk_MacSetEmbedHandler
#ifdef MAC_TCL
VFUNC(void,Tk_MacSetEmbedHandler,V_Tk_MacSetEmbedHandler,_ANSI_ARGS_((
				Tk_MacEmbedRegisterWinProc * registerWinProcPtr,
				Tk_MacEmbedGetGrafPortProc * getPortProcPtr,
				Tk_MacEmbedMakeContainerExistProc * containerExistProcPtr,
				Tk_MacEmbedGetClipProc * getClipProc,
				Tk_MacEmbedGetOffsetInParentProc * getOffsetProc)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef Tk_MacSetEmbedHandler */

#ifndef Tk_MacTkOwnsCursor
#ifdef MAC_TCL
VFUNC(void,Tk_MacTkOwnsCursor,V_Tk_MacTkOwnsCursor,_ANSI_ARGS_((int tkOwnsIt)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef Tk_MacTkOwnsCursor */

#ifndef Tk_MacTurnOffMenus
#ifdef MAC_TCL
VFUNC(void,Tk_MacTurnOffMenus,V_Tk_MacTurnOffMenus,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef Tk_MacTurnOffMenus */

#ifndef Tk_PointerEvent
#ifdef __WIN32__
VFUNC(void,Tk_PointerEvent,V_Tk_PointerEvent,_ANSI_ARGS_((HWND hwnd, int x, int y)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef Tk_PointerEvent */

#ifndef Tk_TranslateWinEvent
#ifdef __WIN32__
VFUNC(int,Tk_TranslateWinEvent,V_Tk_TranslateWinEvent,_ANSI_ARGS_((HWND hwnd,
				UINT message, WPARAM wParam, LPARAM lParam,
				LRESULT * result)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef Tk_TranslateWinEvent */

#endif /* _TKPLATDECLS */
