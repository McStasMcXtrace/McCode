#ifdef _TKINTPLATDECLS
#ifndef TkAboutDlg
#ifdef MAC_OSX_TK
VFUNC(void,TkAboutDlg,V_TkAboutDlg,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,TkAboutDlg,V_TkAboutDlg,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkAboutDlg */

#ifndef TkAlignImageData
#ifdef __WIN32__
VFUNC(char *,TkAlignImageData,V_TkAlignImageData,_ANSI_ARGS_((XImage * image,
				int alignment, int bitOrder)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkAlignImageData */

#ifndef TkCreateXEventSource
#if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK))
VFUNC(void,TkCreateXEventSource,V_TkCreateXEventSource,_ANSI_ARGS_((void)))
#endif /* #if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK)) */
#endif /* #ifndef TkCreateXEventSource */

#ifndef TkFreeWindowId
#if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK))
VFUNC(void,TkFreeWindowId,V_TkFreeWindowId,_ANSI_ARGS_((TkDisplay * dispPtr,
				Window w)))
#endif /* #if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK)) */
#endif /* #ifndef TkFreeWindowId */

#ifndef TkFreeXId
#if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK))
VFUNC(void,TkFreeXId,V_TkFreeXId,_ANSI_ARGS_((TkDisplay * dispPtr)))
#endif /* #if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK)) */
#endif /* #ifndef TkFreeXId */

#ifndef TkGenWMDestroyEvent
#ifdef MAC_OSX_TK
VFUNC(void,TkGenWMDestroyEvent,V_TkGenWMDestroyEvent,_ANSI_ARGS_((Tk_Window tkwin)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,TkGenWMDestroyEvent,V_TkGenWMDestroyEvent,_ANSI_ARGS_((Tk_Window tkwin)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkGenWMDestroyEvent */

#ifndef TkGenerateActivateEvents
#ifdef MAC_OSX_TK
VFUNC(void,TkGenerateActivateEvents,V_TkGenerateActivateEvents,_ANSI_ARGS_((
				TkWindow * winPtr, int active)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,TkGenerateActivateEvents,V_TkGenerateActivateEvents,_ANSI_ARGS_((
				TkWindow * winPtr, int active)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,TkGenerateActivateEvents,V_TkGenerateActivateEvents,_ANSI_ARGS_((
				TkWindow * winPtr, int active)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkGenerateActivateEvents */

#ifndef TkGenerateButtonEvent
#ifdef MAC_OSX_TK
VFUNC(int,TkGenerateButtonEvent,V_TkGenerateButtonEvent,_ANSI_ARGS_((int x, int y,
				Window window, unsigned int state)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(int,TkGenerateButtonEvent,V_TkGenerateButtonEvent,_ANSI_ARGS_((int x, int y,
				Window window, unsigned int state)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkGenerateButtonEvent */

#ifndef TkGetTransientMaster
#ifdef MAC_OSX_TK
VFUNC(Window,TkGetTransientMaster,V_TkGetTransientMaster,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(Window,TkGetTransientMaster,V_TkGetTransientMaster,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkGetTransientMaster */

#ifndef TkInitXId
#if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK))
VFUNC(void,TkInitXId,V_TkInitXId,_ANSI_ARGS_((TkDisplay * dispPtr)))
#endif /* #if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK)) */
#endif /* #ifndef TkInitXId */

#ifndef TkMacButtonKeyState
#ifdef MAC_TCL
VFUNC(unsigned int,TkMacButtonKeyState,V_TkMacButtonKeyState,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacButtonKeyState */

#ifndef TkMacClearMenubarActive
#ifdef MAC_TCL
VFUNC(void,TkMacClearMenubarActive,V_TkMacClearMenubarActive,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacClearMenubarActive */

#ifndef TkMacContainerId
#ifdef MAC_TCL
VFUNC(MacDrawable *,TkMacContainerId,V_TkMacContainerId,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacContainerId */

#ifndef TkMacDispatchMenuEvent
#ifdef MAC_TCL
VFUNC(int,TkMacDispatchMenuEvent,V_TkMacDispatchMenuEvent,_ANSI_ARGS_((int menuID,
				int index)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacDispatchMenuEvent */

#ifndef TkMacDoHLEvent
#ifdef MAC_TCL
VFUNC(void,TkMacDoHLEvent,V_TkMacDoHLEvent,_ANSI_ARGS_((EventRecord * theEvent)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacDoHLEvent */

#ifndef TkMacGenerateTime
#ifdef MAC_TCL
VFUNC(Time,TkMacGenerateTime,V_TkMacGenerateTime,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacGenerateTime */

#ifndef TkMacGetHostToplevel
#ifdef MAC_TCL
VFUNC(MacDrawable *,TkMacGetHostToplevel,V_TkMacGetHostToplevel,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacGetHostToplevel */

#ifndef TkMacGetScrollbarGrowWindow
#ifdef MAC_TCL
VFUNC(TkWindow *,TkMacGetScrollbarGrowWindow,V_TkMacGetScrollbarGrowWindow,_ANSI_ARGS_((
				TkWindow * winPtr)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacGetScrollbarGrowWindow */

#ifndef TkMacGetXWindow
#ifdef MAC_TCL
VFUNC(Window,TkMacGetXWindow,V_TkMacGetXWindow,_ANSI_ARGS_((WindowRef macWinPtr)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacGetXWindow */

#ifndef TkMacGrowToplevel
#ifdef MAC_TCL
VFUNC(int,TkMacGrowToplevel,V_TkMacGrowToplevel,_ANSI_ARGS_((WindowRef whichWindow,
				Point start)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacGrowToplevel */

#ifndef TkMacHandleMenuSelect
#ifdef MAC_TCL
VFUNC(void,TkMacHandleMenuSelect,V_TkMacHandleMenuSelect,_ANSI_ARGS_((long mResult,
				int optionKeyPressed)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacHandleMenuSelect */

#ifndef TkMacHandleTearoffMenu
#ifdef MAC_TCL
VFUNC(void,TkMacHandleTearoffMenu,V_TkMacHandleTearoffMenu,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacHandleTearoffMenu */

#ifndef TkMacInstallCursor
#ifdef MAC_TCL
VFUNC(void,TkMacInstallCursor,V_TkMacInstallCursor,_ANSI_ARGS_((int resizeOverride)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacInstallCursor */

#ifndef TkMacInvalidateWindow
#ifdef MAC_TCL
VFUNC(void,TkMacInvalidateWindow,V_TkMacInvalidateWindow,_ANSI_ARGS_((
				MacDrawable * macWin, int flag)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacInvalidateWindow */

#ifndef TkMacIsCharacterMissing
#ifdef MAC_TCL
VFUNC(int,TkMacIsCharacterMissing,V_TkMacIsCharacterMissing,_ANSI_ARGS_((Tk_Font tkfont,
				unsigned int searchChar)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacIsCharacterMissing */

#ifndef TkMacMakeRealWindowExist
#ifdef MAC_TCL
VFUNC(void,TkMacMakeRealWindowExist,V_TkMacMakeRealWindowExist,_ANSI_ARGS_((
				TkWindow * winPtr)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacMakeRealWindowExist */

#ifndef TkMacMakeStippleMap
#ifdef MAC_TCL
VFUNC(BitMapPtr,TkMacMakeStippleMap,V_TkMacMakeStippleMap,_ANSI_ARGS_((Drawable d1,
				Drawable d2)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacMakeStippleMap */

#ifndef TkMacMenuClick
#ifdef MAC_TCL
VFUNC(void,TkMacMenuClick,V_TkMacMenuClick,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacMenuClick */

#ifndef TkMacOSXButtonKeyState
#ifdef MAC_OSX_TK
VFUNC(unsigned int,TkMacOSXButtonKeyState,V_TkMacOSXButtonKeyState,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXButtonKeyState */

#ifndef TkMacOSXClearMenubarActive
#ifdef MAC_OSX_TK
VFUNC(void,TkMacOSXClearMenubarActive,V_TkMacOSXClearMenubarActive,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXClearMenubarActive */

#ifndef TkMacOSXContainerId
#ifdef MAC_OSX_TK
VFUNC(MacDrawable *,TkMacOSXContainerId,V_TkMacOSXContainerId,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXContainerId */

#ifndef TkMacOSXDispatchMenuEvent
#ifdef MAC_OSX_TK
VFUNC(int,TkMacOSXDispatchMenuEvent,V_TkMacOSXDispatchMenuEvent,_ANSI_ARGS_((int menuID,
				int index)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXDispatchMenuEvent */

#ifndef TkMacOSXDoHLEvent
#ifdef MAC_OSX_TK
VFUNC(int,TkMacOSXDoHLEvent,V_TkMacOSXDoHLEvent,_ANSI_ARGS_((
				EventRecord * theEvent)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXDoHLEvent */

#ifndef TkMacOSXGetCapture
#ifdef MAC_OSX_TK
VFUNC(Tk_Window,TkMacOSXGetCapture,V_TkMacOSXGetCapture,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXGetCapture */

#ifndef TkMacOSXGetHostToplevel
#ifdef MAC_OSX_TK
VFUNC(MacDrawable *,TkMacOSXGetHostToplevel,V_TkMacOSXGetHostToplevel,_ANSI_ARGS_((
				TkWindow * winPtr)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXGetHostToplevel */

#ifndef TkMacOSXGetXWindow
#ifdef MAC_OSX_TK
VFUNC(Window,TkMacOSXGetXWindow,V_TkMacOSXGetXWindow,_ANSI_ARGS_((WindowRef macWinPtr)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXGetXWindow */

#ifndef TkMacOSXGrowToplevel
#ifdef MAC_OSX_TK
VFUNC(int,TkMacOSXGrowToplevel,V_TkMacOSXGrowToplevel,_ANSI_ARGS_((
				WindowRef whichWindow, Point start)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXGrowToplevel */

#ifndef TkMacOSXHandleMenuSelect
#ifdef MAC_OSX_TK
VFUNC(void,TkMacOSXHandleMenuSelect,V_TkMacOSXHandleMenuSelect,_ANSI_ARGS_((long mResult,
				int optionKeyPressed)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXHandleMenuSelect */

#ifndef TkMacOSXHandleTearoffMenu
#ifdef MAC_OSX_TK
VFUNC(void,TkMacOSXHandleTearoffMenu,V_TkMacOSXHandleTearoffMenu,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXHandleTearoffMenu */

#ifndef TkMacOSXInstallCursor
#ifdef MAC_OSX_TK
VFUNC(void,TkMacOSXInstallCursor,V_TkMacOSXInstallCursor,_ANSI_ARGS_((
				int resizeOverride)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXInstallCursor */

#ifndef TkMacOSXInvalidateWindow
#ifdef MAC_OSX_TK
VFUNC(void,TkMacOSXInvalidateWindow,V_TkMacOSXInvalidateWindow,_ANSI_ARGS_((
				MacDrawable * macWin, int flag)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXInvalidateWindow */

#ifndef TkMacOSXIsCharacterMissing
#ifdef MAC_OSX_TK
VFUNC(int,TkMacOSXIsCharacterMissing,V_TkMacOSXIsCharacterMissing,_ANSI_ARGS_((
				Tk_Font tkfont, unsigned int searchChar)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXIsCharacterMissing */

#ifndef TkMacOSXMakeRealWindowExist
#ifdef MAC_OSX_TK
VFUNC(void,TkMacOSXMakeRealWindowExist,V_TkMacOSXMakeRealWindowExist,_ANSI_ARGS_((
				TkWindow * winPtr)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXMakeRealWindowExist */

#ifndef TkMacOSXMakeStippleMap
#ifdef MAC_OSX_TK
VFUNC(BitMapPtr,TkMacOSXMakeStippleMap,V_TkMacOSXMakeStippleMap,_ANSI_ARGS_((Drawable d1,
				Drawable d2)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXMakeStippleMap */

#ifndef TkMacOSXMenuClick
#ifdef MAC_OSX_TK
VFUNC(void,TkMacOSXMenuClick,V_TkMacOSXMenuClick,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXMenuClick */

#ifndef TkMacOSXPreprocessMenu
#ifdef MAC_OSX_TK
VFUNC(void,TkMacOSXPreprocessMenu,V_TkMacOSXPreprocessMenu,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXPreprocessMenu */

#ifndef TkMacOSXRegisterOffScreenWindow
#ifdef MAC_OSX_TK
VFUNC(void,TkMacOSXRegisterOffScreenWindow,V_TkMacOSXRegisterOffScreenWindow,_ANSI_ARGS_((
				Window window, GWorldPtr portPtr)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXRegisterOffScreenWindow */

#ifndef TkMacOSXResizable
#ifdef MAC_OSX_TK
VFUNC(int,TkMacOSXResizable,V_TkMacOSXResizable,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXResizable */

#ifndef TkMacOSXSetHelpMenuItemCount
#ifdef MAC_OSX_TK
VFUNC(void,TkMacOSXSetHelpMenuItemCount,V_TkMacOSXSetHelpMenuItemCount,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXSetHelpMenuItemCount */

#ifndef TkMacOSXSetScrollbarGrow
#ifdef MAC_OSX_TK
VFUNC(void,TkMacOSXSetScrollbarGrow,V_TkMacOSXSetScrollbarGrow,_ANSI_ARGS_((
				TkWindow * winPtr, int flag)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXSetScrollbarGrow */

#ifndef TkMacOSXSetUpClippingRgn
#ifdef MAC_OSX_TK
VFUNC(void,TkMacOSXSetUpClippingRgn,V_TkMacOSXSetUpClippingRgn,_ANSI_ARGS_((
				Drawable drawable)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXSetUpClippingRgn */

#ifndef TkMacOSXSetUpGraphicsPort
#ifdef MAC_OSX_TK
VFUNC(void,TkMacOSXSetUpGraphicsPort,V_TkMacOSXSetUpGraphicsPort,_ANSI_ARGS_((GC gc,
				GWorldPtr destPort)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXSetUpGraphicsPort */

#ifndef TkMacOSXUnregisterMacWindow
#ifdef MAC_OSX_TK
VFUNC(void,TkMacOSXUnregisterMacWindow,V_TkMacOSXUnregisterMacWindow,_ANSI_ARGS_((
				WindowRef portPtr)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXUnregisterMacWindow */

#ifndef TkMacOSXUpdateClipRgn
#ifdef MAC_OSX_TK
VFUNC(void,TkMacOSXUpdateClipRgn,V_TkMacOSXUpdateClipRgn,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXUpdateClipRgn */

#ifndef TkMacOSXUseMenuID
#ifdef MAC_OSX_TK
VFUNC(int,TkMacOSXUseMenuID,V_TkMacOSXUseMenuID,_ANSI_ARGS_((short macID)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXUseMenuID */

#ifndef TkMacOSXVisableClipRgn
#ifdef MAC_OSX_TK
VFUNC(RgnHandle,TkMacOSXVisableClipRgn,V_TkMacOSXVisableClipRgn,_ANSI_ARGS_((
				TkWindow * winPtr)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXVisableClipRgn */

#ifndef TkMacOSXWinBounds
#ifdef MAC_OSX_TK
VFUNC(void,TkMacOSXWinBounds,V_TkMacOSXWinBounds,_ANSI_ARGS_((TkWindow * winPtr,
				Rect * geometry)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXWinBounds */

#ifndef TkMacOSXWindowOffset
#ifdef MAC_OSX_TK
VFUNC(void,TkMacOSXWindowOffset,V_TkMacOSXWindowOffset,_ANSI_ARGS_((WindowRef wRef,
				int * xOffset, int * yOffset)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXWindowOffset */

#ifndef TkMacOSXZoomToplevel
#ifdef MAC_OSX_TK
VFUNC(int,TkMacOSXZoomToplevel,V_TkMacOSXZoomToplevel,_ANSI_ARGS_((
				WindowPtr whichWindow, Point where,
				short zoomPart)))
#endif /* #ifdef MAC_OSX_TK */
#endif /* #ifndef TkMacOSXZoomToplevel */

#ifndef TkMacPreprocessMenu
#ifdef MAC_TCL
VFUNC(void,TkMacPreprocessMenu,V_TkMacPreprocessMenu,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacPreprocessMenu */

#ifndef TkMacRegisterOffScreenWindow
#ifdef MAC_TCL
VFUNC(void,TkMacRegisterOffScreenWindow,V_TkMacRegisterOffScreenWindow,_ANSI_ARGS_((
				Window window, GWorldPtr portPtr)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacRegisterOffScreenWindow */

#ifndef TkMacResizable
#ifdef MAC_TCL
VFUNC(int,TkMacResizable,V_TkMacResizable,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacResizable */

#ifndef TkMacSetHelpMenuItemCount
#ifdef MAC_TCL
VFUNC(void,TkMacSetHelpMenuItemCount,V_TkMacSetHelpMenuItemCount,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacSetHelpMenuItemCount */

#ifndef TkMacSetScrollbarGrow
#ifdef MAC_TCL
VFUNC(void,TkMacSetScrollbarGrow,V_TkMacSetScrollbarGrow,_ANSI_ARGS_((TkWindow * winPtr,
				int flag)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacSetScrollbarGrow */

#ifndef TkMacSetUpClippingRgn
#ifdef MAC_TCL
VFUNC(void,TkMacSetUpClippingRgn,V_TkMacSetUpClippingRgn,_ANSI_ARGS_((Drawable drawable)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacSetUpClippingRgn */

#ifndef TkMacSetUpGraphicsPort
#ifdef MAC_TCL
VFUNC(void,TkMacSetUpGraphicsPort,V_TkMacSetUpGraphicsPort,_ANSI_ARGS_((GC gc)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacSetUpGraphicsPort */

#ifndef TkMacUnregisterMacWindow
#ifdef MAC_TCL
VFUNC(void,TkMacUnregisterMacWindow,V_TkMacUnregisterMacWindow,_ANSI_ARGS_((
				GWorldPtr portPtr)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacUnregisterMacWindow */

#ifndef TkMacUpdateClipRgn
#ifdef MAC_TCL
VFUNC(void,TkMacUpdateClipRgn,V_TkMacUpdateClipRgn,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacUpdateClipRgn */

#ifndef TkMacUseMenuID
#ifdef MAC_TCL
VFUNC(int,TkMacUseMenuID,V_TkMacUseMenuID,_ANSI_ARGS_((short macID)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacUseMenuID */

#ifndef TkMacVisableClipRgn
#ifdef MAC_TCL
VFUNC(RgnHandle,TkMacVisableClipRgn,V_TkMacVisableClipRgn,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacVisableClipRgn */

#ifndef TkMacWinBounds
#ifdef MAC_TCL
VFUNC(void,TkMacWinBounds,V_TkMacWinBounds,_ANSI_ARGS_((TkWindow * winPtr,
				Rect * geometry)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacWinBounds */

#ifndef TkMacWindowOffset
#ifdef MAC_TCL
VFUNC(void,TkMacWindowOffset,V_TkMacWindowOffset,_ANSI_ARGS_((WindowRef wRef,
				int * xOffset, int * yOffset)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacWindowOffset */

#ifndef TkMacZoomToplevel
#ifdef MAC_TCL
VFUNC(int,TkMacZoomToplevel,V_TkMacZoomToplevel,_ANSI_ARGS_((WindowPtr whichWindow,
				Point where, short zoomPart)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkMacZoomToplevel */

#ifndef TkPointerDeadWindow
#ifdef MAC_OSX_TK
VFUNC(void,TkPointerDeadWindow,V_TkPointerDeadWindow,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,TkPointerDeadWindow,V_TkPointerDeadWindow,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,TkPointerDeadWindow,V_TkPointerDeadWindow,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkPointerDeadWindow */

#ifndef TkSendCleanup
#if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK))
VFUNC(void,TkSendCleanup,V_TkSendCleanup,_ANSI_ARGS_((TkDisplay * dispPtr)))
#endif /* #if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK)) */
#endif /* #ifndef TkSendCleanup */

#ifndef TkSetMacColor
#ifdef MAC_OSX_TK
VFUNC(int,TkSetMacColor,V_TkSetMacColor,_ANSI_ARGS_((unsigned long pixel,
				RGBColor * macColor)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(int,TkSetMacColor,V_TkSetMacColor,_ANSI_ARGS_((unsigned long pixel,
				RGBColor * macColor)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkSetMacColor */

#ifndef TkSetPixmapColormap
#ifdef __WIN32__
VFUNC(void,TkSetPixmapColormap,V_TkSetPixmapColormap,_ANSI_ARGS_((Pixmap pixmap,
				Colormap colormap)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkSetPixmapColormap */

#ifndef TkSetWMName
#ifdef MAC_OSX_TK
VFUNC(void,TkSetWMName,V_TkSetWMName,_ANSI_ARGS_((TkWindow * winPtr,
				Tk_Uid titleUid)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,TkSetWMName,V_TkSetWMName,_ANSI_ARGS_((TkWindow * winPtr,
				Tk_Uid titleUid)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkSetWMName */

#ifndef TkSuspendClipboard
#ifdef MAC_OSX_TK
VFUNC(void,TkSuspendClipboard,V_TkSuspendClipboard,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,TkSuspendClipboard,V_TkSuspendClipboard,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkSuspendClipboard */

#ifndef TkUnixContainerId
#if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK))
VFUNC(Window,TkUnixContainerId,V_TkUnixContainerId,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK)) */
#endif /* #ifndef TkUnixContainerId */

#ifndef TkUnixDoOneXEvent
#if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK))
VFUNC(int,TkUnixDoOneXEvent,V_TkUnixDoOneXEvent,_ANSI_ARGS_((Tcl_Time * timePtr)))
#endif /* #if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK)) */
#endif /* #ifndef TkUnixDoOneXEvent */

#ifndef TkUnixSetMenubar
#if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK))
VFUNC(void,TkUnixSetMenubar,V_TkUnixSetMenubar,_ANSI_ARGS_((Tk_Window tkwin,
				Tk_Window menubar)))
#endif /* #if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK)) */
#endif /* #ifndef TkUnixSetMenubar */

#ifndef TkWinCancelMouseTimer
#ifdef __WIN32__
VFUNC(void,TkWinCancelMouseTimer,V_TkWinCancelMouseTimer,_ANSI_ARGS_((void)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinCancelMouseTimer */

#ifndef TkWinClipboardRender
#ifdef __WIN32__
VFUNC(void,TkWinClipboardRender,V_TkWinClipboardRender,_ANSI_ARGS_((
				TkDisplay * dispPtr, UINT format)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinClipboardRender */

#ifndef TkWinDialogDebug
#ifdef __WIN32__
VFUNC(void,TkWinDialogDebug,V_TkWinDialogDebug,_ANSI_ARGS_((int debug)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinDialogDebug */

#ifndef TkWinEmbeddedEventProc
#ifdef __WIN32__
VFUNC(LRESULT,TkWinEmbeddedEventProc,V_TkWinEmbeddedEventProc,_ANSI_ARGS_((HWND hwnd,
				UINT message, WPARAM wParam, LPARAM lParam)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinEmbeddedEventProc */

#ifndef TkWinFillRect
#ifdef __WIN32__
VFUNC(void,TkWinFillRect,V_TkWinFillRect,_ANSI_ARGS_((HDC dc, int x, int y,
				int width, int height, int pixel)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinFillRect */

#ifndef TkWinGetBorderPixels
#ifdef __WIN32__
VFUNC(COLORREF,TkWinGetBorderPixels,V_TkWinGetBorderPixels,_ANSI_ARGS_((Tk_Window tkwin,
				Tk_3DBorder border, int which)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinGetBorderPixels */

#ifndef TkWinGetDrawableDC
#ifdef __WIN32__
VFUNC(HDC,TkWinGetDrawableDC,V_TkWinGetDrawableDC,_ANSI_ARGS_((Display * display,
				Drawable d, TkWinDCState* state)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinGetDrawableDC */

#ifndef TkWinGetMenuSystemDefault
#ifdef __WIN32__
VFUNC(Tcl_Obj *,TkWinGetMenuSystemDefault,V_TkWinGetMenuSystemDefault,_ANSI_ARGS_((
				Tk_Window tkwin, CONST char * dbName,
				CONST char * className)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinGetMenuSystemDefault */

#ifndef TkWinGetModifierState
#ifdef __WIN32__
VFUNC(int,TkWinGetModifierState,V_TkWinGetModifierState,_ANSI_ARGS_((void)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinGetModifierState */

#ifndef TkWinGetPlatformId
#ifdef __WIN32__
VFUNC(int,TkWinGetPlatformId,V_TkWinGetPlatformId,_ANSI_ARGS_((void)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinGetPlatformId */

#ifndef TkWinGetSystemPalette
#ifdef __WIN32__
VFUNC(HPALETTE,TkWinGetSystemPalette,V_TkWinGetSystemPalette,_ANSI_ARGS_((void)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinGetSystemPalette */

#ifndef TkWinGetWrapperWindow
#ifdef __WIN32__
VFUNC(HWND,TkWinGetWrapperWindow,V_TkWinGetWrapperWindow,_ANSI_ARGS_((Tk_Window tkwin)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinGetWrapperWindow */

#ifndef TkWinHandleMenuEvent
#ifdef __WIN32__
VFUNC(int,TkWinHandleMenuEvent,V_TkWinHandleMenuEvent,_ANSI_ARGS_((HWND * phwnd,
				UINT * pMessage, WPARAM * pwParam,
				LPARAM * plParam, LRESULT * plResult)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinHandleMenuEvent */

#ifndef TkWinIndexOfColor
#ifdef __WIN32__
VFUNC(int,TkWinIndexOfColor,V_TkWinIndexOfColor,_ANSI_ARGS_((XColor * colorPtr)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinIndexOfColor */

#ifndef TkWinReleaseDrawableDC
#ifdef __WIN32__
VFUNC(void,TkWinReleaseDrawableDC,V_TkWinReleaseDrawableDC,_ANSI_ARGS_((Drawable d,
				HDC hdc, TkWinDCState* state)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinReleaseDrawableDC */

#ifndef TkWinResendEvent
#ifdef __WIN32__
VFUNC(LRESULT,TkWinResendEvent,V_TkWinResendEvent,_ANSI_ARGS_((WNDPROC wndproc,
				HWND hwnd, XEvent * eventPtr)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinResendEvent */

#ifndef TkWinSelectPalette
#ifdef __WIN32__
VFUNC(HPALETTE,TkWinSelectPalette,V_TkWinSelectPalette,_ANSI_ARGS_((HDC dc,
				Colormap colormap)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinSelectPalette */

#ifndef TkWinSetForegroundWindow
#ifdef __WIN32__
VFUNC(void,TkWinSetForegroundWindow,V_TkWinSetForegroundWindow,_ANSI_ARGS_((
				TkWindow * winPtr)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinSetForegroundWindow */

#ifndef TkWinSetHINSTANCE
#ifdef __WIN32__
VFUNC(void,TkWinSetHINSTANCE,V_TkWinSetHINSTANCE,_ANSI_ARGS_((HINSTANCE hInstance)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinSetHINSTANCE */

#ifndef TkWinSetMenu
#ifdef __WIN32__
VFUNC(void,TkWinSetMenu,V_TkWinSetMenu,_ANSI_ARGS_((Tk_Window tkwin,
				HMENU hMenu)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinSetMenu */

#ifndef TkWinSetWindowPos
#ifdef __WIN32__
VFUNC(void,TkWinSetWindowPos,V_TkWinSetWindowPos,_ANSI_ARGS_((HWND hwnd,
				HWND siblingHwnd, int pos)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinSetWindowPos */

#ifndef TkWinWmCleanup
#ifdef __WIN32__
VFUNC(void,TkWinWmCleanup,V_TkWinWmCleanup,_ANSI_ARGS_((HINSTANCE hInstance)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinWmCleanup */

#ifndef TkWinXCleanup
#ifdef __WIN32__
VFUNC(void,TkWinXCleanup,V_TkWinXCleanup,_ANSI_ARGS_((HINSTANCE hInstance)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinXCleanup */

#ifndef TkWinXInit
#ifdef __WIN32__
VFUNC(void,TkWinXInit,V_TkWinXInit,_ANSI_ARGS_((HINSTANCE hInstance)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkWinXInit */

#ifndef TkWmCleanup
#if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK))
VFUNC(void,TkWmCleanup,V_TkWmCleanup,_ANSI_ARGS_((TkDisplay * dispPtr)))
#endif /* #if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK)) */
#endif /* #ifndef TkWmCleanup */

#ifndef Tk_TopCoordsToWindow
#ifdef MAC_OSX_TK
VFUNC(Tk_Window,Tk_TopCoordsToWindow,V_Tk_TopCoordsToWindow,_ANSI_ARGS_((Tk_Window tkwin,
				int rootX, int rootY, int * newX, int * newY)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(Tk_Window,Tk_TopCoordsToWindow,V_Tk_TopCoordsToWindow,_ANSI_ARGS_((Tk_Window tkwin,
				int rootX, int rootY, int * newX, int * newY)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef Tk_TopCoordsToWindow */

#ifndef TkpCmapStressed
#if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK))
VFUNC(int,TkpCmapStressed,V_TkpCmapStressed,_ANSI_ARGS_((Tk_Window tkwin,
				Colormap colormap)))
#endif /* #if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK)) */
#endif /* #ifndef TkpCmapStressed */

#ifndef TkpGetMS
#ifdef MAC_OSX_TK
VFUNC(unsigned long,TkpGetMS,V_TkpGetMS,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(unsigned long,TkpGetMS,V_TkpGetMS,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(unsigned long,TkpGetMS,V_TkpGetMS,_ANSI_ARGS_((void)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkpGetMS */

#ifndef TkpIsWindowFloating
#ifdef MAC_OSX_TK
VFUNC(int,TkpIsWindowFloating,V_TkpIsWindowFloating,_ANSI_ARGS_((WindowRef window)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(int,TkpIsWindowFloating,V_TkpIsWindowFloating,_ANSI_ARGS_((WindowRef window)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef TkpIsWindowFloating */

#ifndef TkpPrintWindowId
#ifdef __WIN32__
VFUNC(void,TkpPrintWindowId,V_TkpPrintWindowId,_ANSI_ARGS_((char * buf,
				Window window)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkpPrintWindowId */

#ifndef TkpScanWindowId
#if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK))
VFUNC(int,TkpScanWindowId,V_TkpScanWindowId,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_Obj *string, Window * idPtr)))
#endif /* #if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK)) */
#ifdef __WIN32__
VFUNC(int,TkpScanWindowId,V_TkpScanWindowId,_ANSI_ARGS_((Tcl_Interp * interp,
				Tcl_Obj * string, Window * idPtr)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkpScanWindowId */

#ifndef TkpSetCapture
#ifdef MAC_OSX_TK
VFUNC(void,TkpSetCapture,V_TkpSetCapture,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,TkpSetCapture,V_TkpSetCapture,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,TkpSetCapture,V_TkpSetCapture,_ANSI_ARGS_((TkWindow * winPtr)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkpSetCapture */

#ifndef TkpSetCursor
#ifdef MAC_OSX_TK
VFUNC(void,TkpSetCursor,V_TkpSetCursor,_ANSI_ARGS_((TkpCursor cursor)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,TkpSetCursor,V_TkpSetCursor,_ANSI_ARGS_((TkpCursor cursor)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,TkpSetCursor,V_TkpSetCursor,_ANSI_ARGS_((TkpCursor cursor)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkpSetCursor */

#ifndef TkpSync
#if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK))
VFUNC(void,TkpSync,V_TkpSync,_ANSI_ARGS_((Display * display)))
#endif /* #if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK)) */
#endif /* #ifndef TkpSync */

#ifndef TkpWmSetState
#if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK))
VFUNC(int,TkpWmSetState,V_TkpWmSetState,_ANSI_ARGS_((TkWindow * winPtr,
				int state)))
#endif /* #if !(defined(__WIN32__) || defined(MAC_TCL) || defined(MAC_OSX_TK)) */
#ifdef MAC_OSX_TK
VFUNC(void,TkpWmSetState,V_TkpWmSetState,_ANSI_ARGS_((TkWindow * winPtr,
				int state)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,TkpWmSetState,V_TkpWmSetState,_ANSI_ARGS_((TkWindow * winPtr,
				int state)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,TkpWmSetState,V_TkpWmSetState,_ANSI_ARGS_((TkWindow * winPtr,
				int state)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkpWmSetState */

#endif /* _TKINTPLATDECLS */
