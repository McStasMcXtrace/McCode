#ifndef _TKINTPLATDECLS_VM
#define _TKINTPLATDECLS_VM
#include "tkIntPlatDecls_f.h"
#ifndef NO_VTABLES
#ifndef TkAboutDlg
#  define TkAboutDlg (*TkintplatdeclsVptr->V_TkAboutDlg)
#endif

#ifndef TkAlignImageData
#  define TkAlignImageData (*TkintplatdeclsVptr->V_TkAlignImageData)
#endif

#ifndef TkCreateXEventSource
#  define TkCreateXEventSource (*TkintplatdeclsVptr->V_TkCreateXEventSource)
#endif

#ifndef TkFreeWindowId
#  define TkFreeWindowId (*TkintplatdeclsVptr->V_TkFreeWindowId)
#endif

#ifndef TkFreeXId
#  define TkFreeXId (*TkintplatdeclsVptr->V_TkFreeXId)
#endif

#ifndef TkGenWMDestroyEvent
#  define TkGenWMDestroyEvent (*TkintplatdeclsVptr->V_TkGenWMDestroyEvent)
#endif

#ifndef TkGenerateActivateEvents
#  define TkGenerateActivateEvents (*TkintplatdeclsVptr->V_TkGenerateActivateEvents)
#endif

#ifndef TkGenerateButtonEvent
#  define TkGenerateButtonEvent (*TkintplatdeclsVptr->V_TkGenerateButtonEvent)
#endif

#ifndef TkGetTransientMaster
#  define TkGetTransientMaster (*TkintplatdeclsVptr->V_TkGetTransientMaster)
#endif

#ifndef TkInitXId
#  define TkInitXId (*TkintplatdeclsVptr->V_TkInitXId)
#endif

#ifndef TkMacButtonKeyState
#  define TkMacButtonKeyState (*TkintplatdeclsVptr->V_TkMacButtonKeyState)
#endif

#ifndef TkMacClearMenubarActive
#  define TkMacClearMenubarActive (*TkintplatdeclsVptr->V_TkMacClearMenubarActive)
#endif

#ifndef TkMacContainerId
#  define TkMacContainerId (*TkintplatdeclsVptr->V_TkMacContainerId)
#endif

#ifndef TkMacDispatchMenuEvent
#  define TkMacDispatchMenuEvent (*TkintplatdeclsVptr->V_TkMacDispatchMenuEvent)
#endif

#ifndef TkMacDoHLEvent
#  define TkMacDoHLEvent (*TkintplatdeclsVptr->V_TkMacDoHLEvent)
#endif

#ifndef TkMacGenerateTime
#  define TkMacGenerateTime (*TkintplatdeclsVptr->V_TkMacGenerateTime)
#endif

#ifndef TkMacGetHostToplevel
#  define TkMacGetHostToplevel (*TkintplatdeclsVptr->V_TkMacGetHostToplevel)
#endif

#ifndef TkMacGetScrollbarGrowWindow
#  define TkMacGetScrollbarGrowWindow (*TkintplatdeclsVptr->V_TkMacGetScrollbarGrowWindow)
#endif

#ifndef TkMacGetXWindow
#  define TkMacGetXWindow (*TkintplatdeclsVptr->V_TkMacGetXWindow)
#endif

#ifndef TkMacGrowToplevel
#  define TkMacGrowToplevel (*TkintplatdeclsVptr->V_TkMacGrowToplevel)
#endif

#ifndef TkMacHandleMenuSelect
#  define TkMacHandleMenuSelect (*TkintplatdeclsVptr->V_TkMacHandleMenuSelect)
#endif

#ifndef TkMacHandleTearoffMenu
#  define TkMacHandleTearoffMenu (*TkintplatdeclsVptr->V_TkMacHandleTearoffMenu)
#endif

#ifndef TkMacInstallCursor
#  define TkMacInstallCursor (*TkintplatdeclsVptr->V_TkMacInstallCursor)
#endif

#ifndef TkMacInvalidateWindow
#  define TkMacInvalidateWindow (*TkintplatdeclsVptr->V_TkMacInvalidateWindow)
#endif

#ifndef TkMacIsCharacterMissing
#  define TkMacIsCharacterMissing (*TkintplatdeclsVptr->V_TkMacIsCharacterMissing)
#endif

#ifndef TkMacMakeRealWindowExist
#  define TkMacMakeRealWindowExist (*TkintplatdeclsVptr->V_TkMacMakeRealWindowExist)
#endif

#ifndef TkMacMakeStippleMap
#  define TkMacMakeStippleMap (*TkintplatdeclsVptr->V_TkMacMakeStippleMap)
#endif

#ifndef TkMacMenuClick
#  define TkMacMenuClick (*TkintplatdeclsVptr->V_TkMacMenuClick)
#endif

#ifndef TkMacOSXButtonKeyState
#  define TkMacOSXButtonKeyState (*TkintplatdeclsVptr->V_TkMacOSXButtonKeyState)
#endif

#ifndef TkMacOSXClearMenubarActive
#  define TkMacOSXClearMenubarActive (*TkintplatdeclsVptr->V_TkMacOSXClearMenubarActive)
#endif

#ifndef TkMacOSXContainerId
#  define TkMacOSXContainerId (*TkintplatdeclsVptr->V_TkMacOSXContainerId)
#endif

#ifndef TkMacOSXDispatchMenuEvent
#  define TkMacOSXDispatchMenuEvent (*TkintplatdeclsVptr->V_TkMacOSXDispatchMenuEvent)
#endif

#ifndef TkMacOSXDoHLEvent
#  define TkMacOSXDoHLEvent (*TkintplatdeclsVptr->V_TkMacOSXDoHLEvent)
#endif

#ifndef TkMacOSXGetCapture
#  define TkMacOSXGetCapture (*TkintplatdeclsVptr->V_TkMacOSXGetCapture)
#endif

#ifndef TkMacOSXGetHostToplevel
#  define TkMacOSXGetHostToplevel (*TkintplatdeclsVptr->V_TkMacOSXGetHostToplevel)
#endif

#ifndef TkMacOSXGetXWindow
#  define TkMacOSXGetXWindow (*TkintplatdeclsVptr->V_TkMacOSXGetXWindow)
#endif

#ifndef TkMacOSXGrowToplevel
#  define TkMacOSXGrowToplevel (*TkintplatdeclsVptr->V_TkMacOSXGrowToplevel)
#endif

#ifndef TkMacOSXHandleMenuSelect
#  define TkMacOSXHandleMenuSelect (*TkintplatdeclsVptr->V_TkMacOSXHandleMenuSelect)
#endif

#ifndef TkMacOSXHandleTearoffMenu
#  define TkMacOSXHandleTearoffMenu (*TkintplatdeclsVptr->V_TkMacOSXHandleTearoffMenu)
#endif

#ifndef TkMacOSXInstallCursor
#  define TkMacOSXInstallCursor (*TkintplatdeclsVptr->V_TkMacOSXInstallCursor)
#endif

#ifndef TkMacOSXInvalidateWindow
#  define TkMacOSXInvalidateWindow (*TkintplatdeclsVptr->V_TkMacOSXInvalidateWindow)
#endif

#ifndef TkMacOSXIsCharacterMissing
#  define TkMacOSXIsCharacterMissing (*TkintplatdeclsVptr->V_TkMacOSXIsCharacterMissing)
#endif

#ifndef TkMacOSXMakeRealWindowExist
#  define TkMacOSXMakeRealWindowExist (*TkintplatdeclsVptr->V_TkMacOSXMakeRealWindowExist)
#endif

#ifndef TkMacOSXMakeStippleMap
#  define TkMacOSXMakeStippleMap (*TkintplatdeclsVptr->V_TkMacOSXMakeStippleMap)
#endif

#ifndef TkMacOSXMenuClick
#  define TkMacOSXMenuClick (*TkintplatdeclsVptr->V_TkMacOSXMenuClick)
#endif

#ifndef TkMacOSXPreprocessMenu
#  define TkMacOSXPreprocessMenu (*TkintplatdeclsVptr->V_TkMacOSXPreprocessMenu)
#endif

#ifndef TkMacOSXRegisterOffScreenWindow
#  define TkMacOSXRegisterOffScreenWindow (*TkintplatdeclsVptr->V_TkMacOSXRegisterOffScreenWindow)
#endif

#ifndef TkMacOSXResizable
#  define TkMacOSXResizable (*TkintplatdeclsVptr->V_TkMacOSXResizable)
#endif

#ifndef TkMacOSXSetHelpMenuItemCount
#  define TkMacOSXSetHelpMenuItemCount (*TkintplatdeclsVptr->V_TkMacOSXSetHelpMenuItemCount)
#endif

#ifndef TkMacOSXSetScrollbarGrow
#  define TkMacOSXSetScrollbarGrow (*TkintplatdeclsVptr->V_TkMacOSXSetScrollbarGrow)
#endif

#ifndef TkMacOSXSetUpClippingRgn
#  define TkMacOSXSetUpClippingRgn (*TkintplatdeclsVptr->V_TkMacOSXSetUpClippingRgn)
#endif

#ifndef TkMacOSXSetUpGraphicsPort
#  define TkMacOSXSetUpGraphicsPort (*TkintplatdeclsVptr->V_TkMacOSXSetUpGraphicsPort)
#endif

#ifndef TkMacOSXUnregisterMacWindow
#  define TkMacOSXUnregisterMacWindow (*TkintplatdeclsVptr->V_TkMacOSXUnregisterMacWindow)
#endif

#ifndef TkMacOSXUpdateClipRgn
#  define TkMacOSXUpdateClipRgn (*TkintplatdeclsVptr->V_TkMacOSXUpdateClipRgn)
#endif

#ifndef TkMacOSXUseMenuID
#  define TkMacOSXUseMenuID (*TkintplatdeclsVptr->V_TkMacOSXUseMenuID)
#endif

#ifndef TkMacOSXVisableClipRgn
#  define TkMacOSXVisableClipRgn (*TkintplatdeclsVptr->V_TkMacOSXVisableClipRgn)
#endif

#ifndef TkMacOSXWinBounds
#  define TkMacOSXWinBounds (*TkintplatdeclsVptr->V_TkMacOSXWinBounds)
#endif

#ifndef TkMacOSXWindowOffset
#  define TkMacOSXWindowOffset (*TkintplatdeclsVptr->V_TkMacOSXWindowOffset)
#endif

#ifndef TkMacOSXZoomToplevel
#  define TkMacOSXZoomToplevel (*TkintplatdeclsVptr->V_TkMacOSXZoomToplevel)
#endif

#ifndef TkMacPreprocessMenu
#  define TkMacPreprocessMenu (*TkintplatdeclsVptr->V_TkMacPreprocessMenu)
#endif

#ifndef TkMacRegisterOffScreenWindow
#  define TkMacRegisterOffScreenWindow (*TkintplatdeclsVptr->V_TkMacRegisterOffScreenWindow)
#endif

#ifndef TkMacResizable
#  define TkMacResizable (*TkintplatdeclsVptr->V_TkMacResizable)
#endif

#ifndef TkMacSetHelpMenuItemCount
#  define TkMacSetHelpMenuItemCount (*TkintplatdeclsVptr->V_TkMacSetHelpMenuItemCount)
#endif

#ifndef TkMacSetScrollbarGrow
#  define TkMacSetScrollbarGrow (*TkintplatdeclsVptr->V_TkMacSetScrollbarGrow)
#endif

#ifndef TkMacSetUpClippingRgn
#  define TkMacSetUpClippingRgn (*TkintplatdeclsVptr->V_TkMacSetUpClippingRgn)
#endif

#ifndef TkMacSetUpGraphicsPort
#  define TkMacSetUpGraphicsPort (*TkintplatdeclsVptr->V_TkMacSetUpGraphicsPort)
#endif

#ifndef TkMacUnregisterMacWindow
#  define TkMacUnregisterMacWindow (*TkintplatdeclsVptr->V_TkMacUnregisterMacWindow)
#endif

#ifndef TkMacUpdateClipRgn
#  define TkMacUpdateClipRgn (*TkintplatdeclsVptr->V_TkMacUpdateClipRgn)
#endif

#ifndef TkMacUseMenuID
#  define TkMacUseMenuID (*TkintplatdeclsVptr->V_TkMacUseMenuID)
#endif

#ifndef TkMacVisableClipRgn
#  define TkMacVisableClipRgn (*TkintplatdeclsVptr->V_TkMacVisableClipRgn)
#endif

#ifndef TkMacWinBounds
#  define TkMacWinBounds (*TkintplatdeclsVptr->V_TkMacWinBounds)
#endif

#ifndef TkMacWindowOffset
#  define TkMacWindowOffset (*TkintplatdeclsVptr->V_TkMacWindowOffset)
#endif

#ifndef TkMacZoomToplevel
#  define TkMacZoomToplevel (*TkintplatdeclsVptr->V_TkMacZoomToplevel)
#endif

#ifndef TkPointerDeadWindow
#  define TkPointerDeadWindow (*TkintplatdeclsVptr->V_TkPointerDeadWindow)
#endif

#ifndef TkSendCleanup
#  define TkSendCleanup (*TkintplatdeclsVptr->V_TkSendCleanup)
#endif

#ifndef TkSetMacColor
#  define TkSetMacColor (*TkintplatdeclsVptr->V_TkSetMacColor)
#endif

#ifndef TkSetPixmapColormap
#  define TkSetPixmapColormap (*TkintplatdeclsVptr->V_TkSetPixmapColormap)
#endif

#ifndef TkSetWMName
#  define TkSetWMName (*TkintplatdeclsVptr->V_TkSetWMName)
#endif

#ifndef TkSuspendClipboard
#  define TkSuspendClipboard (*TkintplatdeclsVptr->V_TkSuspendClipboard)
#endif

#ifndef TkUnixContainerId
#  define TkUnixContainerId (*TkintplatdeclsVptr->V_TkUnixContainerId)
#endif

#ifndef TkUnixDoOneXEvent
#  define TkUnixDoOneXEvent (*TkintplatdeclsVptr->V_TkUnixDoOneXEvent)
#endif

#ifndef TkUnixSetMenubar
#  define TkUnixSetMenubar (*TkintplatdeclsVptr->V_TkUnixSetMenubar)
#endif

#ifndef TkWinCancelMouseTimer
#  define TkWinCancelMouseTimer (*TkintplatdeclsVptr->V_TkWinCancelMouseTimer)
#endif

#ifndef TkWinClipboardRender
#  define TkWinClipboardRender (*TkintplatdeclsVptr->V_TkWinClipboardRender)
#endif

#ifndef TkWinDialogDebug
#  define TkWinDialogDebug (*TkintplatdeclsVptr->V_TkWinDialogDebug)
#endif

#ifndef TkWinEmbeddedEventProc
#  define TkWinEmbeddedEventProc (*TkintplatdeclsVptr->V_TkWinEmbeddedEventProc)
#endif

#ifndef TkWinFillRect
#  define TkWinFillRect (*TkintplatdeclsVptr->V_TkWinFillRect)
#endif

#ifndef TkWinGetBorderPixels
#  define TkWinGetBorderPixels (*TkintplatdeclsVptr->V_TkWinGetBorderPixels)
#endif

#ifndef TkWinGetDrawableDC
#  define TkWinGetDrawableDC (*TkintplatdeclsVptr->V_TkWinGetDrawableDC)
#endif

#ifndef TkWinGetMenuSystemDefault
#  define TkWinGetMenuSystemDefault (*TkintplatdeclsVptr->V_TkWinGetMenuSystemDefault)
#endif

#ifndef TkWinGetModifierState
#  define TkWinGetModifierState (*TkintplatdeclsVptr->V_TkWinGetModifierState)
#endif

#ifndef TkWinGetPlatformId
#  define TkWinGetPlatformId (*TkintplatdeclsVptr->V_TkWinGetPlatformId)
#endif

#ifndef TkWinGetSystemPalette
#  define TkWinGetSystemPalette (*TkintplatdeclsVptr->V_TkWinGetSystemPalette)
#endif

#ifndef TkWinGetWrapperWindow
#  define TkWinGetWrapperWindow (*TkintplatdeclsVptr->V_TkWinGetWrapperWindow)
#endif

#ifndef TkWinHandleMenuEvent
#  define TkWinHandleMenuEvent (*TkintplatdeclsVptr->V_TkWinHandleMenuEvent)
#endif

#ifndef TkWinIndexOfColor
#  define TkWinIndexOfColor (*TkintplatdeclsVptr->V_TkWinIndexOfColor)
#endif

#ifndef TkWinReleaseDrawableDC
#  define TkWinReleaseDrawableDC (*TkintplatdeclsVptr->V_TkWinReleaseDrawableDC)
#endif

#ifndef TkWinResendEvent
#  define TkWinResendEvent (*TkintplatdeclsVptr->V_TkWinResendEvent)
#endif

#ifndef TkWinSelectPalette
#  define TkWinSelectPalette (*TkintplatdeclsVptr->V_TkWinSelectPalette)
#endif

#ifndef TkWinSetForegroundWindow
#  define TkWinSetForegroundWindow (*TkintplatdeclsVptr->V_TkWinSetForegroundWindow)
#endif

#ifndef TkWinSetHINSTANCE
#  define TkWinSetHINSTANCE (*TkintplatdeclsVptr->V_TkWinSetHINSTANCE)
#endif

#ifndef TkWinSetMenu
#  define TkWinSetMenu (*TkintplatdeclsVptr->V_TkWinSetMenu)
#endif

#ifndef TkWinSetWindowPos
#  define TkWinSetWindowPos (*TkintplatdeclsVptr->V_TkWinSetWindowPos)
#endif

#ifndef TkWinWmCleanup
#  define TkWinWmCleanup (*TkintplatdeclsVptr->V_TkWinWmCleanup)
#endif

#ifndef TkWinXCleanup
#  define TkWinXCleanup (*TkintplatdeclsVptr->V_TkWinXCleanup)
#endif

#ifndef TkWinXInit
#  define TkWinXInit (*TkintplatdeclsVptr->V_TkWinXInit)
#endif

#ifndef TkWmCleanup
#  define TkWmCleanup (*TkintplatdeclsVptr->V_TkWmCleanup)
#endif

#ifndef Tk_TopCoordsToWindow
#  define Tk_TopCoordsToWindow (*TkintplatdeclsVptr->V_Tk_TopCoordsToWindow)
#endif

#ifndef TkpCmapStressed
#  define TkpCmapStressed (*TkintplatdeclsVptr->V_TkpCmapStressed)
#endif

#ifndef TkpGetMS
#  define TkpGetMS (*TkintplatdeclsVptr->V_TkpGetMS)
#endif

#ifndef TkpIsWindowFloating
#  define TkpIsWindowFloating (*TkintplatdeclsVptr->V_TkpIsWindowFloating)
#endif

#ifndef TkpPrintWindowId
#  define TkpPrintWindowId (*TkintplatdeclsVptr->V_TkpPrintWindowId)
#endif

#ifndef TkpScanWindowId
#  define TkpScanWindowId (*TkintplatdeclsVptr->V_TkpScanWindowId)
#endif

#ifndef TkpSetCapture
#  define TkpSetCapture (*TkintplatdeclsVptr->V_TkpSetCapture)
#endif

#ifndef TkpSetCursor
#  define TkpSetCursor (*TkintplatdeclsVptr->V_TkpSetCursor)
#endif

#ifndef TkpSync
#  define TkpSync (*TkintplatdeclsVptr->V_TkpSync)
#endif

#ifndef TkpWmSetState
#  define TkpWmSetState (*TkintplatdeclsVptr->V_TkpWmSetState)
#endif

#endif /* NO_VTABLES */
#endif /* _TKINTPLATDECLS_VM */
