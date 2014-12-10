#ifndef _TKINTXLIBDECLS_VM
#define _TKINTXLIBDECLS_VM
#include "tkIntXlibDecls_f.h"
#ifndef NO_VTABLES
#ifndef TkPutImage
#  define TkPutImage (*TkintxlibdeclsVptr->V_TkPutImage)
#endif

#ifndef XAllocColor
#  define XAllocColor (*TkintxlibdeclsVptr->V_XAllocColor)
#endif

#ifndef XBell
#  define XBell (*TkintxlibdeclsVptr->V_XBell)
#endif

#ifndef XChangeGC
#  define XChangeGC (*TkintxlibdeclsVptr->V_XChangeGC)
#endif

#ifndef XChangeProperty
#  define XChangeProperty (*TkintxlibdeclsVptr->V_XChangeProperty)
#endif

#ifndef XChangeWindowAttributes
#  define XChangeWindowAttributes (*TkintxlibdeclsVptr->V_XChangeWindowAttributes)
#endif

#ifndef XClearWindow
#  define XClearWindow (*TkintxlibdeclsVptr->V_XClearWindow)
#endif

#ifndef XConfigureWindow
#  define XConfigureWindow (*TkintxlibdeclsVptr->V_XConfigureWindow)
#endif

#ifndef XCopyArea
#  define XCopyArea (*TkintxlibdeclsVptr->V_XCopyArea)
#endif

#ifndef XCopyPlane
#  define XCopyPlane (*TkintxlibdeclsVptr->V_XCopyPlane)
#endif

#ifndef XCreateBitmapFromData
#  define XCreateBitmapFromData (*TkintxlibdeclsVptr->V_XCreateBitmapFromData)
#endif

#ifndef XCreateColormap
#  define XCreateColormap (*TkintxlibdeclsVptr->V_XCreateColormap)
#endif

#ifndef XCreateGC
#  define XCreateGC (*TkintxlibdeclsVptr->V_XCreateGC)
#endif

#ifndef XCreateGlyphCursor
#  define XCreateGlyphCursor (*TkintxlibdeclsVptr->V_XCreateGlyphCursor)
#endif

#ifndef XCreateIC
#  define XCreateIC (*TkintxlibdeclsVptr->V_XCreateIC)
#endif

#ifndef XCreateImage
#  define XCreateImage (*TkintxlibdeclsVptr->V_XCreateImage)
#endif

#ifndef XCreatePixmapCursor
#  define XCreatePixmapCursor (*TkintxlibdeclsVptr->V_XCreatePixmapCursor)
#endif

#ifndef XDefineCursor
#  define XDefineCursor (*TkintxlibdeclsVptr->V_XDefineCursor)
#endif

#ifndef XDeleteProperty
#  define XDeleteProperty (*TkintxlibdeclsVptr->V_XDeleteProperty)
#endif

#ifndef XDestroyIC
#  define XDestroyIC (*TkintxlibdeclsVptr->V_XDestroyIC)
#endif

#ifndef XDestroyWindow
#  define XDestroyWindow (*TkintxlibdeclsVptr->V_XDestroyWindow)
#endif

#ifndef XDrawArc
#  define XDrawArc (*TkintxlibdeclsVptr->V_XDrawArc)
#endif

#ifndef XDrawLine
#  define XDrawLine (*TkintxlibdeclsVptr->V_XDrawLine)
#endif

#ifndef XDrawLines
#  define XDrawLines (*TkintxlibdeclsVptr->V_XDrawLines)
#endif

#ifndef XDrawPoint
#  define XDrawPoint (*TkintxlibdeclsVptr->V_XDrawPoint)
#endif

#ifndef XDrawPoints
#  define XDrawPoints (*TkintxlibdeclsVptr->V_XDrawPoints)
#endif

#ifndef XDrawRectangle
#  define XDrawRectangle (*TkintxlibdeclsVptr->V_XDrawRectangle)
#endif

#ifndef XDrawSegments
#  define XDrawSegments (*TkintxlibdeclsVptr->V_XDrawSegments)
#endif

#ifndef XFillArc
#  define XFillArc (*TkintxlibdeclsVptr->V_XFillArc)
#endif

#ifndef XFillPolygon
#  define XFillPolygon (*TkintxlibdeclsVptr->V_XFillPolygon)
#endif

#ifndef XFillRectangle
#  define XFillRectangle (*TkintxlibdeclsVptr->V_XFillRectangle)
#endif

#ifndef XFillRectangles
#  define XFillRectangles (*TkintxlibdeclsVptr->V_XFillRectangles)
#endif

#ifndef XFilterEvent
#  define XFilterEvent (*TkintxlibdeclsVptr->V_XFilterEvent)
#endif

#ifndef XForceScreenSaver
#  define XForceScreenSaver (*TkintxlibdeclsVptr->V_XForceScreenSaver)
#endif

#ifndef XFreeColormap
#  define XFreeColormap (*TkintxlibdeclsVptr->V_XFreeColormap)
#endif

#ifndef XFreeColors
#  define XFreeColors (*TkintxlibdeclsVptr->V_XFreeColors)
#endif

#ifndef XFreeCursor
#  define XFreeCursor (*TkintxlibdeclsVptr->V_XFreeCursor)
#endif

#ifndef XFreeGC
#  define XFreeGC (*TkintxlibdeclsVptr->V_XFreeGC)
#endif

#ifndef XFreeModifiermap
#  define XFreeModifiermap (*TkintxlibdeclsVptr->V_XFreeModifiermap)
#endif

#ifndef XGContextFromGC
#  define XGContextFromGC (*TkintxlibdeclsVptr->V_XGContextFromGC)
#endif

#ifndef XGetAtomName
#  define XGetAtomName (*TkintxlibdeclsVptr->V_XGetAtomName)
#endif

#ifndef XGetGeometry
#  define XGetGeometry (*TkintxlibdeclsVptr->V_XGetGeometry)
#endif

#ifndef XGetImage
#  define XGetImage (*TkintxlibdeclsVptr->V_XGetImage)
#endif

#ifndef XGetInputFocus
#  define XGetInputFocus (*TkintxlibdeclsVptr->V_XGetInputFocus)
#endif

#ifndef XGetModifierMapping
#  define XGetModifierMapping (*TkintxlibdeclsVptr->V_XGetModifierMapping)
#endif

#ifndef XGetVisualInfo
#  define XGetVisualInfo (*TkintxlibdeclsVptr->V_XGetVisualInfo)
#endif

#ifndef XGetWMColormapWindows
#  define XGetWMColormapWindows (*TkintxlibdeclsVptr->V_XGetWMColormapWindows)
#endif

#ifndef XGetWindowAttributes
#  define XGetWindowAttributes (*TkintxlibdeclsVptr->V_XGetWindowAttributes)
#endif

#ifndef XGetWindowProperty
#  define XGetWindowProperty (*TkintxlibdeclsVptr->V_XGetWindowProperty)
#endif

#ifndef XGrabKeyboard
#  define XGrabKeyboard (*TkintxlibdeclsVptr->V_XGrabKeyboard)
#endif

#ifndef XGrabPointer
#  define XGrabPointer (*TkintxlibdeclsVptr->V_XGrabPointer)
#endif

#ifndef XIconifyWindow
#  define XIconifyWindow (*TkintxlibdeclsVptr->V_XIconifyWindow)
#endif

#ifndef XInternAtom
#  define XInternAtom (*TkintxlibdeclsVptr->V_XInternAtom)
#endif

#ifndef XKeycodeToKeysym
#  define XKeycodeToKeysym (*TkintxlibdeclsVptr->V_XKeycodeToKeysym)
#endif

#ifndef XKeysymToKeycode
#  define XKeysymToKeycode (*TkintxlibdeclsVptr->V_XKeysymToKeycode)
#endif

#ifndef XKeysymToString
#  define XKeysymToString (*TkintxlibdeclsVptr->V_XKeysymToString)
#endif

#ifndef XListHosts
#  define XListHosts (*TkintxlibdeclsVptr->V_XListHosts)
#endif

#ifndef XLookupColor
#  define XLookupColor (*TkintxlibdeclsVptr->V_XLookupColor)
#endif

#ifndef XMapWindow
#  define XMapWindow (*TkintxlibdeclsVptr->V_XMapWindow)
#endif

#ifndef XMoveResizeWindow
#  define XMoveResizeWindow (*TkintxlibdeclsVptr->V_XMoveResizeWindow)
#endif

#ifndef XMoveWindow
#  define XMoveWindow (*TkintxlibdeclsVptr->V_XMoveWindow)
#endif

#ifndef XNextEvent
#  define XNextEvent (*TkintxlibdeclsVptr->V_XNextEvent)
#endif

#ifndef XParseColor
#  define XParseColor (*TkintxlibdeclsVptr->V_XParseColor)
#endif

#ifndef XPutBackEvent
#  define XPutBackEvent (*TkintxlibdeclsVptr->V_XPutBackEvent)
#endif

#ifndef XQueryColor
#  define XQueryColor (*TkintxlibdeclsVptr->V_XQueryColor)
#endif

#ifndef XQueryColors
#  define XQueryColors (*TkintxlibdeclsVptr->V_XQueryColors)
#endif

#ifndef XQueryPointer
#  define XQueryPointer (*TkintxlibdeclsVptr->V_XQueryPointer)
#endif

#ifndef XQueryTree
#  define XQueryTree (*TkintxlibdeclsVptr->V_XQueryTree)
#endif

#ifndef XRaiseWindow
#  define XRaiseWindow (*TkintxlibdeclsVptr->V_XRaiseWindow)
#endif

#ifndef XRefreshKeyboardMapping
#  define XRefreshKeyboardMapping (*TkintxlibdeclsVptr->V_XRefreshKeyboardMapping)
#endif

#ifndef XResizeWindow
#  define XResizeWindow (*TkintxlibdeclsVptr->V_XResizeWindow)
#endif

#ifndef XRootWindow
#  define XRootWindow (*TkintxlibdeclsVptr->V_XRootWindow)
#endif

#ifndef XSelectInput
#  define XSelectInput (*TkintxlibdeclsVptr->V_XSelectInput)
#endif

#ifndef XSendEvent
#  define XSendEvent (*TkintxlibdeclsVptr->V_XSendEvent)
#endif

#ifndef XSetArcMode
#  define XSetArcMode (*TkintxlibdeclsVptr->V_XSetArcMode)
#endif

#ifndef XSetBackground
#  define XSetBackground (*TkintxlibdeclsVptr->V_XSetBackground)
#endif

#ifndef XSetClipMask
#  define XSetClipMask (*TkintxlibdeclsVptr->V_XSetClipMask)
#endif

#ifndef XSetClipOrigin
#  define XSetClipOrigin (*TkintxlibdeclsVptr->V_XSetClipOrigin)
#endif

#ifndef XSetCommand
#  define XSetCommand (*TkintxlibdeclsVptr->V_XSetCommand)
#endif

#ifndef XSetDashes
#  define XSetDashes (*TkintxlibdeclsVptr->V_XSetDashes)
#endif

#ifndef XSetErrorHandler
#  define XSetErrorHandler (*TkintxlibdeclsVptr->V_XSetErrorHandler)
#endif

#ifndef XSetFillRule
#  define XSetFillRule (*TkintxlibdeclsVptr->V_XSetFillRule)
#endif

#ifndef XSetFillStyle
#  define XSetFillStyle (*TkintxlibdeclsVptr->V_XSetFillStyle)
#endif

#ifndef XSetFont
#  define XSetFont (*TkintxlibdeclsVptr->V_XSetFont)
#endif

#ifndef XSetForeground
#  define XSetForeground (*TkintxlibdeclsVptr->V_XSetForeground)
#endif

#ifndef XSetFunction
#  define XSetFunction (*TkintxlibdeclsVptr->V_XSetFunction)
#endif

#ifndef XSetIconName
#  define XSetIconName (*TkintxlibdeclsVptr->V_XSetIconName)
#endif

#ifndef XSetInputFocus
#  define XSetInputFocus (*TkintxlibdeclsVptr->V_XSetInputFocus)
#endif

#ifndef XSetLineAttributes
#  define XSetLineAttributes (*TkintxlibdeclsVptr->V_XSetLineAttributes)
#endif

#ifndef XSetSelectionOwner
#  define XSetSelectionOwner (*TkintxlibdeclsVptr->V_XSetSelectionOwner)
#endif

#ifndef XSetStipple
#  define XSetStipple (*TkintxlibdeclsVptr->V_XSetStipple)
#endif

#ifndef XSetTSOrigin
#  define XSetTSOrigin (*TkintxlibdeclsVptr->V_XSetTSOrigin)
#endif

#ifndef XSetWMClientMachine
#  define XSetWMClientMachine (*TkintxlibdeclsVptr->V_XSetWMClientMachine)
#endif

#ifndef XSetWindowBackground
#  define XSetWindowBackground (*TkintxlibdeclsVptr->V_XSetWindowBackground)
#endif

#ifndef XSetWindowBackgroundPixmap
#  define XSetWindowBackgroundPixmap (*TkintxlibdeclsVptr->V_XSetWindowBackgroundPixmap)
#endif

#ifndef XSetWindowBorder
#  define XSetWindowBorder (*TkintxlibdeclsVptr->V_XSetWindowBorder)
#endif

#ifndef XSetWindowBorderPixmap
#  define XSetWindowBorderPixmap (*TkintxlibdeclsVptr->V_XSetWindowBorderPixmap)
#endif

#ifndef XSetWindowBorderWidth
#  define XSetWindowBorderWidth (*TkintxlibdeclsVptr->V_XSetWindowBorderWidth)
#endif

#ifndef XSetWindowColormap
#  define XSetWindowColormap (*TkintxlibdeclsVptr->V_XSetWindowColormap)
#endif

#ifndef XStringListToTextProperty
#  define XStringListToTextProperty (*TkintxlibdeclsVptr->V_XStringListToTextProperty)
#endif

#ifndef XStringToKeysym
#  define XStringToKeysym (*TkintxlibdeclsVptr->V_XStringToKeysym)
#endif

#ifndef XTranslateCoordinates
#  define XTranslateCoordinates (*TkintxlibdeclsVptr->V_XTranslateCoordinates)
#endif

#ifndef XUngrabKeyboard
#  define XUngrabKeyboard (*TkintxlibdeclsVptr->V_XUngrabKeyboard)
#endif

#ifndef XUngrabPointer
#  define XUngrabPointer (*TkintxlibdeclsVptr->V_XUngrabPointer)
#endif

#ifndef XUnmapWindow
#  define XUnmapWindow (*TkintxlibdeclsVptr->V_XUnmapWindow)
#endif

#ifndef XWarpPointer
#  define XWarpPointer (*TkintxlibdeclsVptr->V_XWarpPointer)
#endif

#ifndef XWindowEvent
#  define XWindowEvent (*TkintxlibdeclsVptr->V_XWindowEvent)
#endif

#ifndef XWithdrawWindow
#  define XWithdrawWindow (*TkintxlibdeclsVptr->V_XWithdrawWindow)
#endif

#ifndef XmbLookupString
#  define XmbLookupString (*TkintxlibdeclsVptr->V_XmbLookupString)
#endif

#ifndef _XInitImageFuncPtrs
#  define _XInitImageFuncPtrs (*TkintxlibdeclsVptr->V__XInitImageFuncPtrs)
#endif

#endif /* NO_VTABLES */
#endif /* _TKINTXLIBDECLS_VM */
