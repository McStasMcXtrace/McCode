#ifdef _TKINTXLIBDECLS
#ifndef TkPutImage
#ifdef MAC_OSX_TK
VFUNC(void,TkPutImage,V_TkPutImage,_ANSI_ARGS_((unsigned long * colors,
				int ncolors, Display* display, Drawable d,
				GC gc, XImage* image, int src_x, int src_y,
				int dest_x, int dest_y, unsigned int width,
				unsigned int height)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,TkPutImage,V_TkPutImage,_ANSI_ARGS_((unsigned long * colors,
				int ncolors, Display* display, Drawable d,
				GC gc, XImage* image, int src_x, int src_y,
				int dest_x, int dest_y, unsigned int width,
				unsigned int height)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,TkPutImage,V_TkPutImage,_ANSI_ARGS_((unsigned long * colors,
				int ncolors, Display* display, Drawable d,
				GC gc, XImage* image, int src_x, int src_y,
				int dest_x, int dest_y, unsigned int width,
				unsigned int height)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef TkPutImage */

#ifndef XAllocColor
#ifdef MAC_OSX_TK
VFUNC(Status,XAllocColor,V_XAllocColor,_ANSI_ARGS_((Display* d, Colormap c,
				XColor* xp)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(Status,XAllocColor,V_XAllocColor,_ANSI_ARGS_((Display* d, Colormap c,
				XColor* xp)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(Status,XAllocColor,V_XAllocColor,_ANSI_ARGS_((Display* d, Colormap c,
				XColor* xp)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XAllocColor */

#ifndef XBell
#ifdef MAC_OSX_TK
VFUNC(void,XBell,V_XBell,_ANSI_ARGS_((Display* d, int i)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XBell,V_XBell,_ANSI_ARGS_((Display* d, int i)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XBell,V_XBell,_ANSI_ARGS_((Display* d, int i)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XBell */

#ifndef XChangeGC
#ifdef MAC_OSX_TK
VFUNC(void,XChangeGC,V_XChangeGC,_ANSI_ARGS_((Display * d, GC gc,
				unsigned long mask, XGCValues * values)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XChangeGC,V_XChangeGC,_ANSI_ARGS_((Display * d, GC gc,
				unsigned long mask, XGCValues * values)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XChangeGC,V_XChangeGC,_ANSI_ARGS_((Display * d, GC gc,
				unsigned long mask, XGCValues * values)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XChangeGC */

#ifndef XChangeProperty
#ifdef MAC_OSX_TK
VFUNC(void,XChangeProperty,V_XChangeProperty,_ANSI_ARGS_((Display* d, Window w,
				Atom a1, Atom a2, int i1, int i2,
				_Xconst unsigned char* c, int i3)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XChangeProperty,V_XChangeProperty,_ANSI_ARGS_((Display* d, Window w,
				Atom a1, Atom a2, int i1, int i2,
				_Xconst unsigned char* c, int i3)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XChangeProperty,V_XChangeProperty,_ANSI_ARGS_((Display* d, Window w,
				Atom a1, Atom a2, int i1, int i2,
				_Xconst unsigned char* c, int i3)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XChangeProperty */

#ifndef XChangeWindowAttributes
#ifdef MAC_OSX_TK
VFUNC(void,XChangeWindowAttributes,V_XChangeWindowAttributes,_ANSI_ARGS_((Display* d,
				Window w, unsigned long ul,
				XSetWindowAttributes* x)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XChangeWindowAttributes,V_XChangeWindowAttributes,_ANSI_ARGS_((Display* d,
				Window w, unsigned long ul,
				XSetWindowAttributes* x)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XChangeWindowAttributes,V_XChangeWindowAttributes,_ANSI_ARGS_((Display* d,
				Window w, unsigned long ul,
				XSetWindowAttributes* x)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XChangeWindowAttributes */

#ifndef XClearWindow
#ifdef MAC_OSX_TK
VFUNC(void,XClearWindow,V_XClearWindow,_ANSI_ARGS_((Display* d, Window w)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XClearWindow,V_XClearWindow,_ANSI_ARGS_((Display* d, Window w)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XClearWindow,V_XClearWindow,_ANSI_ARGS_((Display* d, Window w)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XClearWindow */

#ifndef XConfigureWindow
#ifdef MAC_OSX_TK
VFUNC(void,XConfigureWindow,V_XConfigureWindow,_ANSI_ARGS_((Display* d, Window w,
				unsigned int i, XWindowChanges* x)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XConfigureWindow,V_XConfigureWindow,_ANSI_ARGS_((Display* d, Window w,
				unsigned int i, XWindowChanges* x)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XConfigureWindow,V_XConfigureWindow,_ANSI_ARGS_((Display* d, Window w,
				unsigned int i, XWindowChanges* x)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XConfigureWindow */

#ifndef XCopyArea
#ifdef MAC_OSX_TK
VFUNC(void,XCopyArea,V_XCopyArea,_ANSI_ARGS_((Display* d, Drawable dr1,
				Drawable dr2, GC g, int i1, int i2,
				unsigned int ui1, unsigned int ui2, int i3,
				int i4)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XCopyArea,V_XCopyArea,_ANSI_ARGS_((Display* d, Drawable dr1,
				Drawable dr2, GC g, int i1, int i2,
				unsigned int ui1, unsigned int ui2, int i3,
				int i4)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XCopyArea,V_XCopyArea,_ANSI_ARGS_((Display* d, Drawable dr1,
				Drawable dr2, GC g, int i1, int i2,
				unsigned int ui1, unsigned int ui2, int i3,
				int i4)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XCopyArea */

#ifndef XCopyPlane
#ifdef MAC_OSX_TK
VFUNC(void,XCopyPlane,V_XCopyPlane,_ANSI_ARGS_((Display* d, Drawable dr1,
				Drawable dr2, GC g, int i1, int i2,
				unsigned int ui1, unsigned int ui2, int i3,
				int i4, unsigned long ul)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XCopyPlane,V_XCopyPlane,_ANSI_ARGS_((Display* d, Drawable dr1,
				Drawable dr2, GC g, int i1, int i2,
				unsigned int ui1, unsigned int ui2, int i3,
				int i4, unsigned long ul)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XCopyPlane,V_XCopyPlane,_ANSI_ARGS_((Display* d, Drawable dr1,
				Drawable dr2, GC g, int i1, int i2,
				unsigned int ui1, unsigned int ui2, int i3,
				int i4, unsigned long ul)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XCopyPlane */

#ifndef XCreateBitmapFromData
#ifdef MAC_OSX_TK
VFUNC(Pixmap,XCreateBitmapFromData,V_XCreateBitmapFromData,_ANSI_ARGS_((Display* display,
				Drawable d, _Xconst char* data,
				unsigned int width, unsigned int height)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(Pixmap,XCreateBitmapFromData,V_XCreateBitmapFromData,_ANSI_ARGS_((Display* display,
				Drawable d, _Xconst char* data,
				unsigned int width, unsigned int height)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(Pixmap,XCreateBitmapFromData,V_XCreateBitmapFromData,_ANSI_ARGS_((Display* display,
				Drawable d, _Xconst char* data,
				unsigned int width, unsigned int height)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XCreateBitmapFromData */

#ifndef XCreateColormap
#ifdef MAC_OSX_TK
VFUNC(Colormap,XCreateColormap,V_XCreateColormap,_ANSI_ARGS_((Display* d, Window w,
				Visual* v, int i)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(Colormap,XCreateColormap,V_XCreateColormap,_ANSI_ARGS_((Display* d, Window w,
				Visual* v, int i)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(Colormap,XCreateColormap,V_XCreateColormap,_ANSI_ARGS_((Display* d, Window w,
				Visual* v, int i)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XCreateColormap */

#ifndef XCreateGC
#ifdef MAC_OSX_TK
VFUNC(GC,XCreateGC,V_XCreateGC,_ANSI_ARGS_((Display* display, Drawable d,
				unsigned long valuemask, XGCValues* values)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(GC,XCreateGC,V_XCreateGC,_ANSI_ARGS_((Display* display, Drawable d,
				unsigned long valuemask, XGCValues* values)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(GC,XCreateGC,V_XCreateGC,_ANSI_ARGS_((Display* display, Drawable d,
				unsigned long valuemask, XGCValues* values)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XCreateGC */

#ifndef XCreateGlyphCursor
#ifdef __WIN32__
VFUNC(Cursor,XCreateGlyphCursor,V_XCreateGlyphCursor,_ANSI_ARGS_((Display* d, Font f1,
				Font f2, unsigned int ui1, unsigned int ui2,
				XColor* x1, XColor* x2)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XCreateGlyphCursor */

#ifndef XCreateIC
#ifdef MAC_OSX_TK
VFUNC(XIC,XCreateIC,V_XCreateIC,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(XIC,XCreateIC,V_XCreateIC,_ANSI_ARGS_((void)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(XIC,XCreateIC,V_XCreateIC,_ANSI_ARGS_((void)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XCreateIC */

#ifndef XCreateImage
#ifdef MAC_OSX_TK
VFUNC(XImage *,XCreateImage,V_XCreateImage,_ANSI_ARGS_((Display* d, Visual* v,
				unsigned int ui1, int i1, int i2, char* cp,
				unsigned int ui2, unsigned int ui3, int i3,
				int i4)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(XImage *,XCreateImage,V_XCreateImage,_ANSI_ARGS_((Display* d, Visual* v,
				unsigned int ui1, int i1, int i2, char* cp,
				unsigned int ui2, unsigned int ui3, int i3,
				int i4)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(XImage *,XCreateImage,V_XCreateImage,_ANSI_ARGS_((Display* d, Visual* v,
				unsigned int ui1, int i1, int i2, char* cp,
				unsigned int ui2, unsigned int ui3, int i3,
				int i4)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XCreateImage */

#ifndef XCreatePixmapCursor
#ifdef __WIN32__
VFUNC(Cursor,XCreatePixmapCursor,V_XCreatePixmapCursor,_ANSI_ARGS_((Display* d,
				Pixmap p1, Pixmap p2, XColor* x1, XColor* x2,
				unsigned int ui1, unsigned int ui2)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XCreatePixmapCursor */

#ifndef XDefineCursor
#ifdef MAC_OSX_TK
VFUNC(void,XDefineCursor,V_XDefineCursor,_ANSI_ARGS_((Display* d, Window w,
				Cursor c)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XDefineCursor,V_XDefineCursor,_ANSI_ARGS_((Display* d, Window w,
				Cursor c)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XDefineCursor,V_XDefineCursor,_ANSI_ARGS_((Display* d, Window w,
				Cursor c)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XDefineCursor */

#ifndef XDeleteProperty
#ifdef __WIN32__
VFUNC(void,XDeleteProperty,V_XDeleteProperty,_ANSI_ARGS_((Display* d, Window w,
				Atom a)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XDeleteProperty */

#ifndef XDestroyIC
#ifdef __WIN32__
VFUNC(void,XDestroyIC,V_XDestroyIC,_ANSI_ARGS_((XIC x)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XDestroyIC */

#ifndef XDestroyWindow
#ifdef MAC_OSX_TK
VFUNC(void,XDestroyWindow,V_XDestroyWindow,_ANSI_ARGS_((Display* d, Window w)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XDestroyWindow,V_XDestroyWindow,_ANSI_ARGS_((Display* d, Window w)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XDestroyWindow,V_XDestroyWindow,_ANSI_ARGS_((Display* d, Window w)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XDestroyWindow */

#ifndef XDrawArc
#ifdef MAC_OSX_TK
VFUNC(void,XDrawArc,V_XDrawArc,_ANSI_ARGS_((Display* d, Drawable dr, GC g,
				int i1, int i2, unsigned int ui1,
				unsigned int ui2, int i3, int i4)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XDrawArc,V_XDrawArc,_ANSI_ARGS_((Display* d, Drawable dr, GC g,
				int i1, int i2, unsigned int ui1,
				unsigned int ui2, int i3, int i4)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XDrawArc,V_XDrawArc,_ANSI_ARGS_((Display* d, Drawable dr, GC g,
				int i1, int i2, unsigned int ui1,
				unsigned int ui2, int i3, int i4)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XDrawArc */

#ifndef XDrawLine
#ifdef MAC_OSX_TK
VFUNC(void,XDrawLine,V_XDrawLine,_ANSI_ARGS_((Display* d, Drawable dr, GC g,
				int x1, int y1, int x2, int y2)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XDrawLine,V_XDrawLine,_ANSI_ARGS_((Display* d, Drawable dr, GC g,
				int x1, int y1, int x2, int y2)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XDrawLine,V_XDrawLine,_ANSI_ARGS_((Display* d, Drawable dr, GC g,
				int x1, int y1, int x2, int y2)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XDrawLine */

#ifndef XDrawLines
#ifdef MAC_OSX_TK
VFUNC(void,XDrawLines,V_XDrawLines,_ANSI_ARGS_((Display* d, Drawable dr,
				GC g, XPoint* x, int i1, int i2)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XDrawLines,V_XDrawLines,_ANSI_ARGS_((Display* d, Drawable dr,
				GC g, XPoint* x, int i1, int i2)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XDrawLines,V_XDrawLines,_ANSI_ARGS_((Display* d, Drawable dr,
				GC g, XPoint* x, int i1, int i2)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XDrawLines */

#ifndef XDrawPoint
#ifdef MAC_OSX_TK
VFUNC(void,XDrawPoint,V_XDrawPoint,_ANSI_ARGS_((Display* display, Drawable d,
				GC gc, int x, int y)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XDrawPoint,V_XDrawPoint,_ANSI_ARGS_((Display* display, Drawable d,
				GC gc, int x, int y)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef XDrawPoint */

#ifndef XDrawPoints
#ifdef MAC_OSX_TK
VFUNC(void,XDrawPoints,V_XDrawPoints,_ANSI_ARGS_((Display* display,
				Drawable d, GC gc, XPoint * points,
				int npoints, int mode)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XDrawPoints,V_XDrawPoints,_ANSI_ARGS_((Display* display,
				Drawable d, GC gc, XPoint * points,
				int npoints, int mode)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef XDrawPoints */

#ifndef XDrawRectangle
#ifdef MAC_OSX_TK
VFUNC(void,XDrawRectangle,V_XDrawRectangle,_ANSI_ARGS_((Display* d, Drawable dr,
				GC g, int i1, int i2, unsigned int ui1,
				unsigned int ui2)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XDrawRectangle,V_XDrawRectangle,_ANSI_ARGS_((Display* d, Drawable dr,
				GC g, int i1, int i2, unsigned int ui1,
				unsigned int ui2)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XDrawRectangle,V_XDrawRectangle,_ANSI_ARGS_((Display* d, Drawable dr,
				GC g, int i1, int i2, unsigned int ui1,
				unsigned int ui2)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XDrawRectangle */

#ifndef XDrawSegments
#ifdef MAC_OSX_TK
VFUNC(void,XDrawSegments,V_XDrawSegments,_ANSI_ARGS_((Display * display,
				Drawable d, GC gc, XSegment * segments,
				int nsegments)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XDrawSegments,V_XDrawSegments,_ANSI_ARGS_((Display * display,
				Drawable d, GC gc, XSegment * segments,
				int nsegments)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef XDrawSegments */

#ifndef XFillArc
#ifdef MAC_OSX_TK
VFUNC(void,XFillArc,V_XFillArc,_ANSI_ARGS_((Display* d, Drawable dr, GC g,
				int i1, int i2, unsigned int ui1,
				unsigned int ui2, int i3, int i4)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XFillArc,V_XFillArc,_ANSI_ARGS_((Display* d, Drawable dr, GC g,
				int i1, int i2, unsigned int ui1,
				unsigned int ui2, int i3, int i4)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XFillArc,V_XFillArc,_ANSI_ARGS_((Display* d, Drawable dr, GC g,
				int i1, int i2, unsigned int ui1,
				unsigned int ui2, int i3, int i4)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XFillArc */

#ifndef XFillPolygon
#ifdef MAC_OSX_TK
VFUNC(void,XFillPolygon,V_XFillPolygon,_ANSI_ARGS_((Display* d, Drawable dr,
				GC g, XPoint* x, int i1, int i2, int i3)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XFillPolygon,V_XFillPolygon,_ANSI_ARGS_((Display* d, Drawable dr,
				GC g, XPoint* x, int i1, int i2, int i3)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XFillPolygon,V_XFillPolygon,_ANSI_ARGS_((Display* d, Drawable dr,
				GC g, XPoint* x, int i1, int i2, int i3)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XFillPolygon */

#ifndef XFillRectangle
#ifdef MAC_OSX_TK
VFUNC(void,XFillRectangle,V_XFillRectangle,_ANSI_ARGS_((Display* display,
				Drawable d, GC gc, int x, int y,
				unsigned int width, unsigned int height)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XFillRectangle,V_XFillRectangle,_ANSI_ARGS_((Display* display,
				Drawable d, GC gc, int x, int y,
				unsigned int width, unsigned int height)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XFillRectangle,V_XFillRectangle,_ANSI_ARGS_((Display* display,
				Drawable d, GC gc, int x, int y,
				unsigned int width, unsigned int height)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XFillRectangle */

#ifndef XFillRectangles
#ifdef MAC_OSX_TK
VFUNC(void,XFillRectangles,V_XFillRectangles,_ANSI_ARGS_((Display* d, Drawable dr,
				GC g, XRectangle* x, int i)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XFillRectangles,V_XFillRectangles,_ANSI_ARGS_((Display* d, Drawable dr,
				GC g, XRectangle* x, int i)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XFillRectangles,V_XFillRectangles,_ANSI_ARGS_((Display* d, Drawable dr,
				GC g, XRectangle* x, int i)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XFillRectangles */

#ifndef XFilterEvent
#ifdef __WIN32__
VFUNC(Bool,XFilterEvent,V_XFilterEvent,_ANSI_ARGS_((XEvent* x, Window w)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XFilterEvent */

#ifndef XForceScreenSaver
#ifdef MAC_OSX_TK
VFUNC(void,XForceScreenSaver,V_XForceScreenSaver,_ANSI_ARGS_((Display* display,
				int mode)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XForceScreenSaver,V_XForceScreenSaver,_ANSI_ARGS_((Display* display,
				int mode)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XForceScreenSaver,V_XForceScreenSaver,_ANSI_ARGS_((Display* d, int i)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XForceScreenSaver */

#ifndef XFreeColormap
#ifdef MAC_OSX_TK
VFUNC(void,XFreeColormap,V_XFreeColormap,_ANSI_ARGS_((Display* d, Colormap c)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XFreeColormap,V_XFreeColormap,_ANSI_ARGS_((Display* d, Colormap c)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XFreeColormap,V_XFreeColormap,_ANSI_ARGS_((Display* d, Colormap c)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XFreeColormap */

#ifndef XFreeColors
#ifdef MAC_OSX_TK
VFUNC(void,XFreeColors,V_XFreeColors,_ANSI_ARGS_((Display* d, Colormap c,
				unsigned long* ulp, int i, unsigned long ul)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XFreeColors,V_XFreeColors,_ANSI_ARGS_((Display* d, Colormap c,
				unsigned long* ulp, int i, unsigned long ul)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XFreeColors,V_XFreeColors,_ANSI_ARGS_((Display* d, Colormap c,
				unsigned long* ulp, int i, unsigned long ul)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XFreeColors */

#ifndef XFreeCursor
#ifdef __WIN32__
VFUNC(void,XFreeCursor,V_XFreeCursor,_ANSI_ARGS_((Display* d, Cursor c)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XFreeCursor */

#ifndef XFreeGC
#ifdef MAC_OSX_TK
VFUNC(void,XFreeGC,V_XFreeGC,_ANSI_ARGS_((Display* display, GC gc)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XFreeGC,V_XFreeGC,_ANSI_ARGS_((Display* display, GC gc)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XFreeGC,V_XFreeGC,_ANSI_ARGS_((Display* display, GC gc)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XFreeGC */

#ifndef XFreeModifiermap
#ifdef MAC_OSX_TK
VFUNC(void,XFreeModifiermap,V_XFreeModifiermap,_ANSI_ARGS_((XModifierKeymap* x)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XFreeModifiermap,V_XFreeModifiermap,_ANSI_ARGS_((XModifierKeymap* x)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XFreeModifiermap,V_XFreeModifiermap,_ANSI_ARGS_((XModifierKeymap* x)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XFreeModifiermap */

#ifndef XGContextFromGC
#ifdef MAC_OSX_TK
VFUNC(GContext,XGContextFromGC,V_XGContextFromGC,_ANSI_ARGS_((GC g)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(GContext,XGContextFromGC,V_XGContextFromGC,_ANSI_ARGS_((GC g)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(GContext,XGContextFromGC,V_XGContextFromGC,_ANSI_ARGS_((GC g)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XGContextFromGC */

#ifndef XGetAtomName
#ifdef MAC_OSX_TK
VFUNC(char *,XGetAtomName,V_XGetAtomName,_ANSI_ARGS_((Display* d, Atom a)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(char *,XGetAtomName,V_XGetAtomName,_ANSI_ARGS_((Display* d, Atom a)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(char *,XGetAtomName,V_XGetAtomName,_ANSI_ARGS_((Display* d, Atom a)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XGetAtomName */

#ifndef XGetGeometry
#ifdef MAC_OSX_TK
VFUNC(Status,XGetGeometry,V_XGetGeometry,_ANSI_ARGS_((Display* d, Drawable dr,
				Window* w, int* i1, int* i2,
				unsigned int* ui1, unsigned int* ui2,
				unsigned int* ui3, unsigned int* ui4)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(Status,XGetGeometry,V_XGetGeometry,_ANSI_ARGS_((Display* d, Drawable dr,
				Window* w, int* i1, int* i2,
				unsigned int* ui1, unsigned int* ui2,
				unsigned int* ui3, unsigned int* ui4)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(Status,XGetGeometry,V_XGetGeometry,_ANSI_ARGS_((Display* d, Drawable dr,
				Window* w, int* i1, int* i2,
				unsigned int* ui1, unsigned int* ui2,
				unsigned int* ui3, unsigned int* ui4)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XGetGeometry */

#ifndef XGetImage
#ifdef MAC_OSX_TK
VFUNC(XImage *,XGetImage,V_XGetImage,_ANSI_ARGS_((Display* d, Drawable dr,
				int i1, int i2, unsigned int ui1,
				unsigned int ui2, unsigned long ul, int i3)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(XImage *,XGetImage,V_XGetImage,_ANSI_ARGS_((Display* d, Drawable dr,
				int i1, int i2, unsigned int ui1,
				unsigned int ui2, unsigned long ul, int i3)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(XImage *,XGetImage,V_XGetImage,_ANSI_ARGS_((Display* d, Drawable dr,
				int i1, int i2, unsigned int ui1,
				unsigned int ui2, unsigned long ul, int i3)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XGetImage */

#ifndef XGetInputFocus
#ifdef __WIN32__
VFUNC(void,XGetInputFocus,V_XGetInputFocus,_ANSI_ARGS_((Display* d, Window* w,
				int* i)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XGetInputFocus */

#ifndef XGetModifierMapping
#ifdef MAC_OSX_TK
VFUNC(XModifierKeymap*,XGetModifierMapping,V_XGetModifierMapping,_ANSI_ARGS_((Display* d)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(XModifierKeymap*,XGetModifierMapping,V_XGetModifierMapping,_ANSI_ARGS_((Display* d)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(XModifierKeymap*,XGetModifierMapping,V_XGetModifierMapping,_ANSI_ARGS_((Display* d)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XGetModifierMapping */

#ifndef XGetVisualInfo
#ifdef MAC_OSX_TK
VFUNC(XVisualInfo *,XGetVisualInfo,V_XGetVisualInfo,_ANSI_ARGS_((Display* display,
				long vinfo_mask, XVisualInfo* vinfo_template,
				int* nitems_return)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(XVisualInfo *,XGetVisualInfo,V_XGetVisualInfo,_ANSI_ARGS_((Display* display,
				long vinfo_mask, XVisualInfo* vinfo_template,
				int* nitems_return)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(XVisualInfo *,XGetVisualInfo,V_XGetVisualInfo,_ANSI_ARGS_((Display* display,
				long vinfo_mask, XVisualInfo* vinfo_template,
				int* nitems_return)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XGetVisualInfo */

#ifndef XGetWMColormapWindows
#ifdef __WIN32__
VFUNC(Status,XGetWMColormapWindows,V_XGetWMColormapWindows,_ANSI_ARGS_((Display* d,
				Window w, Window** wpp, int* ip)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XGetWMColormapWindows */

#ifndef XGetWindowAttributes
#ifdef __WIN32__
VFUNC(Status,XGetWindowAttributes,V_XGetWindowAttributes,_ANSI_ARGS_((Display* d,
				Window w, XWindowAttributes* x)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XGetWindowAttributes */

#ifndef XGetWindowProperty
#ifdef MAC_OSX_TK
VFUNC(int,XGetWindowProperty,V_XGetWindowProperty,_ANSI_ARGS_((Display* d, Window w,
				Atom a1, long l1, long l2, Bool b, Atom a2,
				Atom* ap, int* ip, unsigned long* ulp1,
				unsigned long* ulp2, unsigned char** cpp)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(int,XGetWindowProperty,V_XGetWindowProperty,_ANSI_ARGS_((Display* d, Window w,
				Atom a1, long l1, long l2, Bool b, Atom a2,
				Atom* ap, int* ip, unsigned long* ulp1,
				unsigned long* ulp2, unsigned char** cpp)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(int,XGetWindowProperty,V_XGetWindowProperty,_ANSI_ARGS_((Display* d, Window w,
				Atom a1, long l1, long l2, Bool b, Atom a2,
				Atom* ap, int* ip, unsigned long* ulp1,
				unsigned long* ulp2, unsigned char** cpp)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XGetWindowProperty */

#ifndef XGrabKeyboard
#ifdef MAC_OSX_TK
VFUNC(int,XGrabKeyboard,V_XGrabKeyboard,_ANSI_ARGS_((Display* d, Window w,
				Bool b, int i1, int i2, Time t)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(int,XGrabKeyboard,V_XGrabKeyboard,_ANSI_ARGS_((Display* d, Window w,
				Bool b, int i1, int i2, Time t)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(int,XGrabKeyboard,V_XGrabKeyboard,_ANSI_ARGS_((Display* d, Window w,
				Bool b, int i1, int i2, Time t)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XGrabKeyboard */

#ifndef XGrabPointer
#ifdef MAC_OSX_TK
VFUNC(int,XGrabPointer,V_XGrabPointer,_ANSI_ARGS_((Display* d, Window w1,
				Bool b, unsigned int ui, int i1, int i2,
				Window w2, Cursor c, Time t)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(int,XGrabPointer,V_XGrabPointer,_ANSI_ARGS_((Display* d, Window w1,
				Bool b, unsigned int ui, int i1, int i2,
				Window w2, Cursor c, Time t)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(int,XGrabPointer,V_XGrabPointer,_ANSI_ARGS_((Display* d, Window w1,
				Bool b, unsigned int ui, int i1, int i2,
				Window w2, Cursor c, Time t)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XGrabPointer */

#ifndef XIconifyWindow
#ifdef __WIN32__
VFUNC(Status,XIconifyWindow,V_XIconifyWindow,_ANSI_ARGS_((Display* d, Window w,
				int i)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XIconifyWindow */

#ifndef XInternAtom
#ifdef MAC_OSX_TK
VFUNC(Atom,XInternAtom,V_XInternAtom,_ANSI_ARGS_((Display* display,
				_Xconst char* atom_name, Bool only_if_exists)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(Atom,XInternAtom,V_XInternAtom,_ANSI_ARGS_((Display* display,
				_Xconst char* atom_name, Bool only_if_exists)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(Atom,XInternAtom,V_XInternAtom,_ANSI_ARGS_((Display* display,
				_Xconst char* atom_name, Bool only_if_exists)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XInternAtom */

#ifndef XKeycodeToKeysym
#ifdef MAC_OSX_TK
VFUNC(KeySym,XKeycodeToKeysym,V_XKeycodeToKeysym,_ANSI_ARGS_((Display* d, KeyCode k,
				int i)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(KeySym,XKeycodeToKeysym,V_XKeycodeToKeysym,_ANSI_ARGS_((Display* d, KeyCode k,
				int i)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(KeySym,XKeycodeToKeysym,V_XKeycodeToKeysym,_ANSI_ARGS_((Display* d,
				unsigned int k, int i)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XKeycodeToKeysym */

#ifndef XKeysymToKeycode
#ifdef MAC_OSX_TK
VFUNC(KeyCode,XKeysymToKeycode,V_XKeysymToKeycode,_ANSI_ARGS_((Display* d, KeySym k)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(KeyCode,XKeysymToKeycode,V_XKeysymToKeycode,_ANSI_ARGS_((Display* d, KeySym k)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(KeyCode,XKeysymToKeycode,V_XKeysymToKeycode,_ANSI_ARGS_((Display* d, KeySym k)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XKeysymToKeycode */

#ifndef XKeysymToString
#ifdef MAC_OSX_TK
VFUNC(char *,XKeysymToString,V_XKeysymToString,_ANSI_ARGS_((KeySym k)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(char *,XKeysymToString,V_XKeysymToString,_ANSI_ARGS_((KeySym k)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(char *,XKeysymToString,V_XKeysymToString,_ANSI_ARGS_((KeySym k)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XKeysymToString */

#ifndef XListHosts
#ifdef __WIN32__
VFUNC(XHostAddress *,XListHosts,V_XListHosts,_ANSI_ARGS_((Display* d, int* i, Bool* b)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XListHosts */

#ifndef XLookupColor
#ifdef __WIN32__
VFUNC(Status,XLookupColor,V_XLookupColor,_ANSI_ARGS_((Display* d, Colormap c1,
				_Xconst char* c2, XColor* x1, XColor* x2)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XLookupColor */

#ifndef XMapWindow
#ifdef MAC_OSX_TK
VFUNC(void,XMapWindow,V_XMapWindow,_ANSI_ARGS_((Display* d, Window w)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XMapWindow,V_XMapWindow,_ANSI_ARGS_((Display* d, Window w)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XMapWindow,V_XMapWindow,_ANSI_ARGS_((Display* d, Window w)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XMapWindow */

#ifndef XMoveResizeWindow
#ifdef MAC_OSX_TK
VFUNC(void,XMoveResizeWindow,V_XMoveResizeWindow,_ANSI_ARGS_((Display* d, Window w,
				int i1, int i2, unsigned int ui1,
				unsigned int ui2)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XMoveResizeWindow,V_XMoveResizeWindow,_ANSI_ARGS_((Display* d, Window w,
				int i1, int i2, unsigned int ui1,
				unsigned int ui2)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XMoveResizeWindow,V_XMoveResizeWindow,_ANSI_ARGS_((Display* d, Window w,
				int i1, int i2, unsigned int ui1,
				unsigned int ui2)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XMoveResizeWindow */

#ifndef XMoveWindow
#ifdef MAC_OSX_TK
VFUNC(void,XMoveWindow,V_XMoveWindow,_ANSI_ARGS_((Display* d, Window w,
				int i1, int i2)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XMoveWindow,V_XMoveWindow,_ANSI_ARGS_((Display* d, Window w,
				int i1, int i2)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XMoveWindow,V_XMoveWindow,_ANSI_ARGS_((Display* d, Window w,
				int i1, int i2)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XMoveWindow */

#ifndef XNextEvent
#ifdef __WIN32__
VFUNC(void,XNextEvent,V_XNextEvent,_ANSI_ARGS_((Display* d, XEvent* x)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XNextEvent */

#ifndef XParseColor
#ifdef MAC_OSX_TK
VFUNC(Status,XParseColor,V_XParseColor,_ANSI_ARGS_((Display * display,
				Colormap map, _Xconst char* spec,
				XColor * colorPtr)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(Status,XParseColor,V_XParseColor,_ANSI_ARGS_((Display * display,
				Colormap map, _Xconst char* spec,
				XColor * colorPtr)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(Status,XParseColor,V_XParseColor,_ANSI_ARGS_((Display * display,
				Colormap map, _Xconst char* spec,
				XColor * colorPtr)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XParseColor */

#ifndef XPutBackEvent
#ifdef __WIN32__
VFUNC(void,XPutBackEvent,V_XPutBackEvent,_ANSI_ARGS_((Display* d, XEvent* x)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XPutBackEvent */

#ifndef XQueryColor
#ifdef MAC_OSX_TK
VFUNC(void,XQueryColor,V_XQueryColor,_ANSI_ARGS_((Display * display,
				Colormap colormap, XColor * def_in_out)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XQueryColor,V_XQueryColor,_ANSI_ARGS_((Display * display,
				Colormap colormap, XColor * def_in_out)))
#endif /* #ifdef MAC_TCL */
#endif /* #ifndef XQueryColor */

#ifndef XQueryColors
#ifdef MAC_OSX_TK
VFUNC(void,XQueryColors,V_XQueryColors,_ANSI_ARGS_((Display * display,
				Colormap colormap, XColor * defs_in_out,
				int ncolors)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XQueryColors,V_XQueryColors,_ANSI_ARGS_((Display * display,
				Colormap colormap, XColor * defs_in_out,
				int ncolors)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XQueryColors,V_XQueryColors,_ANSI_ARGS_((Display* d, Colormap c,
				XColor* x, int i)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XQueryColors */

#ifndef XQueryPointer
#ifdef MAC_OSX_TK
VFUNC(Bool,XQueryPointer,V_XQueryPointer,_ANSI_ARGS_((Display* d, Window w1,
				Window* w2, Window* w3, int* i1, int* i2,
				int* i3, int* i4, unsigned int* ui)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(Bool,XQueryPointer,V_XQueryPointer,_ANSI_ARGS_((Display* d, Window w1,
				Window* w2, Window* w3, int* i1, int* i2,
				int* i3, int* i4, unsigned int* ui)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(Bool,XQueryPointer,V_XQueryPointer,_ANSI_ARGS_((Display* d, Window w1,
				Window* w2, Window* w3, int* i1, int* i2,
				int* i3, int* i4, unsigned int* ui)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XQueryPointer */

#ifndef XQueryTree
#ifdef MAC_OSX_TK
VFUNC(Status,XQueryTree,V_XQueryTree,_ANSI_ARGS_((Display* d, Window w1,
				Window* w2, Window* w3, Window** w4,
				unsigned int* ui)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(Status,XQueryTree,V_XQueryTree,_ANSI_ARGS_((Display* d, Window w1,
				Window* w2, Window* w3, Window** w4,
				unsigned int* ui)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(Status,XQueryTree,V_XQueryTree,_ANSI_ARGS_((Display* d, Window w1,
				Window* w2, Window* w3, Window** w4,
				unsigned int* ui)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XQueryTree */

#ifndef XRaiseWindow
#ifdef MAC_OSX_TK
VFUNC(void,XRaiseWindow,V_XRaiseWindow,_ANSI_ARGS_((Display* d, Window w)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XRaiseWindow,V_XRaiseWindow,_ANSI_ARGS_((Display* d, Window w)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XRaiseWindow,V_XRaiseWindow,_ANSI_ARGS_((Display* d, Window w)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XRaiseWindow */

#ifndef XRefreshKeyboardMapping
#ifdef MAC_OSX_TK
VFUNC(void,XRefreshKeyboardMapping,V_XRefreshKeyboardMapping,_ANSI_ARGS_((
				XMappingEvent* x)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XRefreshKeyboardMapping,V_XRefreshKeyboardMapping,_ANSI_ARGS_((
				XMappingEvent* x)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XRefreshKeyboardMapping,V_XRefreshKeyboardMapping,_ANSI_ARGS_((
				XMappingEvent* x)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XRefreshKeyboardMapping */

#ifndef XResizeWindow
#ifdef MAC_OSX_TK
VFUNC(void,XResizeWindow,V_XResizeWindow,_ANSI_ARGS_((Display* d, Window w,
				unsigned int ui1, unsigned int ui2)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XResizeWindow,V_XResizeWindow,_ANSI_ARGS_((Display* d, Window w,
				unsigned int ui1, unsigned int ui2)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XResizeWindow,V_XResizeWindow,_ANSI_ARGS_((Display* d, Window w,
				unsigned int ui1, unsigned int ui2)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XResizeWindow */

#ifndef XRootWindow
#ifdef MAC_OSX_TK
VFUNC(Window,XRootWindow,V_XRootWindow,_ANSI_ARGS_((Display* d, int i)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(Window,XRootWindow,V_XRootWindow,_ANSI_ARGS_((Display* d, int i)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(Window,XRootWindow,V_XRootWindow,_ANSI_ARGS_((Display* d, int i)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XRootWindow */

#ifndef XSelectInput
#ifdef MAC_OSX_TK
VFUNC(void,XSelectInput,V_XSelectInput,_ANSI_ARGS_((Display* d, Window w,
				long l)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSelectInput,V_XSelectInput,_ANSI_ARGS_((Display* d, Window w,
				long l)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSelectInput,V_XSelectInput,_ANSI_ARGS_((Display* d, Window w,
				long l)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSelectInput */

#ifndef XSendEvent
#ifdef MAC_OSX_TK
VFUNC(Status,XSendEvent,V_XSendEvent,_ANSI_ARGS_((Display* d, Window w, Bool b,
				long l, XEvent* x)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(Status,XSendEvent,V_XSendEvent,_ANSI_ARGS_((Display* d, Window w, Bool b,
				long l, XEvent* x)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(Status,XSendEvent,V_XSendEvent,_ANSI_ARGS_((Display* d, Window w, Bool b,
				long l, XEvent* x)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSendEvent */

#ifndef XSetArcMode
#ifdef MAC_OSX_TK
VFUNC(void,XSetArcMode,V_XSetArcMode,_ANSI_ARGS_((Display * display, GC gc,
				int arc_mode)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSetArcMode,V_XSetArcMode,_ANSI_ARGS_((Display * display, GC gc,
				int arc_mode)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSetArcMode,V_XSetArcMode,_ANSI_ARGS_((Display * display, GC gc,
				int arc_mode)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetArcMode */

#ifndef XSetBackground
#ifdef MAC_OSX_TK
VFUNC(void,XSetBackground,V_XSetBackground,_ANSI_ARGS_((Display* display, GC gc,
				unsigned long foreground)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSetBackground,V_XSetBackground,_ANSI_ARGS_((Display* display, GC gc,
				unsigned long foreground)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSetBackground,V_XSetBackground,_ANSI_ARGS_((Display* display, GC gc,
				unsigned long foreground)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetBackground */

#ifndef XSetClipMask
#ifdef MAC_OSX_TK
VFUNC(void,XSetClipMask,V_XSetClipMask,_ANSI_ARGS_((Display* display, GC gc,
				Pixmap pixmap)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSetClipMask,V_XSetClipMask,_ANSI_ARGS_((Display* display, GC gc,
				Pixmap pixmap)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSetClipMask,V_XSetClipMask,_ANSI_ARGS_((Display* display, GC gc,
				Pixmap pixmap)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetClipMask */

#ifndef XSetClipOrigin
#ifdef MAC_OSX_TK
VFUNC(void,XSetClipOrigin,V_XSetClipOrigin,_ANSI_ARGS_((Display* display, GC gc,
				int clip_x_origin, int clip_y_origin)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSetClipOrigin,V_XSetClipOrigin,_ANSI_ARGS_((Display* display, GC gc,
				int clip_x_origin, int clip_y_origin)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSetClipOrigin,V_XSetClipOrigin,_ANSI_ARGS_((Display* display, GC gc,
				int clip_x_origin, int clip_y_origin)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetClipOrigin */

#ifndef XSetCommand
#ifdef __WIN32__
VFUNC(void,XSetCommand,V_XSetCommand,_ANSI_ARGS_((Display* d, Window w,
				CONST char** c, int i)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetCommand */

#ifndef XSetDashes
#ifdef MAC_OSX_TK
VFUNC(void,XSetDashes,V_XSetDashes,_ANSI_ARGS_((Display* display, GC gc,
				int dash_offset, _Xconst char* dash_list,
				int n)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSetDashes,V_XSetDashes,_ANSI_ARGS_((Display* display, GC gc,
				int dash_offset, _Xconst char* dash_list,
				int n)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSetDashes,V_XSetDashes,_ANSI_ARGS_((Display* display, GC gc,
				int dash_offset, _Xconst char* dash_list,
				int n)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetDashes */

#ifndef XSetErrorHandler
#ifdef MAC_OSX_TK
VFUNC(XErrorHandler,XSetErrorHandler,V_XSetErrorHandler,_ANSI_ARGS_((XErrorHandler x)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(XErrorHandler,XSetErrorHandler,V_XSetErrorHandler,_ANSI_ARGS_((XErrorHandler x)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(XErrorHandler,XSetErrorHandler,V_XSetErrorHandler,_ANSI_ARGS_((XErrorHandler x)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetErrorHandler */

#ifndef XSetFillRule
#ifdef MAC_OSX_TK
VFUNC(void,XSetFillRule,V_XSetFillRule,_ANSI_ARGS_((Display * display, GC gc,
				int fill_rule)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSetFillRule,V_XSetFillRule,_ANSI_ARGS_((Display * display, GC gc,
				int fill_rule)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSetFillRule,V_XSetFillRule,_ANSI_ARGS_((Display * display, GC gc,
				int fill_rule)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetFillRule */

#ifndef XSetFillStyle
#ifdef MAC_OSX_TK
VFUNC(void,XSetFillStyle,V_XSetFillStyle,_ANSI_ARGS_((Display * display, GC gc,
				int fill_style)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSetFillStyle,V_XSetFillStyle,_ANSI_ARGS_((Display * display, GC gc,
				int fill_style)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSetFillStyle,V_XSetFillStyle,_ANSI_ARGS_((Display * display, GC gc,
				int fill_style)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetFillStyle */

#ifndef XSetFont
#ifdef MAC_OSX_TK
VFUNC(void,XSetFont,V_XSetFont,_ANSI_ARGS_((Display * display, GC gc,
				Font font)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSetFont,V_XSetFont,_ANSI_ARGS_((Display * display, GC gc,
				Font font)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSetFont,V_XSetFont,_ANSI_ARGS_((Display * display, GC gc,
				Font font)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetFont */

#ifndef XSetForeground
#ifdef MAC_OSX_TK
VFUNC(void,XSetForeground,V_XSetForeground,_ANSI_ARGS_((Display* display, GC gc,
				unsigned long foreground)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSetForeground,V_XSetForeground,_ANSI_ARGS_((Display* display, GC gc,
				unsigned long foreground)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSetForeground,V_XSetForeground,_ANSI_ARGS_((Display* display, GC gc,
				unsigned long foreground)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetForeground */

#ifndef XSetFunction
#ifdef MAC_OSX_TK
VFUNC(void,XSetFunction,V_XSetFunction,_ANSI_ARGS_((Display * display, GC gc,
				int function)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSetFunction,V_XSetFunction,_ANSI_ARGS_((Display * display, GC gc,
				int function)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSetFunction,V_XSetFunction,_ANSI_ARGS_((Display * display, GC gc,
				int function)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetFunction */

#ifndef XSetIconName
#ifdef MAC_OSX_TK
VFUNC(void,XSetIconName,V_XSetIconName,_ANSI_ARGS_((Display* d, Window w,
				_Xconst char* c)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSetIconName,V_XSetIconName,_ANSI_ARGS_((Display* d, Window w,
				_Xconst char* c)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSetIconName,V_XSetIconName,_ANSI_ARGS_((Display* d, Window w,
				_Xconst char* c)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetIconName */

#ifndef XSetInputFocus
#ifdef MAC_OSX_TK
VFUNC(void,XSetInputFocus,V_XSetInputFocus,_ANSI_ARGS_((Display* d, Window w,
				int i, Time t)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSetInputFocus,V_XSetInputFocus,_ANSI_ARGS_((Display* d, Window w,
				int i, Time t)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSetInputFocus,V_XSetInputFocus,_ANSI_ARGS_((Display* d, Window w,
				int i, Time t)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetInputFocus */

#ifndef XSetLineAttributes
#ifdef MAC_OSX_TK
VFUNC(void,XSetLineAttributes,V_XSetLineAttributes,_ANSI_ARGS_((Display * display,
				GC gc, unsigned int line_width,
				int line_style, int cap_style,
				int join_style)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSetLineAttributes,V_XSetLineAttributes,_ANSI_ARGS_((Display * display,
				GC gc, unsigned int line_width,
				int line_style, int cap_style,
				int join_style)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSetLineAttributes,V_XSetLineAttributes,_ANSI_ARGS_((Display * display,
				GC gc, unsigned int line_width,
				int line_style, int cap_style,
				int join_style)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetLineAttributes */

#ifndef XSetSelectionOwner
#ifdef MAC_OSX_TK
VFUNC(void,XSetSelectionOwner,V_XSetSelectionOwner,_ANSI_ARGS_((Display* d, Atom a,
				Window w, Time t)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSetSelectionOwner,V_XSetSelectionOwner,_ANSI_ARGS_((Display* d, Atom a,
				Window w, Time t)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSetSelectionOwner,V_XSetSelectionOwner,_ANSI_ARGS_((Display* d, Atom a,
				Window w, Time t)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetSelectionOwner */

#ifndef XSetStipple
#ifdef MAC_OSX_TK
VFUNC(void,XSetStipple,V_XSetStipple,_ANSI_ARGS_((Display * display, GC gc,
				Pixmap stipple)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSetStipple,V_XSetStipple,_ANSI_ARGS_((Display * display, GC gc,
				Pixmap stipple)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSetStipple,V_XSetStipple,_ANSI_ARGS_((Display * display, GC gc,
				Pixmap stipple)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetStipple */

#ifndef XSetTSOrigin
#ifdef MAC_OSX_TK
VFUNC(void,XSetTSOrigin,V_XSetTSOrigin,_ANSI_ARGS_((Display* display, GC gc,
				int ts_x_origin, int ts_y_origin)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSetTSOrigin,V_XSetTSOrigin,_ANSI_ARGS_((Display* display, GC gc,
				int ts_x_origin, int ts_y_origin)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSetTSOrigin,V_XSetTSOrigin,_ANSI_ARGS_((Display* display, GC gc,
				int ts_x_origin, int ts_y_origin)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetTSOrigin */

#ifndef XSetWMClientMachine
#ifdef MAC_OSX_TK
VFUNC(void,XSetWMClientMachine,V_XSetWMClientMachine,_ANSI_ARGS_((Display* display,
				Window w, XTextProperty* text_prop)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSetWMClientMachine,V_XSetWMClientMachine,_ANSI_ARGS_((Display* display,
				Window w, XTextProperty* text_prop)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSetWMClientMachine,V_XSetWMClientMachine,_ANSI_ARGS_((Display* display,
				Window w, XTextProperty* text_prop)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetWMClientMachine */

#ifndef XSetWindowBackground
#ifdef MAC_OSX_TK
VFUNC(void,XSetWindowBackground,V_XSetWindowBackground,_ANSI_ARGS_((Display* d,
				Window w, unsigned long ul)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSetWindowBackground,V_XSetWindowBackground,_ANSI_ARGS_((Display* d,
				Window w, unsigned long ul)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSetWindowBackground,V_XSetWindowBackground,_ANSI_ARGS_((Display* d,
				Window w, unsigned long ul)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetWindowBackground */

#ifndef XSetWindowBackgroundPixmap
#ifdef MAC_OSX_TK
VFUNC(void,XSetWindowBackgroundPixmap,V_XSetWindowBackgroundPixmap,_ANSI_ARGS_((Display* d,
				Window w, Pixmap p)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSetWindowBackgroundPixmap,V_XSetWindowBackgroundPixmap,_ANSI_ARGS_((Display* d,
				Window w, Pixmap p)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSetWindowBackgroundPixmap,V_XSetWindowBackgroundPixmap,_ANSI_ARGS_((Display* d,
				Window w, Pixmap p)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetWindowBackgroundPixmap */

#ifndef XSetWindowBorder
#ifdef MAC_OSX_TK
VFUNC(void,XSetWindowBorder,V_XSetWindowBorder,_ANSI_ARGS_((Display* d, Window w,
				unsigned long ul)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSetWindowBorder,V_XSetWindowBorder,_ANSI_ARGS_((Display* d, Window w,
				unsigned long ul)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSetWindowBorder,V_XSetWindowBorder,_ANSI_ARGS_((Display* d, Window w,
				unsigned long ul)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetWindowBorder */

#ifndef XSetWindowBorderPixmap
#ifdef MAC_OSX_TK
VFUNC(void,XSetWindowBorderPixmap,V_XSetWindowBorderPixmap,_ANSI_ARGS_((Display* d,
				Window w, Pixmap p)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSetWindowBorderPixmap,V_XSetWindowBorderPixmap,_ANSI_ARGS_((Display* d,
				Window w, Pixmap p)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSetWindowBorderPixmap,V_XSetWindowBorderPixmap,_ANSI_ARGS_((Display* d,
				Window w, Pixmap p)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetWindowBorderPixmap */

#ifndef XSetWindowBorderWidth
#ifdef MAC_OSX_TK
VFUNC(void,XSetWindowBorderWidth,V_XSetWindowBorderWidth,_ANSI_ARGS_((Display* d,
				Window w, unsigned int ui)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSetWindowBorderWidth,V_XSetWindowBorderWidth,_ANSI_ARGS_((Display* d,
				Window w, unsigned int ui)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSetWindowBorderWidth,V_XSetWindowBorderWidth,_ANSI_ARGS_((Display* d,
				Window w, unsigned int ui)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetWindowBorderWidth */

#ifndef XSetWindowColormap
#ifdef MAC_OSX_TK
VFUNC(void,XSetWindowColormap,V_XSetWindowColormap,_ANSI_ARGS_((Display* d, Window w,
				Colormap c)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XSetWindowColormap,V_XSetWindowColormap,_ANSI_ARGS_((Display* d, Window w,
				Colormap c)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XSetWindowColormap,V_XSetWindowColormap,_ANSI_ARGS_((Display* d, Window w,
				Colormap c)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XSetWindowColormap */

#ifndef XStringListToTextProperty
#ifdef MAC_OSX_TK
VFUNC(Status,XStringListToTextProperty,V_XStringListToTextProperty,_ANSI_ARGS_((char** list,
				int count, XTextProperty* text_prop_return)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(Status,XStringListToTextProperty,V_XStringListToTextProperty,_ANSI_ARGS_((char** list,
				int count, XTextProperty* text_prop_return)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(Status,XStringListToTextProperty,V_XStringListToTextProperty,_ANSI_ARGS_((char** list,
				int count, XTextProperty* text_prop_return)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XStringListToTextProperty */

#ifndef XStringToKeysym
#ifdef MAC_OSX_TK
VFUNC(KeySym,XStringToKeysym,V_XStringToKeysym,_ANSI_ARGS_((_Xconst char* c)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(KeySym,XStringToKeysym,V_XStringToKeysym,_ANSI_ARGS_((_Xconst char* c)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(KeySym,XStringToKeysym,V_XStringToKeysym,_ANSI_ARGS_((_Xconst char* c)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XStringToKeysym */

#ifndef XTranslateCoordinates
#ifdef __WIN32__
VFUNC(Bool,XTranslateCoordinates,V_XTranslateCoordinates,_ANSI_ARGS_((Display* d,
				Window w1, Window w2, int i1, int i2,
				int* i3, int* i4, Window* w3)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XTranslateCoordinates */

#ifndef XUngrabKeyboard
#ifdef MAC_OSX_TK
VFUNC(void,XUngrabKeyboard,V_XUngrabKeyboard,_ANSI_ARGS_((Display* d, Time t)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XUngrabKeyboard,V_XUngrabKeyboard,_ANSI_ARGS_((Display* d, Time t)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XUngrabKeyboard,V_XUngrabKeyboard,_ANSI_ARGS_((Display* d, Time t)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XUngrabKeyboard */

#ifndef XUngrabPointer
#ifdef MAC_OSX_TK
VFUNC(void,XUngrabPointer,V_XUngrabPointer,_ANSI_ARGS_((Display* d, Time t)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XUngrabPointer,V_XUngrabPointer,_ANSI_ARGS_((Display* d, Time t)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XUngrabPointer,V_XUngrabPointer,_ANSI_ARGS_((Display* d, Time t)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XUngrabPointer */

#ifndef XUnmapWindow
#ifdef MAC_OSX_TK
VFUNC(void,XUnmapWindow,V_XUnmapWindow,_ANSI_ARGS_((Display* d, Window w)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XUnmapWindow,V_XUnmapWindow,_ANSI_ARGS_((Display* d, Window w)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XUnmapWindow,V_XUnmapWindow,_ANSI_ARGS_((Display* d, Window w)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XUnmapWindow */

#ifndef XWarpPointer
#ifdef MAC_OSX_TK
VFUNC(void,XWarpPointer,V_XWarpPointer,_ANSI_ARGS_((Display* display,
				Window src_w, Window dest_w, int src_x,
				int src_y, unsigned int src_width,
				unsigned int src_height, int dest_x,
				int dest_y)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(void,XWarpPointer,V_XWarpPointer,_ANSI_ARGS_((Display* display,
				Window src_w, Window dest_w, int src_x,
				int src_y, unsigned int src_width,
				unsigned int src_height, int dest_x,
				int dest_y)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(void,XWarpPointer,V_XWarpPointer,_ANSI_ARGS_((Display* d, Window s,
				Window dw, int sx, int sy, unsigned int sw,
				unsigned int sh, int dx, int dy)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XWarpPointer */

#ifndef XWindowEvent
#ifdef __WIN32__
VFUNC(void,XWindowEvent,V_XWindowEvent,_ANSI_ARGS_((Display* d, Window w,
				long l, XEvent* x)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XWindowEvent */

#ifndef XWithdrawWindow
#ifdef __WIN32__
VFUNC(Status,XWithdrawWindow,V_XWithdrawWindow,_ANSI_ARGS_((Display* d, Window w,
				int i)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XWithdrawWindow */

#ifndef XmbLookupString
#ifdef __WIN32__
VFUNC(int,XmbLookupString,V_XmbLookupString,_ANSI_ARGS_((XIC xi,
				XKeyPressedEvent* xk, char* c, int i,
				KeySym* k, Status* s)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef XmbLookupString */

#ifndef _XInitImageFuncPtrs
#ifdef MAC_OSX_TK
VFUNC(int,_XInitImageFuncPtrs,V__XInitImageFuncPtrs,_ANSI_ARGS_((XImage * image)))
#endif /* #ifdef MAC_OSX_TK */
#ifdef MAC_TCL
VFUNC(int,_XInitImageFuncPtrs,V__XInitImageFuncPtrs,_ANSI_ARGS_((XImage * image)))
#endif /* #ifdef MAC_TCL */
#ifdef __WIN32__
VFUNC(int,_XInitImageFuncPtrs,V__XInitImageFuncPtrs,_ANSI_ARGS_((XImage * image)))
#endif /* #ifdef __WIN32__ */
#endif /* #ifndef _XInitImageFuncPtrs */

#endif /* _TKINTXLIBDECLS */
