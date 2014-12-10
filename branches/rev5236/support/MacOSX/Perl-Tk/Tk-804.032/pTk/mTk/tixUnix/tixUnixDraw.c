
/*	$Id: tixUnixDraw.c,v 1.1.1.1 2000/05/17 11:08:54 idiscovery Exp $	*/

/*
 * tixUnixDraw.c --
 *
 *	Implement the Unix specific function calls for drawing.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include "tixPort.h"
#include "tixUnixInt.h"


/*
 *----------------------------------------------------------------------
 * TixpDrawTmpLine --
 *
 *	Draws a "temporary" line between the two points. The line can be
 *	removed by calling the function again with the same parameters.
 *
 * Results:
 *	Standard Tcl result.
 *
 * Side effects:
 *	A line is XOR'ed onto the screen.
 *----------------------------------------------------------------------
 */
void
TixpDrawTmpLine(x1, y1, x2, y2, tkwin)
    int x1;
    int y1;
    int x2;
    int y2;
    Tk_Window tkwin;
{
    GC gc;
    XGCValues values;
    unsigned long valuemask = GCForeground | GCSubwindowMode | GCFunction;
    Window winId;		/* The Window to draw into. */
    Tk_Window toplevel;		/* Toplevel containing the tkwin. */
    int rootx1, rooty1;		/* Root x and y of the toplevel window. */
    int rootx2, rooty2;

    for (toplevel=tkwin; !Tk_IsTopLevel(toplevel);
	    toplevel=Tk_Parent(toplevel)) {
	;
    }

    Tk_GetRootCoords(toplevel, &rootx1, &rooty1);
    rootx2 = rootx1 + Tk_Width(toplevel)  - 1;
    rooty2 = rooty1 + Tk_Height(toplevel) - 1;

    if (x1 >= rootx1 && x2 <= rootx2 &&	y1 >= rooty1 && y2 <= rooty2) {
	/*
	 * The line is completely inside the toplevel containing
	 * tkwin. It's better to draw into this window because on some
	 * X servers, especially PC X Servers running on Windows,
	 * drawing into the root window shows no effect.
	 */
	winId = Tk_WindowId(toplevel);
	x1 -= rootx1;
	y1 -= rooty1;
	x2 -= rootx1;
	y2 -= rooty1;
    } else {
	winId = XRootWindow(Tk_Display(tkwin), Tk_ScreenNumber(tkwin));
    }

    values.foreground	  = 0xff;
    values.subwindow_mode = IncludeInferiors;
    values.function	  = GXxor;

    gc = XCreateGC(Tk_Display(tkwin), winId, valuemask, &values);
    XDrawLine(Tk_Display(tkwin), winId, gc, x1, y1, x2, y2);
    XFreeGC(Tk_Display(tkwin), gc);
}

/*----------------------------------------------------------------------
 * TixpDrawAnchorLines --
 *
 *	See comments near Tix_DrawAnchorLines.
 *----------------------------------------------------------------------
 */

void TixpDrawAnchorLines(display, drawable, gc, x, y, w, h)
    Display *display;
    Drawable drawable;
    GC gc;
    int x;
    int y;
    int w;
    int h;
{
    XPoint points[4];

    if (w < 1) {
	w = 1;
    }
    if (h < 1) {
	h = 1;
    }

    XDrawRectangle(display, drawable, gc, x, y, w-1, h-1);

    /*
     * Draw these points so that the corners will not be rounded
     */
    points[0].x = x;
    points[0].y = y;
    points[1].x = x + w - 1;
    points[1].y = y;
    points[2].x = x;
    points[2].y = y + h - 1;
    points[3].x = x + w - 1;
    points[3].y = y + h - 1;

    XDrawPoints(display, drawable, gc, points, 4, CoordModeOrigin);
}


