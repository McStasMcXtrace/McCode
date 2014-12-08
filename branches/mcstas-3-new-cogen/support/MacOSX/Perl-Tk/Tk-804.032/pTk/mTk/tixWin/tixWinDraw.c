
/*	$Id: tixWinDraw.c,v 1.1.1.1 2000/05/17 11:08:55 idiscovery Exp $	*/

/*
 * tixWinDraw.c --
 *
 *	Implement the Windows specific function calls for drawing.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include <tkInt.h>
#ifdef __PM__
#    define __PM_WIN__
#    include <tkOS2Int.h>
#else
#    include <tkWinInt.h>
#endif
#include "tixInt.h"
#include "tixPort.h"



/*----------------------------------------------------------------------
 * TixpDrawTmpLine --
 *
 *	Draws a "temporarily" line on the desktop window with XOR
 *	drawing mode. This function is used by the PanedWindow and
 *	ResizeHandler to draw the rubberband lines. Calling the
 *	function again with the same parameters cancels the temporary
 *	lines without affecting what was originally on the screen.
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
#ifdef __PM__
    panic("Not implemented: TixpDrawTmpLine");
#else
    HWND desktop;
    HDC hdc;
    HPEN hpen;
    HGDIOBJ old;

    desktop = GetDesktopWindow();
    hdc = GetWindowDC(desktop);
    hpen = CreatePen(PS_SOLID, 0, RGB(255,255,255));

    old = SelectObject(hdc, hpen);
    SetROP2(hdc, R2_XORPEN);

    MoveToEx(hdc, x1, y1, NULL);
    LineTo(hdc, x2, y2);

    SelectObject(hdc, old);
    DeleteObject(hpen);
    ReleaseDC(desktop, hdc);
#endif
}

/*----------------------------------------------------------------------
 * TixpDrawAnchorLines --
 *
 *	See comments near Tix_DrawAnchorLines.
 *----------------------------------------------------------------------
 */

#ifndef __PM__
void
TixpDrawAnchorLines(display, drawable, gc, x, y, w, h)
    Display *display;
    Drawable drawable;
    GC gc;
    int x;
    int y;
    int w;
    int h;
{
    HDC hdc;
    TkWinDCState state;
    HPEN hpen;
    HGDIOBJ old;

    hdc = TkWinGetDrawableDC(display, drawable, &state);
    hpen = CreatePen(PS_DOT, 1, gc->foreground);

    old = SelectObject(hdc, hpen);
    MoveToEx(hdc, x, y, NULL);
    LineTo(hdc, x,     y+h-1);
    LineTo(hdc, x+w-1, y+h-1);
    LineTo(hdc, x+w-1, y);
    LineTo(hdc, x,     y);

    SelectObject(hdc, old);
    DeleteObject(hpen);

    TkWinReleaseDrawableDC(drawable, hdc, &state);
}
#endif


