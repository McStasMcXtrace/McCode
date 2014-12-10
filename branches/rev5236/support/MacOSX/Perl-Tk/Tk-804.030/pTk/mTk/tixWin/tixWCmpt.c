
/*	$Id: tixWCmpt.c,v 1.1.1.1 2000/05/17 11:08:55 idiscovery Exp $	*/

/*
 * tixWCmpt.c --
 *
 *	Windows compatibility module: implements missing functions in Windows.
 */

#include <tkWinInt.h>
#include "tixPort.h"
#include "tixInt.h"

#ifndef strcasecmp
int strcasecmp(char * a, char *b)
{
    while (1) {
	if (*a== 0 && *b==0) {
	    return 0;
	}
	if (*a==0) {
	    return (1);
	}
	if (*b==0) {
	    return (-1);
	}
	if (tolower(*a)>tolower(*b)) {
	    return (-1);
	}
	if (tolower(*b)>tolower(*a)) {
	    return (1);
	}
	a++; b++;
    }
}
#endif

/*
 *----------------------------------------------------------------------
 *
 * XLowerWindow --
 *
 *	Change the stacking order of a window.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Changes the stacking order of the specified window.
 *
 *----------------------------------------------------------------------
 */
void
XLowerWindow(display, w)
    Display* display;
    Window w;
{
    HWND window = TkWinGetHWND(w);

    display->request++;
    SetWindowPos(window, HWND_TOPMOST, 0, 0, 0, 0,
	    SWP_NOMOVE | SWP_NOSIZE);
}



#if 1
void XDrawPoints(display, d, gc, points, npoints, mode)
    Display*		display;
    Drawable		d;
    GC			gc;
    XPoint*		points;
    int			npoints;
    int			mode;
{
    int i;

    for (i=0; i<npoints; i++) {
	XDrawLine(display, d, gc, points[i].x, points[i].y,
	    points[i].x, points[i].y);
    }
}

#endif

#if 1

/*
 * The following declaration is for the VC++ DLL entry point.
 */

BOOL APIENTRY		DllMain _ANSI_ARGS_((HINSTANCE hInst,
			    DWORD reason, LPVOID reserved));

/*
 *----------------------------------------------------------------------
 *
 * DllEntryPoint --
 *
 *	This wrapper function is used by Borland to invoke the
 *	initialization code for Tk.  It simply calls the DllMain
 *	routine.
 *
 * Results:
 *	See DllMain.
 *
 * Side effects:
 *	See DllMain.
 *
 *----------------------------------------------------------------------
 */

BOOL APIENTRY
DllEntryPoint(hInst, reason, reserved)
    HINSTANCE hInst;		/* Library instance handle. */
    DWORD reason;		/* Reason this function is being called. */
    LPVOID reserved;		/* Not used. */
{
    return DllMain(hInst, reason, reserved);
}

/*
 *----------------------------------------------------------------------
 *
 * DllMain --
 *
 *	DLL entry point.
 *
 * Results:
 *	TRUE on sucess, FALSE on failure.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

BOOL APIENTRY
DllMain(hInstance, reason, reserved)
    HINSTANCE hInstance;
    DWORD reason;
    LPVOID reserved;
{
    /*
     * If we are attaching to the DLL from a new process, tell Tk about
     * the hInstance to use. If we are detaching then clean up any
     * data structures related to this DLL.
     */

    return(TRUE);
}
#else

#define DllExport	__declspec( dllexport )

DllExport
DllEntryPoint(hInst, reason, reserved)
    HINSTANCE hInst;		/* Library instance handle. */
    DWORD reason;		/* Reason this function is being called. */
    LPVOID reserved;		/* Not used. */
{
    return TRUE;
}

#endif

int TixPlatformInit(Tcl_Interp * interp)
{
    return Tcl_GlobalEval(interp, "set tix(isWindows) 1");
}
