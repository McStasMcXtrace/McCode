/*
 * tkWin.h --
 *
 *	Declarations of public types and interfaces that are only
 *	available under Windows.
 *
 * Copyright (c) 1996-1997 by Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * RCS: @(#) $Id: tkWin.h,v 1.6 1999/04/16 01:51:48 stanton Exp $
 */

#ifndef _TKWIN
#define _TKWIN

#ifndef _TK
#include <tk.h>
#endif

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN

#ifdef BUILD_tk
# undef TCL_STORAGE_CLASS
# define TCL_STORAGE_CLASS DLLEXPORT
#endif

/*
 * The following messages are use to communicate between a Tk toplevel
 * and its container window.
 */

#define TK_CLAIMFOCUS	(WM_USER)
#define TK_GEOMETRYREQ	(WM_USER+1)
#define TK_ATTACHWINDOW	(WM_USER+2)
#define TK_DETACHWINDOW	(WM_USER+3)


typedef void Tcl_HandleProc (ClientData, HANDLE);


/*
 *--------------------------------------------------------------
 *
 * Exported procedures defined for the Windows platform only.
 *
 *--------------------------------------------------------------
 */

#include "tkPlatDecls.h"

extern HINSTANCE TclWinGetTclInstance _ANSI_ARGS_((void));

# undef TCL_STORAGE_CLASS
# define TCL_STORAGE_CLASS DLLIMPORT

#endif /* _TKWIN */
