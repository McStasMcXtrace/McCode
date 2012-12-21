/*	$Id: tixWinMain.c,v 1.2.2.1 2001/11/04 05:23:02 idiscovery Exp $	*/

/*
 * tixWinMain.c --
 *
 *	Main entry point for wish and other Tk-based applications.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *
 */

#include <tk.h>
#include "tixInt.h"
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
#include <malloc.h>
#include <locale.h>

#ifdef ITCL_2
#include "itcl.h"
#include "itk.h"
#endif

/*
 * The following declarations refer to internal Tk routines.  These
 * interfaces are available for use, but are not supported.
 */
void	TkConsoleCreate _ANSI_ARGS_((void));
int	TkConsoleInit _ANSI_ARGS_((Tcl_Interp *interp));
void	TkConsolePrint _ANSI_ARGS_((Tcl_Interp *interp,
			    int devId, char *buffer, long size));

/*
 * Forward declarations for procedures defined later in this file:
 */

static void		WishPanic _ANSI_ARGS_(TCL_VARARGS(char *,format));


/*
 *----------------------------------------------------------------------
 *
 * WinMain --
 *
 *	Main entry point from Windows.
 *
 * Results:
 *	Returns false if initialization fails, otherwise it never
 *	returns.
 *
 * Side effects:
 *	Just about anything, since from here we call arbitrary Tcl code.
 *
 *----------------------------------------------------------------------
 */

int APIENTRY
WinMain(hInstance, hPrevInstance, lpszCmdLine, nCmdShow)
    HINSTANCE hInstance;
    HINSTANCE hPrevInstance;
    LPSTR lpszCmdLine;
    int nCmdShow;
{
    char **argv, **argvlist, *p;
    int argc, size, i;
    char buffer[MAX_PATH];

    Tcl_SetPanicProc(WishPanic);

    /*
     * Increase the application queue size from default value of 8.
     * At the default value, cross application SendMessage of WM_KILLFOCUS
     * will fail because the handler will not be able to do a PostMessage!
     * This is only needed for Windows 3.x, since NT dynamically expands
     * the queue.
     */
    SetMessageQueue(64);

    /*
     * Precompute an overly pessimistic guess at the number of arguments
     * in the command line by counting non-space spans.  Note that we
     * have to allow room for the executable name and the trailing NULL
     * argument.
     */

    for (size = 3, p = lpszCmdLine; *p != '\0'; p++) {
	if (isspace(*p)) {
	    size++;
	    while (isspace(*p)) {
		p++;
	    }
	    if (*p == '\0') {
		break;
	    }
	}
    }
    argvlist = (char **) ckalloc((unsigned) (size * sizeof(char *)));
    argv = argvlist;

    /*
     * Parse the Windows command line string.  If an argument begins with a
     * double quote, then spaces are considered part of the argument until the
     * next double quote.  The argument terminates at the second quote.  Note
     * that this is different from the usual Unix semantics.
     */

    for (i = 1, p = lpszCmdLine; *p != '\0'; i++) {
	while (isspace(*p)) {
	    p++;
	}
	if (*p == '\0') {
	    break;
	}
	if (*p == '"') {
	    p++;
	    argv[i] = p;
	    while ((*p != '\0') && (*p != '"')) {
		p++;
	    }
	} else {
	    argv[i] = p;
	    while (*p != '\0' && !isspace(*p)) {
		p++;
	    }
	}
	if (*p != '\0') {
	    *p = '\0';
	    p++;
	}
    }
    argv[i] = NULL;
    argc = i;

    /*
     * Since Windows programs don't get passed the command name as the
     * first argument, we need to fetch it explicitly.
     */

    GetModuleFileName(NULL, buffer, sizeof(buffer));
    argv[0] = buffer;

    Tk_Main(argc, argv, Tcl_AppInit);
    return 1;
}


/*
 *----------------------------------------------------------------------
 *
 * Tcl_AppInit --
 *
 *	This procedure performs application-specific initialization.
 *	Most applications, especially those that incorporate additional
 *	packages, will have their own version of this procedure.
 *
 * Results:
 *	Returns a standard Tcl completion code, and leaves an error
 *	message in interp->result if an error occurs.
 *
 * Side effects:
 *	Depends on the startup script.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_AppInit(interp)
    Tcl_Interp *interp;		/* Interpreter for application. */
{
    /*
     * Set up the default locale to be standard "C" locale so parsing
     * is performed correctly.
     */
    setlocale(LC_ALL, "C");

    /*
     * Increase the application queue size from default value of 8.
     * At the default value, cross application SendMessage of WM_KILLFOCUS
     * will fail because the handler will not be able to do a PostMessage!
     * This is only needed for Windows 3.x, since NT dynamically expands
     * the queue.
     */
    SetMessageQueue(64);

    /*
     * Create the console channels and install them as the standard
     * channels.  All I/O will be discarded until TkConsoleInit is
     * called to attach the console to a text widget.
     */

    TkConsoleCreate();

    if (Tcl_Init(interp) == TCL_ERROR) {
	if (Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY)) {
	    MessageBox(NULL, Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY),
		"Tcl Init Error", MB_OK|MB_ICONSTOP);
	} else {
	    MessageBox(NULL, interp->result, "Tcl Init Error",
		MB_OK|MB_ICONSTOP );
	}
	return TCL_ERROR;
    }

    if (Tk_Init(interp) == TCL_ERROR) {
	if (Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY)) {
	    MessageBox(NULL, Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY),
		"Tk Init Error", MB_OK|MB_ICONSTOP);
	} else {
	    MessageBox(NULL, interp->result, "Tk Init Error",
		MB_OK|MB_ICONSTOP);
	}
	return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Tk", Tk_Init, (Tcl_PackageInitProc *) NULL);

#ifdef ITCL_2
    if (Itcl_Init(interp) == TCL_ERROR) {
	if (Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY)) {
	    MessageBox(NULL, Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY),
		"Itcl Init Error", MB_OK|MB_ICONSTOP);
	} else {
	    MessageBox(NULL, interp->result, "Itcl Init Error",
		MB_OK|MB_ICONSTOP);
	}
	return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Itcl", Itcl_Init, (Tcl_PackageInitProc *) NULL);

    if (Itk_Init(interp) == TCL_ERROR) {
	if (Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY)) {
	    MessageBox(NULL, Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY),
		"Itk Init Error", MB_OK|MB_ICONSTOP);
	} else {
	    MessageBox(NULL, interp->result, "Itk Init Error",
		MB_OK|MB_ICONSTOP);
	}
	return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Itk", Itk_Init, (Tcl_PackageInitProc *) NULL);


    /*
     *  This is itclsh, so import all [incr Tcl] commands by
     *  default into the global namespace.  Fix up the autoloader
     *  to do the same.
     */
    if (Tcl_Import(interp, Tcl_GetGlobalNamespace(interp),
            "::itcl::*", /* allowOverwrite */ 1) != TCL_OK) {
        return TCL_ERROR;
    }

    if (Tcl_Eval(interp, "auto_mkindex_parser::slavehook { _%@namespace import -force ::itcl::* }") != TCL_OK) {
        return TCL_ERROR;
    }

#endif

    if (Tix_Init(interp) == TCL_ERROR) {
	if (Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY)) {
	    MessageBox(NULL, Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY),
		"Tix Init Error", MB_OK|MB_ICONSTOP);
	} else {
	    MessageBox(NULL, interp->result, "Tix Init Error",
		MB_OK|MB_ICONSTOP);
	}
	return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Tix", Tix_Init, (Tcl_PackageInitProc *) NULL);

    /*
     * Initialize the console only if we are running as an interactive
     * application.
     */

    if (strcmp(Tcl_GetVar(interp, "tcl_interactive", TCL_GLOBAL_ONLY), "1")
	    == 0) {
	if (TkConsoleInit(interp) == TCL_ERROR) {
	    return TCL_ERROR;
	}
    }

    /* Now done in Tix.tcl */
    /* Tcl_SetVar(interp, "tcl_rcFileName", "~/wishrc.tcl", TCL_GLOBAL_ONLY); */
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * WishPanic --
 *
 *	Display a message and exit.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Exits the program.
 *
 *----------------------------------------------------------------------
 */

void
WishPanic TCL_VARARGS_DEF(char *,arg1)
{
    va_list argList;
    char buf[1024];
    char *format;

    format = TCL_VARARGS_START(char *,arg1,argList);
    vsprintf(buf, format, argList);

    MessageBeep(MB_ICONEXCLAMATION);
    MessageBox(NULL, buf, "Fatal Error in Wish",
	MB_ICONSTOP | MB_OK | MB_TASKMODAL | MB_SETFOREGROUND);
    ExitProcess(1);
}

