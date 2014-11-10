/* $Id: tixAppInit.c,v 1.1.2.1 2001/11/15 05:18:16 idiscovery Exp $ */
/* 
 * tixAppInit.c --
 *
 *	Provides a default version of the Tcl_AppInit procedure for
 *	use in wish and similar Tk-based applications.
 *
 * Copyright (c) 1995 Ioi K Lam
 * Copyright (c) 1993 The Regents of the University of California.
 * Copyright (c) 1994 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#ifndef lint
static char sccsid[] = "@(#) tkAppInit.c 1.15 95/06/28 13:14:28";
#endif /* not lint */

#include "tk.h"
#include "itcl.h"
#include "itk.h"
#include <tix.h>

/*
 * The following variable is a special hack that is needed in order for
 * Sun shared libraries to be used for Tcl.
 */

extern int matherr();
int *tclDummyMathPtr = (int *) matherr;

/*
 *----------------------------------------------------------------------
 *
 * main --
 *
 *	This is the main program for the application.
 *
 * Results:
 *	None: Tk_Main never returns here, so this procedure never
 *	returns either.
 *
 * Side effects:
 *	Whatever the application does.
 *
 *----------------------------------------------------------------------
 */

int
main(argc, argv)
    int argc;			/* Number of command-line arguments. */
    char **argv;		/* Values of command-line arguments. */
{
    Tk_Main(argc, argv, Tcl_AppInit);
    return 0;			/* Needed only to prevent compiler warning. */
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
    if (Tcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    if (Tk_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Tk",  Tk_Init,  (Tcl_PackageInitProc *) NULL);

    /*
     * Call the init procedures for included packages.  Each call should
     * look like this:
     *
     * if (Mod_Init(interp) == TCL_ERROR) {
     *     return TCL_ERROR;
     * }
     *
     * where "Mod" is the name of the module.
     */
    if (Itcl_Init(interp) == TCL_ERROR) {
        return TCL_ERROR;
    }
    if (Itk_Init(interp) == TCL_ERROR) {
        return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Itcl", Itcl_Init, Itcl_SafeInit);
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

    /*
     * Call Tcl_CreateCommand for application-specific commands, if
     * they weren't already created by the init procedures called above.
     */
    if (Tix_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Tix", Tix_Init, (Tcl_PackageInitProc *) NULL);


    /*
     Tix_SetRcFileName(interp, "~/.tixwishrc");
     In the past, the interactive initialization file was inconsistent,
     and on Windows, $env(HOME) is undefined or most users don't even
     know where there HOME is (Profiles\User\Application Data\)!
     So a site wide initialization file tixwishrc.tcl is now used,
     which must be in the same directory as the executable. To restore
     the past behaviour, simply add the following line to that file:
	 if {[file isfile [set file ~/.tixwishrc]]} {source $file}
     */

    return TCL_OK;
}
