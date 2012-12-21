
/*	$Id: tixUnixWm.c,v 1.1.1.1 2000/05/17 11:08:54 idiscovery Exp $	*/

/*
 * tixUnixWm.c --
 *
 *	Implement the Windows specific function calls for window management.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include "tixPort.h"
#include "tixUnixInt.h"

int
TixpSetWindowParent(interp, tkwin, newParent, parentId)
    Tcl_Interp * interp;
    Tk_Window tkwin;
    Tk_Window newParent;
    int parentId;
{
    return TCL_OK;
}
