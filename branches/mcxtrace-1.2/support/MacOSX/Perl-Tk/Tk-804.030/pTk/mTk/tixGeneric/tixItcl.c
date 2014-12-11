
/*	$Id: tixItcl.c,v 1.1.1.1 2000/05/17 11:08:42 idiscovery Exp $	*/

/*
 * tixItcl.c --
 *
 *	Compatibility functions that allow Tix to work under Incr Tcl.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 *
 */

/*
 * With Tcl 8.0, namespaces moved from Itcl to Tcl, and so
 * the Tix hacks have to be used in any verison of 8.0,
 * regardless of the presence of Itcl...
 */
#include "Lang.h"
#include "tixInt.h"
#include "tixItcl.h"

/*----------------------------------------------------------------------
 * TixItclSetGlobalNameSp --
 *
 *	Set the ITcl scope to the global scope. This way, all the Tix
 *	commands and variables will be defined in the global scope. This
 *	is necessary for Tix to function properly under ITcl.
 *
 *----------------------------------------------------------------------
 */
int
TixItclSetGlobalNameSp(nameSpPtr, interp)
    TixItclNameSp * nameSpPtr;
    Tcl_Interp * interp;
{
    Interp *p = nameSpPtr->iPtr;
    nameSpPtr->savedVarFramePtr = p->varFramePtr;
    if ( NULL == p->varFramePtr
            || p->varFramePtr->nsPtr == p->globalNsPtr ) {
        return 1;
    }
    p->varFramePtr = NULL;
    return 1;
}

/*----------------------------------------------------------------------
 * TixItclRestoreGlobalNameSp --
 *
 *	Set the ITcl scope to the scope saved by TixItclSetGlobalNameSp.
 *
 *----------------------------------------------------------------------
 */
void
TixItclRestoreGlobalNameSp(nameSpPtr, interp)
    TixItclNameSp * nameSpPtr;
    Tcl_Interp * interp;
{
    nameSpPtr->iPtr->varFramePtr = nameSpPtr->savedVarFramePtr;
}
