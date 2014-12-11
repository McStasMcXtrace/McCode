
/*	$Id: tixUnixSam.c,v 1.2 2000/11/22 07:55:24 idiscovery Exp $	*/

/*
 * tixUnixSam.c --
 *
 *	Initializes embedded Tix for Tix version 8.1.
 *
 */

#include "tixPort.h"
#include "tixInt.h"

#include "tixSamLib.c"

int		SamTix_Init _ANSI_ARGS_((Tcl_Interp *interp));

int
Tixsam_Init(interp)
    Tcl_Interp *interp;		/* Interpreter to initialize. */
{
    Tcl_Interp * Et_Interp = interp;

    if (TixInitSam(interp) != TCL_OK ){
	return TCL_ERROR;
    }
    if (LoadScripts(interp) != TCL_OK ){
	return TCL_ERROR;
    }
    if (Tcl_GlobalEval(interp, "__tixInit") != TCL_OK) {
	return TCL_ERROR;
    }

    return TCL_OK;
}

int
Tixsam_SafeInit(interp)
    Tcl_Interp *interp;		/* Interpreter to initialize. */
{
    return Tixsam_Init(interp);
}
