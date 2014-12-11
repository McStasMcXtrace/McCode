
/*	$Id: tixItcl.h,v 1.1.1.1 2000/05/17 11:08:42 idiscovery Exp $	*/

/*
 * tixItcl.h --
 *
 *	Compatibility functions and macros that allow Tix to work
 *	under Incr Tcl.
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

#include "tix.h"

#ifndef _LANG
#include "Lang.h"
#endif

/*
 * Structure to store Tcl 8.0 name space information.
 */

typedef struct _TixItclNameSp {
    Tcl_Interp *iPtr;
    CallFrame *savedVarFramePtr;
} TixItclNameSp;

#define DECLARE_ITCL_NAMESP(x,i) \
    TixItclNameSp x; \
    x.iPtr = (Interp*)(i);

extern int		TixItclSetGlobalNameSp _ANSI_ARGS_((
			    TixItclNameSp * nameSpPtr, Tcl_Interp * interp));
extern void		TixItclRestoreGlobalNameSp _ANSI_ARGS_((
			    TixItclNameSp * nameSpPtr, Tcl_Interp * interp));
