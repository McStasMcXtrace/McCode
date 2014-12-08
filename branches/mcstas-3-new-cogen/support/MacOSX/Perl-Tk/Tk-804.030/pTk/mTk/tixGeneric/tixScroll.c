
/*	$Id: tixScroll.c,v 1.1.1.1 2000/05/17 11:08:42 idiscovery Exp $	*/

/*
 * tixScroll.c -- Handle all the mess of Tk scroll bars
 *
 *
 *
 */

#include "tixPort.h"
#include "tixInt.h"


void Tix_InitScrollInfo(siPtr, type)
    Tix_ScrollInfo * siPtr;
    int type;
{

    siPtr->command 	= NULL;
    siPtr->type 	= type;

    if (type == TIX_SCROLL_INT) {
	Tix_IntScrollInfo*    isiPtr = (Tix_IntScrollInfo*)   siPtr;
	isiPtr->total	= 1;
	isiPtr->window	= 1;
	isiPtr->offset	= 0;
	isiPtr->unit	= 1;
    }
    else {
	Tix_DoubleScrollInfo* dsiPtr = (Tix_DoubleScrollInfo*)siPtr;
	dsiPtr->total	= 1.0;
	dsiPtr->window	= 1.0;
	dsiPtr->offset	= 0.0;
	dsiPtr->unit	= 1.0;
    }
}

/*----------------------------------------------------------------------
 * Tix_GetScrollFractions --
 *
 * Compute the fractions of a scroll-able widget.
 *
 */
void Tix_GetScrollFractions(siPtr, first_ret, last_ret)
    Tix_ScrollInfo * siPtr;
    double * first_ret;
    double * last_ret;
{
    double total, window, first;

    if (siPtr->type == TIX_SCROLL_INT) {
	Tix_IntScrollInfo*    isiPtr = (Tix_IntScrollInfo*)   siPtr;
	total  = isiPtr->total;
	window = isiPtr->window;
	first  = isiPtr->offset;
    } else {
	Tix_DoubleScrollInfo* dsiPtr = (Tix_DoubleScrollInfo*)siPtr;
	total  = dsiPtr->total;
	window = dsiPtr->window;
	first  = dsiPtr->offset;
    }

    if (total == 0 || total < window) {
	*first_ret = 0.0;
	*last_ret  = 1.0;
    } else {
	*first_ret = first / total;
	*last_ret  = (first+window) / total;
    }
}

void Tix_UpdateScrollBar(interp, siPtr)
    Tcl_Interp *interp;
    Tix_ScrollInfo * siPtr;
{
    double d_first, d_last;

    if (siPtr->type == TIX_SCROLL_INT) {
	Tix_IntScrollInfo*    isiPtr = (Tix_IntScrollInfo*)   siPtr;
	/* Check whether the topPixel is out of bound */
	if (isiPtr->offset < 0) {
	    isiPtr->offset = 0;
	} else {
	    if (isiPtr->window > isiPtr->total) {
		isiPtr->offset = 0;
	    }
	    else if((isiPtr->offset+isiPtr->window) > isiPtr->total) {
		isiPtr->offset = isiPtr->total - isiPtr->window;
	    }
	}
    } else {
	Tix_DoubleScrollInfo* dsiPtr = (Tix_DoubleScrollInfo*)siPtr;
	/* Check whether the topPixel is out of bound */
	if (dsiPtr->offset < 0) {
	    dsiPtr->offset = 0;
	} else {
	    if (dsiPtr->window > dsiPtr->total) {
		dsiPtr->offset = 0;
	    }
	    else if((dsiPtr->offset+dsiPtr->window) > dsiPtr->total) {
		dsiPtr->offset = dsiPtr->total - dsiPtr->window;
	    }
	}
    }


    if (siPtr->command) {
	Tix_GetScrollFractions(siPtr, &d_first, &d_last);

	if (LangDoCallback(interp, siPtr->command, 0, 2 , "%g %g", d_first, d_last)
	    != TCL_OK) {
	    Tcl_AddErrorInfo(interp,
		"\n    (scrolling command executed by tixTList)");
	    Tcl_BackgroundError(interp);
	}
    }
}

int Tix_SetScrollBarView(interp, siPtr, argc, argv, compat)
    Tcl_Interp *interp;		/* Current interpreter. */
    Tix_ScrollInfo * siPtr;
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
    int compat;			/* compatibility mode */
{
    int offset;

    if (compat && Tcl_GetIntFromObj(interp, argv[0], &offset) == TCL_OK) {
	/* backward-compatible mode */
	if (siPtr->type == TIX_SCROLL_INT) {
	    Tix_IntScrollInfo*    isiPtr = (Tix_IntScrollInfo*)   siPtr;
	    isiPtr->offset = offset;
	}
	else {
	    Tix_DoubleScrollInfo* dsiPtr = (Tix_DoubleScrollInfo*)siPtr;
	    dsiPtr->offset = (double)offset;
	}

	return TCL_OK;
    }
    else {
	int type, count;
	double fraction;

	Tcl_ResetResult(interp);

	/* Tk_GetScrollInfo () wants strange argc,argv combinations .. */
	type = Tk_GetScrollInfo(interp, argc+2, argv-2, &fraction, &count);

	if (siPtr->type == TIX_SCROLL_INT) {
	    Tix_IntScrollInfo*    isiPtr = (Tix_IntScrollInfo*)   siPtr;
	    switch (type) {
	      case TK_SCROLL_ERROR:
		return TCL_ERROR;

	      case TK_SCROLL_MOVETO:
		isiPtr->offset =
		  (int)(fraction * (double)isiPtr->total);
		break;

	      case TK_SCROLL_PAGES:
		isiPtr->offset += count * isiPtr->window;
		break;

	      case TK_SCROLL_UNITS:
		isiPtr->offset += count * isiPtr->unit;
		break;
	    }
	} else {
	    Tix_DoubleScrollInfo* dsiPtr = (Tix_DoubleScrollInfo*)siPtr;
	    switch (type) {
	      case TK_SCROLL_ERROR:
		return TCL_ERROR;

	      case TK_SCROLL_MOVETO:
		dsiPtr->offset =
		  fraction * dsiPtr->total;
		break;

	      case TK_SCROLL_PAGES:
		dsiPtr->offset += count * dsiPtr->window;
		break;

	      case TK_SCROLL_UNITS:
		dsiPtr->offset += count * dsiPtr->unit;
		break;
	    }
	}
    }
    return TCL_OK;
}
