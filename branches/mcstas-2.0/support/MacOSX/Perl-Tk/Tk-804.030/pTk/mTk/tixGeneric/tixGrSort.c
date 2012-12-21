
/*	$Id: tixGrSort.c,v 1.1.1.1 2000/05/17 11:08:38 idiscovery Exp $	*/

/*
 * tixGrSel.c --
 *
 *	This module handles the sorting of the Grid widget.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include "tixPort.h"
#include "tixInt.h"
#include "tixGrid.h"

/*
 * The variables below are used to implement the "lsort" command.
 * Unfortunately, this use of static variables prevents "lsort"
 * from being thread-safe, but there's no alternative given the
 * current implementation of qsort.  In a threaded environment
 * these variables should be made thread-local if possible, or else
 * "lsort" needs internal mutual exclusion.
 */

static Tcl_Interp *sortInterp = NULL;	/* Interpreter for "lsort" command.
					 * NULL means no lsort is active. */
static enum {ASCII, INTEGER, REAL, COMMAND} sortMode;
					/* Mode for sorting:compare as strings,
					 * compare as numbers, or call
					 * user-defined command for
					 * comparison. */
#ifndef _LANG
static Tcl_DString sortCmd;		/* Holds command if mode is COMMAND.
					 * pre-initialized to hold base of
					 * command. */
#endif
static int sortIncreasing;		/* 0 means sort in decreasing order,
					 * 1 means increasing order. */
static int sortCode;			/* Anything other than TCL_OK means a
					 * problem occurred while sorting; this
					 * executing a comparison command, so
					 * the sort was aborted. */

/*
 * Forward declarations for procedures defined in this file:
 */

EXTERN TIX_DECLARE_SUBCMD(Tix_GrSort);

static int		SortCompareProc _ANSI_ARGS_((CONST VOID *first,
			    CONST VOID *second));
Tcl_Obj *			Tix_GrGetCellText _ANSI_ARGS_((WidgetPtr wPtr,
			    int x, int y));
Tix_GrSortItem *	Tix_GrGetSortItems _ANSI_ARGS_((WidgetPtr wPtr,
			    int axis, int start, int end, int sortKeyIndex));
void			Tix_GrFreeSortItems _ANSI_ARGS_((WidgetPtr wPtr,
			    Tix_GrSortItem * items, int numItems));


/*
 *----------------------------------------------------------------------
 *
 * Tcl_LsortCmd --
 *
 *	This procedure is invoked to process the "lsort" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */

Tcl_Obj *
Tix_GrGetCellText(wPtr, x, y)
    WidgetPtr wPtr;
    int x;
    int y;
{
    TixGrEntry* chPtr;

    if ((chPtr = (TixGrEntry*) TixGridDataFindEntry(wPtr->dataSet, x, y))) {
	switch (Tix_DItemType(chPtr->iPtr)) {
	  case TIX_DITEM_TEXT:
	    return chPtr->iPtr->text.text;
	  case TIX_DITEM_IMAGETEXT:
	    return chPtr->iPtr->imagetext.text;
	  default:
	    return NULL;
	}
    } else {
	return NULL;
    }
}

Tix_GrSortItem *
Tix_GrGetSortItems(wPtr, axis, start, end, sortKeyIndex)
    WidgetPtr wPtr;
    int axis;
    int start;
    int end;
    int sortKeyIndex;
{
    int i, k, numItems;
    Tix_GrSortItem *items;

    if (end <= start) {
	/* sanity check: no need to sort */
	return (Tix_GrSortItem *) NULL;
    }

    numItems = end-start+1;
    items = (Tix_GrSortItem *)ckalloc(sizeof(Tix_GrSortItem) * numItems);

    for (k=0,i=start; i<=end; i++, k++) {
	items[k].index = i;
	if (axis == 0) {
	    items[k].data = Tix_GrGetCellText(wPtr, i, sortKeyIndex);
	} else {
	    items[k].data = Tix_GrGetCellText(wPtr, sortKeyIndex, i);
	}
    }

    return items;
}


void
Tix_GrFreeSortItems(wPtr, items, numItems)
    WidgetPtr wPtr;
    Tix_GrSortItem * items;
    int numItems;
{
    ckfree((char*)items);
}

int
Tix_GrSort(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    WidgetPtr wPtr = (WidgetPtr) clientData;
    int i, axis, otherAxis, start, end;
    size_t len;
    Tix_GrSortItem *items = NULL;
    int numItems;
    LangCallback *command = NULL;		/* Initialization needed only to
					 * prevent compiler warning. */
    int sortKeyIndex;
    int gridSize[2];

    /*-------------------------------------------------------------------
     *			Argument parsing
     *-------------------------------------------------------------------
     */
    if (sortInterp != NULL) {
	interp->result = "can't invoke the tixGrid sort command recursively";
	return TCL_ERROR;
    }

    /* Figure out the sorting dimension
     */
    len = strlen(argv[0]);
    if (strncmp(argv[0], "rows", len)==0) {
	axis = 1;
	otherAxis = 0;
    } else if (strncmp(argv[0], "column", len)==0) {
	axis = 0;
	otherAxis = 1;
    } else {
	Tcl_AppendResult(interp, "wrong dimension \"", argv[0],
	    "\", should be row or column",  (char *) NULL);
	return TCL_ERROR;
    }

    /* get the start and end index
     */
    if (axis == 0) {
	if (TixGridDataGetIndex(interp, wPtr, objv[1], NULL, &start, NULL)
		!=TCL_OK) {
	    return TCL_ERROR;
	}
	if (TixGridDataGetIndex(interp, wPtr, objv[2], NULL, &end, NULL)
		!=TCL_OK) {
	    return TCL_ERROR;
	}
    } else {
	if (TixGridDataGetIndex(interp, wPtr, NULL, objv[1], NULL, &start)
		!=TCL_OK) {
	    return TCL_ERROR;
	}
	if (TixGridDataGetIndex(interp, wPtr, NULL, objv[2], NULL, &end)
		!=TCL_OK) {
	    return TCL_ERROR;
	}
    }

    /* Check the range
     */
    TixGridDataGetGridSize(wPtr->dataSet, &gridSize[0], &gridSize[1]);
    if (start > end) {
	int tmp = start;
	start = end;
	end = tmp;
    }
    if (start >= gridSize[axis]) {
	/* no need to sort */
	return TCL_OK;
    }
    if (end == start) {
	/* no need to sort */
	return TCL_OK;
    }

    /* get the options
     */
    if ((argc-3) %2 != 0) {
	Tcl_AppendResult(interp, "value for \"", argv[argc-1],
		"\" missing", NULL);
	return TCL_ERROR;
    }
    sortInterp = interp;
    sortMode = ASCII;
    sortIncreasing = 1;
    sortCode = TCL_OK;
    sortKeyIndex = wPtr->hdrSize[otherAxis];	/* by default, use the first
						 * scrollable item as the key
						 */
    for (i=3; i<argc; i+=2) {
	len = strlen(argv[i]);
	if (strncmp(argv[i], "-type", len) == 0) {
	    if (strcmp(argv[i+1], "ascii") == 0) {
		sortMode = ASCII;
	    } else if (strcmp(argv[i+1], "integer") == 0) {
		sortMode = INTEGER;
	    } else if (strcmp(argv[i+1], "real") == 0) {
		sortMode = REAL;
	    } else {
		Tcl_AppendResult(interp, "wrong type \"", argv[i+1],
		    "\": must be ascii, integer or real", (char *) NULL);
		sortCode = TCL_ERROR;
		goto done;
	    }
	}
	else if (strncmp(argv[i], "-order", len) == 0) {
	    if (strcmp(argv[i+1], "increasing") == 0) {
		sortIncreasing = 1;
	    } else if (strcmp(argv[i+1], "decreasing") == 0) {
	        sortIncreasing = 0;
	    } else {
	        Tcl_AppendResult(interp, "wrong order \"", argv[i+1],
		    "\": must be increasing or decreasing", (char *) NULL);
		sortCode = TCL_ERROR;
		goto done;
	    }
	}
	else if (strncmp(argv[i], "-key", len) == 0) {
	    if (axis == 0) {
		/* sort columns: the key is a column index (1) */
		if (TixGridDataGetIndex(interp, wPtr, NULL, objv[i+1], NULL,
			&sortKeyIndex) !=TCL_OK) {
		    sortCode = TCL_ERROR;
		    goto done;
		}
	    } else {
		/* sort rows: the key is a row index (0)*/
		if (TixGridDataGetIndex(interp, wPtr, objv[i+1], NULL,
			&sortKeyIndex, NULL) !=TCL_OK) {
		    sortCode = TCL_ERROR;
		    goto done;
		}
	    }
	}
	else if (strncmp(argv[i], "-command", len) == 0) {
	    sortMode = COMMAND;
	    command = LangMakeCallback(objv[i+1]);
	}
	else {
	    Tcl_AppendResult(interp, "wrong option \"", argv[i],
		"\": must be -command, -key, -order or -type", (char *) NULL);
	    sortCode = TCL_ERROR;
	    goto done;
	}
    }
    if (sortMode == COMMAND) {
#ifndef _LANG
	Tcl_DStringInit(&sortCmd);
	Tcl_DStringAppend(&sortCmd, command, -1);
#endif
    }

    /*------------------------------------------------------------------
     *		SORTING
     *------------------------------------------------------------------
     */
    /* prepare the array to be sorted */
    numItems = end - start + 1;
    items = Tix_GrGetSortItems(wPtr, axis, start, end, sortKeyIndex);

    if (items != NULL) {
	int sizeChanged;

	qsort((VOID *)items, (size_t)numItems, sizeof(Tix_GrSortItem),
	    SortCompareProc);
	sizeChanged = TixGridDataUpdateSort(wPtr->dataSet, axis, start, end,
	    items);
	if (sizeChanged) {
	    Tix_GrDoWhenIdle(wPtr, TIX_GR_RESIZE);
	} else {
	    wPtr->toResetRB = 1;
	    Tix_GrDoWhenIdle(wPtr, TIX_GR_REDRAW);
	}

	Tix_GrFreeSortItems(wPtr, items, numItems);
    }

    /* Done */
    if (sortCode == TCL_OK) {
	Tcl_ResetResult(interp);
    }
    if (sortMode == COMMAND) {
#ifdef _LANG
	LangFreeCallback(command);
#else
	Tcl_DStringFree(&sortCmd);
#endif
    }

  done:
    sortInterp = NULL;
    return sortCode;
}

/*
 *----------------------------------------------------------------------
 *
 * SortCompareProc --
 *
 *	This procedure is invoked by qsort to determine the proper
 *	ordering between two elements.
 *
 * Results:
 *	< 0 means first is "smaller" than "second", > 0 means "first"
 *	is larger than "second", and 0 means they should be treated
 *	as equal.
 *
 * Side effects:
 *	None, unless a user-defined comparison command does something
 *	weird.
 *
 *----------------------------------------------------------------------
 */

static int
SortCompareProc(first, second)
    CONST VOID *first, *second;		/* Elements to be compared. */
{
    int order;
    Tcl_Obj * firstString  = ((Tix_GrSortItem*)first )->data;
    Tcl_Obj * secondString = ((Tix_GrSortItem*)second)->data;

    order = 0;
    if (sortCode != TCL_OK) {
	/*
	 * Once an error has occurred, skip any future comparisons
	 * so as to preserve the error message in sortInterp->result.
	 */

	return order;
    }
    if (firstString == NULL && secondString == NULL) {
	/* equal */
	return order;
    }
    if (secondString == NULL) {
	/* first larger than second */
	order = 1;
	goto done;
    }
    if (firstString == NULL) {
	order = -1;
	goto done;
    }

    if (sortMode == ASCII) {
	order = strcmp(Tcl_GetString(firstString), Tcl_GetString(secondString));
    } else if (sortMode == INTEGER) {
	int a, b;

	if ((Tcl_GetIntFromObj(sortInterp, firstString, &a) != TCL_OK)
		|| (Tcl_GetIntFromObj(sortInterp, secondString, &b) != TCL_OK)) {
	    Tcl_AddErrorInfo(sortInterp,
		    "\n    (converting list element from string to integer)");
	    sortCode = TCL_ERROR;
	    return order;
	}
	if (a > b) {
	    order = 1;
	} else if (b > a) {
	    order = -1;
	}
    } else if (sortMode == REAL) {
	double a, b;

	if ((Tcl_GetDoubleFromObj(sortInterp, firstString, &a) != TCL_OK)
		|| (Tcl_GetDoubleFromObj(sortInterp, secondString, &b) != TCL_OK)) {
	    Tcl_AddErrorInfo(sortInterp,
		    "\n    (converting list element from string to real)");
	    sortCode = TCL_ERROR;
	    return order;
	}
	if (a > b) {
	    order = 1;
	} else if (b > a) {
	    order = -1;
	}
    } else {
#ifdef _LANG
	/* FIXME */
	Tcl_Panic("Need Callback Handling Added");
	return 0;
#else
	int oldLength;
	char *end;

	/*
	 * Generate and evaluate a command to determine which string comes
	 * first.
	 */

	oldLength = Tcl_DStringLength(&sortCmd);
	Tcl_DStringAppendElement(&sortCmd, firstString);
	Tcl_DStringAppendElement(&sortCmd, secondString);
	sortCode = Tcl_Eval(sortInterp, Tcl_DStringValue(&sortCmd));
	Tcl_DStringTrunc(&sortCmd, oldLength);
	if (sortCode != TCL_OK) {
	    Tcl_AddErrorInfo(sortInterp,
		    "\n    (user-defined comparison command)");
	    return order;
	}

	/*
	 * Parse the result of the command.
	 */

	order = strtol(sortInterp->result, &end, 0);
	if ((end == sortInterp->result) || (*end != 0)) {
	    Tcl_ResetResult(sortInterp);
	    Tcl_AppendResult(sortInterp,
		    "comparison command returned non-numeric result",
		    (char *) NULL);
	    sortCode = TCL_ERROR;
	    return order;
	}
#endif
    }

done:
    if (!sortIncreasing) {
	order = -order;
    }
    return order;
}
