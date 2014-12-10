
/*	$Id: tixGrSel.c,v 1.1.1.1 2000/05/17 11:08:42 idiscovery Exp $	*/

/*
 * tixGrSel.c --
 *
 *	This module handles the selection
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include "tixPort.h"
#include "tixInt.h"
#include "tixDef.h"
#include "tixGrid.h"

EXTERN TIX_DECLARE_SUBCMD(Tix_GrSelection);
static TIX_DECLARE_SUBCMD(Tix_GrSelIncludes);
static TIX_DECLARE_SUBCMD(Tix_GrSelModify);

static int		Tix_GrSelIncludes _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
static void		Tix_GrAdjustSelection _ANSI_ARGS_((
			    WidgetPtr wPtr, SelectBlock * sbPtr));
static void		Tix_GrMergeSelection _ANSI_ARGS_((
			    WidgetPtr wPtr, SelectBlock * sbPtr));
static int 		Intersect _ANSI_ARGS_((SelectBlock * s1,
			    SelectBlock * s2));
static int 		Include _ANSI_ARGS_((SelectBlock * s1,
			    SelectBlock * s2));

int
Tix_GrSelection(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    static Tix_SubCmdInfo subCmdInfo[] = {
	{TIX_DEFAULT_LEN, "adjust",   2, 4, Tix_GrSelModify,
	   "x1 y1 ?x2 y2?"},
	{TIX_DEFAULT_LEN, "clear",    2, 4, Tix_GrSelModify,
	   "x1 y1 ?x2 y2?"},
	{TIX_DEFAULT_LEN, "includes", 2, 4, Tix_GrSelIncludes,
	   "x1 y1 ?x2 y2?"},
	{TIX_DEFAULT_LEN, "set",      2, 4, Tix_GrSelModify,
	   "x1 y1 ?x2 y2?"},
	{TIX_DEFAULT_LEN, "toggle",   2, 4, Tix_GrSelModify,
	   "x1 y1 ?x2 y2?"},
    };
    static Tix_CmdInfo cmdInfo = {
	Tix_ArraySize(subCmdInfo), 1, TIX_VAR_ARGS, "?option? ?arg ...?",
    };

    return Tix_HandleSubCmds(&cmdInfo, subCmdInfo, clientData,
	interp, argc+1, argv-1);
}

static int
Selected(WidgetPtr wPtr, int row, int col)
{
    int value = 0;
    Tix_ListIterator li;

    Tix_SimpleListIteratorInit(&li);

    for (Tix_SimpleListStart(&wPtr->selList, &li);
		 !Tix_SimpleListDone(&li);
		 Tix_SimpleListNext (&wPtr->selList, &li)) {
	SelectBlock *ptr = (SelectBlock *)li.curr;
        if (ptr->range[TIX_X][0] <= col && ptr->range[TIX_X][1] >= col &&
            ptr->range[TIX_Y][0] <= row && ptr->range[TIX_Y][1] >= row)
         {
          switch(ptr->type)
           {
            case TIX_GR_CLEAR:
             value = 0;
             break;
            case TIX_GR_SET:
             value = 1;
             break;
            case TIX_GR_TOGGLE:
             value = !value;
             break;
           }
	}
    }
 return value;
}

static int
Tix_GrSelIncludes(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    int row;
    int col;
    int value = 1;
    WidgetPtr wPtr = (WidgetPtr) clientData;
    if (argc != 2 && argc != 4) {
	return Tix_ArgcError(interp, argc+2, argv-2, 2, "x1 y1 ?x2 y2?");
    }
    if (Tcl_GetIntFromObj(interp, argv[0], &col) != TCL_OK) {
	return TCL_ERROR;
    }
    if (Tcl_GetIntFromObj(interp, argv[1], &row) != TCL_OK) {
	return TCL_ERROR;
    }
    if (argc == 2)
     {
      value = Selected(wPtr,row,col);
     }
    else
     {
      int row2;
      int col2;
      if (Tcl_GetIntFromObj(interp, argv[0], &col2) != TCL_OK)
       return TCL_ERROR;
      if (Tcl_GetIntFromObj(interp, argv[1], &row2) != TCL_OK)
       return TCL_ERROR;
      if (row2 < row)
       {
        int t = row;
        row = row2;
        row2 = t;
       }
      if (col2 < col)
       {
        int t = col;
        col = col2;
        col2 = t;
       }
      while (row <= row2)
       {
        while (col <= col2)
         {
          if (!Selected(wPtr,row,col))
           {
            value = 0;
            goto done;
           }
          col++;
         }
        row++;
       }
     }
   done:
    Tcl_SetIntObj(Tcl_GetObjResult(interp),value);
    return TCL_OK;
}

static int Intersect(s1, s2)
    SelectBlock * s1;
    SelectBlock * s2;
{
    return 0;
}

static int Include(s1, s2)
    SelectBlock * s1;
    SelectBlock * s2;
{
    return 0;
}

static void
Tix_GrMergeSelection(wPtr, sbPtr)
    WidgetPtr wPtr;
    SelectBlock * sbPtr;
{
    Tix_ListIterator li;

    switch (sbPtr->type) {
      case TIX_GR_SET:
      case TIX_GR_CLEAR:
	if (sbPtr->range[0][0] == 0 &&
	    sbPtr->range[1][0] == 0 &&
	    sbPtr->range[0][1] == TIX_GR_MAX &&
	    sbPtr->range[1][1] == TIX_GR_MAX) {

	    /* clear everything else from the list
	     */
	    Tix_SimpleListIteratorInit(&li);

	    for (Tix_SimpleListStart(&wPtr->selList, &li);
		 !Tix_SimpleListDone(&li);
		 Tix_SimpleListNext (&wPtr->selList, &li)) {

		SelectBlock *ptr = (SelectBlock *)li.curr;
		Tix_SimpleListDelete(&wPtr->selList, &li);
		ckfree((char*)ptr);
	    }
	}
	if (sbPtr->type == TIX_GR_SET) {
	    Tix_SimpleListAppend(&wPtr->selList, (char*)sbPtr, 0);
	}
	goto done;
    }

#if 0

    switch (sbPtr->type) {
        case TIX_GR_TOGGLE: {
        }
	break;
        case TIX_GR_SET: {
	  Tix_SimpleListAppend(&wPtr->selList, (char*)sbPtr, 0);
        }
        break;
        case TIX_GR_CLEAR: {
	    Tix_SimpleListIteratorInit(&li);

	    for (Tix_SimpleListStart(&wPtr->selList, &li);
		 !Tix_SimpleListDone(&li);
		 Tix_SimpleListNext (&wPtr->selList, &li)) {
	    }
        }

    }
#else

    Tix_SimpleListAppend(&wPtr->selList, (char*)sbPtr, 0);

#endif

  done:
    Tix_GrAddChangedRect(wPtr, sbPtr->range, 0);
}

static void
Tix_GrAdjustSelection(wPtr, sbPtr)
    WidgetPtr wPtr;
    SelectBlock * sbPtr;
{
    int changed[2][2];
    SelectBlock * current;

    current = (SelectBlock*)wPtr->selList.tail;

    /*
     * The changed region is the union of the area of the current selection
     * and the adjusted selection.
     */
    changed[TIX_X][0] = sbPtr->range[TIX_X][0];
    changed[TIX_X][1] = sbPtr->range[TIX_X][1];
    changed[TIX_Y][0] = sbPtr->range[TIX_Y][0];
    changed[TIX_Y][1] = sbPtr->range[TIX_Y][1];

    if (changed[TIX_X][0] > current->range[TIX_X][0]) {
	changed[TIX_X][0] = current->range[TIX_X][0];
    }
    if (changed[TIX_X][1] < current->range[TIX_X][1]) {
	changed[TIX_X][1] = current->range[TIX_X][1];
    }
    if (changed[TIX_Y][0] > current->range[TIX_Y][0]) {
	changed[TIX_Y][0] = current->range[TIX_Y][0];
    }
    if (changed[TIX_Y][1] < current->range[TIX_Y][1]) {
	changed[TIX_Y][1] = current->range[TIX_Y][1];
    }

    /* Adjust the current selection according to sbPtr */
    current->range[TIX_X][0] = sbPtr->range[TIX_X][0];
    current->range[TIX_X][1] = sbPtr->range[TIX_X][1];
    current->range[TIX_Y][0] = sbPtr->range[TIX_Y][0];
    current->range[TIX_Y][1] = sbPtr->range[TIX_Y][1];

    /* Set the changed area */
    Tix_GrAddChangedRect(wPtr, changed, 0);

    /* sbPtr is no longer needed */
    ckfree((char*)sbPtr);
}

static int
Tix_GrSelModify(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    WidgetPtr wPtr = (WidgetPtr) clientData;
    int type = 0, adjust = 0;
    SelectBlock * sbPtr = NULL;
    int tmp;

    if (argc != 2 && argc != 4) {
	return Tix_ArgcError(interp, argc+2, argv-2, 2, "x1 y1 ?x2 y2?");
    }

    /*
     * (1) find out the type of operation.
     */
    if (argv[-1][0] == 'a') {
	if (wPtr->selList.numItems <= 0) {
	    /*
	     * There is nothing in the selection list to adjust!
	     */
	    Tcl_AppendResult(interp, "selection list is empty", NULL);
	    return TCL_ERROR;
	}
	adjust = 1;
    }
    else if (argv[-1][0] == 'c') {
	type = TIX_GR_CLEAR;
    }
    else if (argv[-1][0] == 's') {
	type = TIX_GR_SET;
    }
    else {
	type = TIX_GR_TOGGLE;
    }

    sbPtr = (SelectBlock*)ckalloc(sizeof(SelectBlock));
    sbPtr->type = type;

    if (Tcl_GetIntFromObj(interp, argv[0], &sbPtr->range[0][0]) != TCL_OK) {
	goto error;
    }
    if (Tcl_GetIntFromObj(interp, argv[1], &sbPtr->range[1][0]) != TCL_OK) {
	goto error;
    }
    if (argc == 4) {
	if (Tcl_GetIntFromObj(interp, argv[2], &sbPtr->range[0][1]) != TCL_OK) {
	    if (strcmp(argv[2], "max") == 0) {
		Tcl_ResetResult(interp);
		sbPtr->range[0][1] = TIX_GR_MAX;
	    } else {
		goto error;
	    }
	}
	if (Tcl_GetIntFromObj(interp, argv[3], &sbPtr->range[1][1]) != TCL_OK) {
	    if (strcmp(argv[3], "max") == 0) {
		Tcl_ResetResult(interp);
		sbPtr->range[1][1] = TIX_GR_MAX;
	    } else {
		goto error;
	    }
	}
    } else {
	sbPtr->range[0][1] = sbPtr->range[0][0];
	sbPtr->range[1][1] = sbPtr->range[1][0];
    }

    if (wPtr->selectUnit != tixRowUid) {
	if (sbPtr->range[0][0] > sbPtr->range[0][1]) {
	    tmp = sbPtr->range[0][1];
	    sbPtr->range[0][1] = sbPtr->range[0][0];
	    sbPtr->range[0][0] = tmp;
	}
    } else {
	sbPtr->range[0][0] = 0;
	sbPtr->range[0][1] = TIX_GR_MAX;
    }

    if (wPtr->selectUnit != tixColumnUid) {
	if (sbPtr->range[1][0] > sbPtr->range[1][1]) {
	    tmp = sbPtr->range[1][1];
	    sbPtr->range[1][1] = sbPtr->range[1][0];
	    sbPtr->range[1][0] = tmp;
	}
    } else {
	sbPtr->range[1][0] = 0;
	sbPtr->range[1][1] = TIX_GR_MAX;
    }

    if (adjust) {
	Tix_GrAdjustSelection(wPtr, sbPtr);
	sbPtr = NULL;
    } else {
	Tix_GrMergeSelection(wPtr, sbPtr);
    }
    wPtr->toComputeSel = 1;
    return TCL_OK;

  error:
    if (sbPtr) {
	ckfree((char*)sbPtr);
    }
    return TCL_ERROR;
}
