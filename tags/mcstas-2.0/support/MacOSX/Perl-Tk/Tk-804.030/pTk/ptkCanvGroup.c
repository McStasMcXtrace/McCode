/*
 * ptkCanvGroup.c --
 *
 *	This file implements grid items for canvas
 *	widgets.
 *
 * Copyright (c) 1991-1994 The Regents of the University of California.
 * Copyright (c) 1994-1996 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * RCS: @(#) $Id: tkGroup.c,v 1.2 1998/09/14 18:23:16 stanton Exp $
 */

#include "tkPort.h"
#include "tk.h"
#include "tkInt.h"
#include "tkCanvases.h"

/*
 * The structure below defines the record for each rectangle/oval item.
 */

typedef struct GroupItem  {
    Tk_Item header;		/* Generic stuff that's the same for all
				 * types.  MUST BE FIRST IN STRUCTURE. */
    double posn[2];		/* Nonminal position of the group */
    Tcl_Interp *interp;		/* For error reporting */
    Tk_Canvas canvas;		/* Needed to find items by id when configured */
    int numMembers;		/* Number of items in the group */
    int numSlots;		/* Space in the array */
    Tk_Item **members;		/* array of items */
} GroupItem;

/*
 * Information used for parsing configuration specs:
 */

static int		MembersParseProc _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, Tk_Window tkwin,
			    Tcl_Obj * value, char *recordPtr, int offset));
static Tcl_Obj *		MembersPrintProc _ANSI_ARGS_((ClientData clientData,
			    Tk_Window tkwin, char *recordPtr, int offset,
			    Tcl_FreeProc **freeProcPtr));


static Tk_CustomOption stateOption = {
    TkStateParseProc,
    TkStatePrintProc, (ClientData) 3
};
static Tk_CustomOption tagsOption = {
    Tk_CanvasTagsParseProc,
    Tk_CanvasTagsPrintProc, (ClientData) NULL
};

static Tk_CustomOption membersOption = {
    MembersParseProc,
    MembersPrintProc, (ClientData) NULL
};


static Tk_ConfigSpec configSpecs[] = {
    {TK_CONFIG_CUSTOM, "-members",          NULL,          NULL,
	"1.0", Tk_Offset(GroupItem, members),
	TK_CONFIG_DONT_SET_DEFAULT, &membersOption},
    {TK_CONFIG_CUSTOM, "-state",          NULL,          NULL,
	         NULL, Tk_Offset(Tk_Item, state),TK_CONFIG_NULL_OK,
	&stateOption},
    {TK_CONFIG_CUSTOM, "-tags",          NULL,          NULL,
	         NULL, 0, TK_CONFIG_NULL_OK, &tagsOption},
    {TK_CONFIG_CALLBACK, "-updatecommand",          NULL,          NULL,
	         NULL, Tk_Offset(Tk_Item, updateCmd), TK_CONFIG_NULL_OK},
    {TK_CONFIG_END,          NULL,          NULL,          NULL,
	         NULL, 0, 0}
};

/*
 * Prototypes for procedures defined in this file:
 */

static void		ComputeGroupBbox _ANSI_ARGS_((Tk_Canvas canvas,
			    GroupItem *groupPtr));
static int		ConfigureGroup _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, Tk_Item *itemPtr, int argc,
			    Tcl_Obj *CONST *args, int flags));
static int		CreateGroup _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, struct Tk_Item *itemPtr,
			    int argc, Tcl_Obj *CONST *args));
static void		DeleteGroup _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, Display *display));
static void		DisplayGroup _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, Display *display, Drawable dst,
			    int x, int y, int width, int height));
static int		GroupCoords _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, Tk_Item *itemPtr, int argc,
			    Tcl_Obj *CONST *args));
static int		GroupToPostscript _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, Tk_Item *itemPtr, int prepass));
static int		GroupToArea _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, double *areaPtr));
static double		GroupToPoint _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, double *pointPtr));
static void		ScaleGroup _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, double originX, double originY,
			    double scaleX, double scaleY));
static void		TranslateGroup _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, double deltaX, double deltaY));
static int		GroupIndex _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, Tk_Item *itemPtr, Tcl_Obj *indexString,
			    int *indexPtr));
static int		GroupInsert _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, int beforeThis, Tcl_Obj *string));
static void		GroupInsertProc _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, int beforeThis, Tcl_Obj *string));
static void		GroupDChars _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, int first, int last));

/*
 * The structures below defines the rectangle and oval item types
 * by means of procedures that can be invoked by generic item code.
 */

Tk_ItemType ptkCanvGroupType = {
    "group",				/* name */
    sizeof(GroupItem),			/* itemSize */
    CreateGroup,			/* createProc */
    configSpecs,			/* configSpecs */
    ConfigureGroup,			/* configureProc */
    GroupCoords,			/* coordProc */
    DeleteGroup,			/* deleteProc */
    DisplayGroup,			/* displayProc */
    TK_ITEM_ALWAYS_REDRAW|TK_CONFIG_OBJS,/* flags */
    GroupToPoint,			/* pointProc */
    GroupToArea,			/* areaProc */
    GroupToPostscript,			/* postscriptProc */
    ScaleGroup,				/* scaleProc */
    TranslateGroup,			/* translateProc */
    GroupIndex,				/* indexProc */
    (Tk_ItemCursorProc *) NULL,		/* icursorProc */  /* Abuse to set active? */
    (Tk_ItemSelectionProc *) NULL,	/* selectionProc */
    GroupInsertProc,			/* insertProc */
    GroupDChars,			/* dTextProc */
    (Tk_ItemType *) NULL,		/* nextPtr */
    (Tk_ItemBboxProc *) ComputeGroupBbox,/* bboxProc */
    Tk_Offset(Tk_VisitorType, visitGroup), /* acceptProc */
    NULL,	/* getCoordPtr */
    NULL	/* setCoordPtr */
};


static void
ShowMembers(char *f,GroupItem *groupPtr)
{
 int i;
 LangDebug("%s gid=%d %d [",f,groupPtr->header.id, groupPtr->numMembers);
 if (groupPtr->numMembers > groupPtr->numSlots)
  abort();
 for (i=0; i < groupPtr->numMembers; i++)
  {
   if (groupPtr->members[i])
    {
     LangDebug(" %d",groupPtr->members[i]->id);
    }
   else
    {
     LangDebug(" NULL",groupPtr->members[i]->id);
    }
  }
 LangDebug("]\n");
}



/*
 *--------------------------------------------------------------
 *
 * CreateGroup --
 *
 *	This procedure is invoked to create a new rectangle
 *	or oval item in a canvas.
 *
 * Results:
 *	A standard Tcl return value.  If an error occurred in
 *	creating the item, then an error message is left in
 *	Tcl_GetResult(interp);  in this case itemPtr is left uninitialized,
 *	so it can be safely freed by the caller.
 *
 * Side effects:
 *	A new rectangle or oval item is created.
 *
 *--------------------------------------------------------------
 */

static int
CreateGroup(interp, canvas, itemPtr, argc, args)
    Tcl_Interp *interp;			/* For error reporting. */
    Tk_Canvas canvas;			/* Canvas to hold new item. */
    Tk_Item *itemPtr;			/* Record to hold new item;  header
					 * has been initialized by caller. */
    int argc;				/* Number of arguments in args. */
    Tcl_Obj *CONST *args;		/* Arguments describing group. */
{
    GroupItem *groupPtr = (GroupItem *) itemPtr;
    int i;

    if (argc==1) {
	i = 1;
    } else {
	char *arg = Tcl_GetStringFromObj(args[1], NULL);
	if ((argc>1) && (arg[0] == '-')
		&& (arg[1] >= 'a') && (arg[1] <= 'z')) {
	    i = 1;
	} else {
	    i = 2;
	}
    }

    if (argc < i) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
		Tk_PathName(Tk_CanvasTkwin(canvas)), " create ",
		itemPtr->typePtr->name, " x1 y1 ?options?\"",
		         NULL);
	return TCL_ERROR;
    }

    /*
     * Carry out initialization that is needed in order to clean
     * up after errors during the the remainder of this procedure.
     */

    groupPtr->canvas     = canvas;
    groupPtr->interp     = interp;
    groupPtr->members    = NULL;
    groupPtr->numSlots   = 0;
    groupPtr->numMembers = 0;

    /*
     * Process the arguments to fill in the item record.
     */

    if ((GroupCoords(interp, canvas, itemPtr, i, args) != TCL_OK)) {
	goto error;
    }
    if (ConfigureGroup(interp, canvas, itemPtr, argc-i, args+i, 0)
	    == TCL_OK) {
	return TCL_OK;
    }

    error:
    DeleteGroup(canvas, itemPtr, Tk_Display(Tk_CanvasTkwin(canvas)));
    return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * GroupCoords --
 *
 *	This procedure is invoked to process the "coords" widget
 *	command on rectangles and ovals.  See the user documentation
 *	for details on what it does.
 *
 * Results:
 *	Returns TCL_OK or TCL_ERROR, and sets Tcl_GetResult(interp).
 *
 * Side effects:
 *	The coordinates for the given item may be changed.
 *
 *--------------------------------------------------------------
 */

static int
GroupCoords(interp, canvas, itemPtr, argc, args)
    Tcl_Interp *interp;			/* Used for error reporting. */
    Tk_Canvas canvas;			/* Canvas containing item. */
    Tk_Item *itemPtr;			/* Item whose coordinates are to be
					 * read or modified. */
    int argc;				/* Number of coordinates supplied in
					 * args. */
    Tcl_Obj *CONST *args;		/* Array of coordinates: x1, y1,
					 * x2, y2, ... */
{
    GroupItem *groupPtr = (GroupItem *) itemPtr;
    char c0[TCL_DOUBLE_SPACE];

    if (argc == 0) {
	Tcl_Obj *obj = Tcl_NewObj();
	Tcl_Obj *subobj = Tcl_NewDoubleObj(groupPtr->posn[0]);
	Tcl_ListObjAppendElement(interp, obj, subobj);
	subobj = Tcl_NewDoubleObj(groupPtr->posn[1]);
	Tcl_ListObjAppendElement(interp, obj, subobj);
	Tcl_SetObjResult(interp, obj);
    } else if ((argc == 1)||(argc == 2)) {
	double newX;
	double newY;
 	if (argc==1) {
	    if (Tcl_ListObjGetElements(interp, args[0], &argc, (Tcl_Obj ***)&args) != TCL_OK) {
		return TCL_ERROR;
	    } else if (argc != 2) {
		sprintf(c0,"%d",argc);
		Tcl_AppendResult(interp, "wrong # coordinates: expected 2, got ",
		c0,          NULL);
		return TCL_ERROR;
	    }
	}
	if ((Tk_CanvasGetCoordFromObj(interp, canvas, args[0],
 		    &newX) != TCL_OK)
		|| (Tk_CanvasGetCoordFromObj(interp, canvas, args[1],
		    &newY) != TCL_OK)) {
	    return TCL_ERROR;
	}
	TranslateGroup(canvas, itemPtr, newX - groupPtr->posn[0], newY - groupPtr->posn[1]);
    } else {
	sprintf(c0,"%d",argc);
	Tcl_AppendResult(interp, "wrong # coordinates: expected 0 or 4, got ",
	c0,          NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * ConfigureGroup --
 *
 *	This procedure is invoked to configure various aspects
 *	of a rectangle or oval item, such as its border and
 *	background colors.
 *
 * Results:
 *	A standard Tcl result code.  If an error occurs, then
 *	an error message is left in Tcl_GetResult(interp).
 *
 * Side effects:
 *	Configuration information, such as colors and stipple
 *	patterns, may be set for itemPtr.
 *
 *--------------------------------------------------------------
 */

static int
ConfigureGroup(interp, canvas, itemPtr, argc, args, flags)
    Tcl_Interp *interp;		/* Used for error reporting. */
    Tk_Canvas canvas;		/* Canvas containing itemPtr. */
    Tk_Item *itemPtr;		/* Rectangle item to reconfigure. */
    int argc;			/* Number of elements in args.  */
    Tcl_Obj *CONST *args;	/* Arguments describing things to configure. */
    int flags;			/* Flags to pass to Tk_ConfigureWidget. */
{
    TkCanvas *canvasPtr = (TkCanvas *) canvas;
    GroupItem *groupPtr = (GroupItem *) itemPtr;
    Tk_Item *saveGroup  = canvasPtr->activeGroup;
    Tk_State state = Tk_GetItemState(canvas, itemPtr);
    int i;
    Tk_Window tkwin = Tk_CanvasTkwin(canvas);

    if (Tk_ConfigureWidget(interp, tkwin, configSpecs, argc, args,
	    (char *) groupPtr, flags|TK_CONFIG_OBJS) != TCL_OK) {
	return TCL_ERROR;
    }

    /*
     * A few of the options require additional processing, such as
     * graphics contexts.
     */

    itemPtr->redraw_flags &= ~TK_ITEM_STATE_DEPENDANT;

    ComputeGroupBbox(canvas, groupPtr);

    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * DeleteGroup --
 *
 *	This procedure is called to clean up the data structure
 *	associated with a rectangle or oval item.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Resources associated with itemPtr are released.
 *
 *--------------------------------------------------------------
 */

static void
DeleteGroup(canvas, itemPtr, display)
    Tk_Canvas canvas;			/* Info about overall widget. */
    Tk_Item *itemPtr;			/* Item that is being deleted. */
    Display *display;			/* Display containing window for
					 * canvas. */
{
    TkCanvas *canvasPtr = (TkCanvas *) canvas;
    GroupItem *groupPtr = (GroupItem *) itemPtr;
    Tk_Item *saveGroup  = canvasPtr->activeGroup;
    Tk_State state = Tk_GetItemState(canvas, itemPtr);
    int i;

    canvasPtr->activeGroup = itemPtr;
    for (i=groupPtr->numMembers-1; i >= 0; i--) {
	Tk_Item *subitemPtr = groupPtr->members[i];
	TkGroupRemoveItem(subitemPtr);
	
#ifdef DELETE_GROUP_DELETES_MEMBERS
	if (subitemPtr != NULL) {
	    (*subitemPtr->typePtr->deleteProc)(canvas, subitemPtr, display);
	}
#endif
    }
    canvasPtr->activeGroup = saveGroup;
    if (groupPtr->members) {
	ckfree((char *) groupPtr->members);
    }
}


void
TkGroupRemoveItem(itemPtr)
Tk_Item *itemPtr;
{
    GroupItem *groupPtr = (GroupItem *) (itemPtr->group);
    if (groupPtr != NULL) {
	int i;
	for (i=groupPtr->numMembers-1; i >= 0; i--) {
	    if (groupPtr->members[i] == itemPtr) {
		int j;
		for (j=i+1; j < groupPtr->numMembers; j++) {
		    groupPtr->members[j-1] = groupPtr->members[j];
		}
		itemPtr->redraw_flags |= FORCE_REDRAW;
		groupPtr->numMembers--;
		itemPtr->group = NULL;
		return;
	    }
	}
    }
  itemPtr->group = NULL;
  LangDebug("Cannot find %d in %d\n",itemPtr->id, groupPtr->header.id);
}


/*
 *--------------------------------------------------------------
 *
 * ComputeGroupBbox --
 *
 *	This procedure is invoked to compute the bounding box of
 *	all the pixels that may be drawn as part of a rectangle
 *	or oval.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The fields x1, y1, x2, and y2 are updated in the header
 *	for itemPtr.
 *
 *--------------------------------------------------------------
 */

	/* ARGSUSED */
static void
ComputeGroupBbox(canvas, groupPtr)
    Tk_Canvas canvas;			/* Canvas that contains item. */
    GroupItem *groupPtr;		/* Item whose bbox is to be
					 * recomputed. */
{
    TkCanvas *canvasPtr = (TkCanvas *) canvas;
    Tk_Item *saveGroup  = canvasPtr->activeGroup;
    Tk_State state = Tk_GetItemState(canvas, &groupPtr->header);
    int seen = 0;
    int i;

    canvasPtr->activeGroup = &groupPtr->header;
    for (i=0; i < groupPtr->numMembers; i++) {
	Tk_Item *subitemPtr = groupPtr->members[i];
	if (subitemPtr != NULL) {
	    if (Tk_GetItemState(canvas, subitemPtr) == TK_STATE_HIDDEN) {
		continue;
	    }
	    if (seen++ == 0) {
		groupPtr->header.x1 = subitemPtr->x1;
		groupPtr->header.y1 = subitemPtr->y1;
		groupPtr->header.x2 = subitemPtr->x2;
		groupPtr->header.y2 = subitemPtr->y2;
	    } else {
		if (subitemPtr->x1 < groupPtr->header.x1) {
		     groupPtr->header.x1 = subitemPtr->x1;
		}
		if (subitemPtr->y1 < groupPtr->header.y1) {
		     groupPtr->header.y1 = subitemPtr->y1;
		}
		if (subitemPtr->x2 > groupPtr->header.x2) {
		     groupPtr->header.x2 = subitemPtr->x2;
		}
		if (subitemPtr->y2 > groupPtr->header.y2) {
		     groupPtr->header.y2 = subitemPtr->y2;
		}
	    }
	}
    }
    canvasPtr->activeGroup = saveGroup;

    /* If all items were hidden then have a "null" bbox */

    if (seen == 0) {
	groupPtr->header.x1 = groupPtr->posn[0];
	groupPtr->header.y1 = groupPtr->posn[1];
	groupPtr->header.x2 = groupPtr->header.x1;
	groupPtr->header.y2 = groupPtr->header.y1;
    }
}

/*
 *--------------------------------------------------------------
 *
 * DisplayGroup --
 *
 *	This procedure is invoked to draw a rectangle or oval
 *	item in a given drawable.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	ItemPtr is drawn in drawable using the transformation
 *	information in canvas.
 *
 *--------------------------------------------------------------
 */

static void
DisplayGroup(canvas, itemPtr, display, drawable, x, y, width, height)
    Tk_Canvas canvas;			/* Canvas that contains item. */
    Tk_Item *itemPtr;			/* Item to be displayed. */
    Display *display;			/* Display on which to draw item. */
    Drawable drawable;			/* Pixmap or window in which to draw
					 * item. */
    int x, y, width, height;		/* Describes region of canvas that
					 * must be redisplayed (not used). */
{
    TkCanvas *canvasPtr = (TkCanvas *) canvas;
    GroupItem *groupPtr = (GroupItem *) itemPtr;
    Tk_Item *saveGroup  = canvasPtr->activeGroup;
    Tk_State state = Tk_GetItemState(canvas, itemPtr);
    int i;
    if (state == TK_STATE_HIDDEN) {
	return;
    }
    canvasPtr->activeGroup = itemPtr;
    for (i=0; i < groupPtr->numMembers; i++) {
	Tk_Item *subitemPtr = groupPtr->members[i];
	if (subitemPtr != NULL) {
	    if (Tk_GetItemState(canvas, subitemPtr) == TK_STATE_HIDDEN) {
		continue;
	    }
	    if (drawable != None ||
		(subitemPtr->typePtr->alwaysRedraw & 1)) {
		if (subitemPtr->updateCmd) {
		    if (canvasPtr->updateCmds == NULL) {
			canvasPtr->updateCmds = Tcl_NewListObj(0,NULL);
		    }
		    Tcl_IncrRefCount(subitemPtr->updateCmd);
		    Tcl_ListObjAppendElement(canvasPtr->interp,canvasPtr->updateCmds,
				subitemPtr->updateCmd);
		}
		(*subitemPtr->typePtr->displayProc)(canvas, subitemPtr, display,
		    drawable, x, y, width, height);
	    }
	}
    }
    canvasPtr->activeGroup = saveGroup;
}

/*
 *--------------------------------------------------------------
 *
 * GroupToPoint --
 *
 *	Computes the distance from a given point to a given
 *	group, in canvas units.
 *
 * Results:
 *	The return value is 0 if the point whose x and y coordinates
 *	are coordPtr[0] and coordPtr[1] is inside the group.  If the
 *	point isn't inside the rectangle then the return value is the
 *	distance from the point to the group.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

	/* ARGSUSED */
static double
GroupToPoint(canvas, itemPtr, pointPtr)
    Tk_Canvas canvas;		/* Canvas containing item. */
    Tk_Item *itemPtr;		/* Item to check against point. */
    double *pointPtr;		/* Pointer to x and y coordinates. */
{
    TkCanvas *canvasPtr = (TkCanvas *) canvas;
    GroupItem *groupPtr = (GroupItem *) itemPtr;
    Tk_Item *saveGroup  = canvasPtr->activeGroup;
    Tk_State state = Tk_GetItemState(canvas, itemPtr);
    int i;

    double best = 1.0e36;

    if (state == TK_STATE_HIDDEN) {
	return best;
    }

    /* If the group is active it is invisible to picking */
    if (state == TK_STATE_ACTIVE) {
	return best;
    }

    canvasPtr->activeGroup = itemPtr;
    for (i=0; i < groupPtr->numMembers; i++) {
	Tk_Item *subitemPtr = groupPtr->members[i];
	if (subitemPtr != NULL) {
	    double try = (*subitemPtr->typePtr->pointProc)(canvas, subitemPtr, pointPtr);
	    if (try < best) {
		best = try;
		if (best == 0.0) {
		   break;
		}
	    }
	}
    }
    canvasPtr->activeGroup = saveGroup;
    return best;
}


/*
 *--------------------------------------------------------------
 *
 * GroupToArea --
 *
 *	This procedure is called to determine whether an item
 *	lies entirely inside, entirely outside, or overlapping
 *	a given rectangle.
 *
 * Results:
 *	-1 is returned if the item is entirely outside the area
 *	given by rectPtr, 0 if it overlaps, and 1 if it is entirely
 *	inside the given area.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

	/* ARGSUSED */
static int
GroupToArea(canvas, itemPtr, areaPtr)
    Tk_Canvas canvas;		/* Canvas containing item. */
    Tk_Item *itemPtr;		/* Item to check against rectangle. */
    double *areaPtr;		/* Pointer to array of four coordinates
				 * (x1, y1, x2, y2) describing rectangular
				 * area.  */
{
    TkCanvas *canvasPtr = (TkCanvas *) canvas;
    GroupItem *groupPtr = (GroupItem *) itemPtr;
    Tk_Item *saveGroup  = canvasPtr->activeGroup;
    Tk_State state = Tk_GetItemState(canvas, itemPtr);
    int i;
#define ALL_OUTSIDE 1
#define ALL_INSIDE  2
    int seen    = ALL_INSIDE|ALL_OUTSIDE;

    if (state == TK_STATE_HIDDEN) {
	return -1;
    }

    /* If the group is active it is invisible to picking */
    if (state == TK_STATE_ACTIVE) {
	return -1;
    }

    canvasPtr->activeGroup = itemPtr;
    for (i=0; i < groupPtr->numMembers; i++) {
	Tk_Item *subitemPtr = groupPtr->members[i];
	if (subitemPtr != NULL) {
	    int inner =  (*subitemPtr->typePtr->areaProc)(canvas, subitemPtr, areaPtr);
	    if (inner < 0)   /* outside */
		seen &= ~ALL_INSIDE;  /* clear the inside option */
	    if (inner == 0)  /* overlap */
		seen = 0;
	    if (inner > 0)   /* inside */
		seen &= ~ALL_OUTSIDE;  /* clear the outside option */
	    if (seen == 0)
		break;
	}
    }
    canvasPtr->activeGroup = saveGroup;

    switch (seen) {
      case 0 :
       return 0;
      case ALL_INSIDE  :
       return 1;
      default:
      case ALL_OUTSIDE :
       return -1;
    }
}


/*
 *--------------------------------------------------------------
 *
 * ScaleGroup --
 *
 *	This procedure is invoked to rescale a rectangle or oval
 *	item.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The rectangle or oval referred to by itemPtr is rescaled
 *	so that the following transformation is applied to all
 *	point coordinates:
 *		x' = originX + scaleX*(x-originX)
 *		y' = originY + scaleY*(y-originY)
 *
 *--------------------------------------------------------------
 */

static void
ScaleGroup(canvas, itemPtr, originX, originY, scaleX, scaleY)
    Tk_Canvas canvas;			/* Canvas containing rectangle. */
    Tk_Item *itemPtr;			/* Rectangle to be scaled. */
    double originX, originY;		/* Origin about which to scale rect. */
    double scaleX;			/* Amount to scale in X direction. */
    double scaleY;			/* Amount to scale in Y direction. */
{
    TkCanvas *canvasPtr = (TkCanvas *) canvas;
    GroupItem *groupPtr = (GroupItem *) itemPtr;
    Tk_Item *saveGroup  = canvasPtr->activeGroup;
    Tk_State state = Tk_GetItemState(canvas, itemPtr);
    int i;

    groupPtr->posn[0] = originX + scaleX*(groupPtr->posn[0] - originX);
    groupPtr->posn[1] = originY + scaleY*(groupPtr->posn[1] - originY);

    canvasPtr->activeGroup = itemPtr;
    for (i=0; i < groupPtr->numMembers; i++) {
	Tk_Item *subitemPtr = groupPtr->members[i];
	if (subitemPtr != NULL) {
	    (*subitemPtr->typePtr->scaleProc)(canvas, subitemPtr, originX, originY, scaleX, scaleY);
	}
    }
    canvasPtr->activeGroup = saveGroup;

    ComputeGroupBbox(canvas, groupPtr);
}

/*
 *--------------------------------------------------------------
 *
 * TranslateGroup --
 *
 *	This procedure is called to move a rectangle or oval by a
 *	given amount.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The position of the rectangle or oval is offset by
 *	(xDelta, yDelta), and the bounding box is updated in the
 *	generic part of the item structure.
 *
 *--------------------------------------------------------------
 */

static void
TranslateGroup(canvas, itemPtr, deltaX, deltaY)
    Tk_Canvas canvas;			/* Canvas containing item. */
    Tk_Item *itemPtr;			/* Item that is being moved. */
    double deltaX, deltaY;		/* Amount by which item is to be
					 * moved. */
{
    TkCanvas *canvasPtr = (TkCanvas *) canvas;
    GroupItem *groupPtr = (GroupItem *) itemPtr;
    Tk_Item *saveGroup  = canvasPtr->activeGroup;
    int i;
    groupPtr->posn[0] += deltaX;
    groupPtr->posn[1] += deltaY;
    canvasPtr->activeGroup = itemPtr;
    for (i=0; i < groupPtr->numMembers; i++) {
	Tk_Item *subitemPtr = groupPtr->members[i];
	if (subitemPtr != NULL) {
	    (*subitemPtr->typePtr->translateProc)(canvas, subitemPtr, deltaX, deltaY);
	}
    }
    canvasPtr->activeGroup = saveGroup;
    ComputeGroupBbox(canvas, groupPtr);
}

/*
 *--------------------------------------------------------------
 *
 * GroupToPostscript --
 *
 *	This procedure is called to generate Postscript for
 *	rectangle and oval items.
 *
 * Results:
 *	The return value is a standard Tcl result.  If an error
 *	occurs in generating Postscript then an error message is
 *	left in Tcl_GetResult(interp), replacing whatever used to be there.
 *	If no error occurs, then Postscript for the rectangle is
 *	appended to the result.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

static int
GroupToPostscript(interp, canvas, itemPtr, prepass)
    Tcl_Interp *interp;			/* Interpreter for error reporting. */
    Tk_Canvas canvas;			/* Information about overall canvas. */
    Tk_Item *itemPtr;			/* Item for which Postscript is
					 * wanted. */
    int prepass;			/* 1 means this is a prepass to
					 * collect font information;  0 means
					 * final Postscript is being created. */
{
    TkCanvas *canvasPtr = (TkCanvas *) canvas;
    GroupItem *groupPtr = (GroupItem *) itemPtr;
    Tk_Item *saveGroup  = canvasPtr->activeGroup;
    Tk_State state = Tk_GetItemState(canvas, itemPtr);
    int code = TCL_OK;
    int i;

    if (state == TK_STATE_HIDDEN) {
	return code;
    }

    canvasPtr->activeGroup = itemPtr;
    for (i=0; i < groupPtr->numMembers; i++) {
	Tk_Item *subitemPtr = groupPtr->members[i];
	if (subitemPtr != NULL) {
	    if (Tk_GetItemState(canvas, subitemPtr) == TK_STATE_HIDDEN) {
		continue;
	    }
	    code = (*subitemPtr->typePtr->postscriptProc)(interp, canvas,
			subitemPtr, prepass);
	    if (code != TCL_OK) {
		break;
	    }
	}
    }
    canvasPtr->activeGroup = saveGroup;
    return code;
}

static int
GroupIndex(interp, canvas, itemPtr, obj, indexPtr)
Tcl_Interp *interp;
Tk_Canvas canvas;
Tk_Item *itemPtr;
Tcl_Obj *obj;
int *indexPtr;
{
    TkCanvas *canvasPtr = (TkCanvas *) canvas;
    GroupItem *groupPtr = (GroupItem *) itemPtr;
    Tk_Item *saveGroup  = canvasPtr->activeGroup;
    Tk_State state = Tk_GetItemState(canvas, itemPtr);
    int i;
    int length;
    int id;
    char *string;
    double point[2];
    double bestDist;
    char *end, *p;
    Tcl_Obj **objv;

    *indexPtr = -1;

    if (Tcl_ListObjGetElements(interp, obj, &i, &objv) == TCL_OK && i == 2
	&& Tk_CanvasGetCoordFromObj(interp, canvas, objv[0], &point[0]) == TCL_OK
	&& Tk_CanvasGetCoordFromObj(interp, canvas, objv[1], &point[1]) == TCL_OK) {
	goto doxy;
    }

    string = Tcl_GetStringFromObj(obj, &length);
    if (string[0] == 'e') {
	if (strncmp(string, "end", length) == 0) {
	    *indexPtr = groupPtr->numMembers;
	} else {
	    badIndex:

	    /*
	     * Some of the paths here leave messages in interp->result,
	     * so we have to clear it out before storing our own message.
	     */

	    Tcl_SetResult(interp, (char *) NULL, TCL_STATIC);
	    Tcl_AppendResult(interp, "bad index \"", string, "\"",
		    (char *) NULL);
	    return TCL_ERROR;
	}
    } else if (string[0] == '@') {
	p = string+1;
	point[0] = strtod(p, &end);
	if ((end == p) || (*end != ',')) {
	    goto badIndex;
	}
	p = end+1;
	point[1] = strtod(p, &end);
	if ((end == p) || (*end != 0)) {
	    goto badIndex;
	}
     doxy:
	bestDist = 1.0e36;
	*indexPtr = 0;
	canvasPtr->activeGroup = itemPtr;
	for(i=0; i < groupPtr->numMembers; i++) {
	    Tk_Item *subitemPtr = groupPtr->members[i];
	    double dist = (*subitemPtr->typePtr->pointProc)(canvas, subitemPtr, point);
	    if (dist < bestDist) {
		bestDist  = dist;
		*indexPtr = i;
	    }
	}
	canvasPtr->activeGroup = saveGroup;
    } else {
	if (Tcl_GetIntFromObj(interp, obj, &id) == TCL_OK) {
	    for(i=0; i < groupPtr->numMembers; i++) {
		Tk_Item *subitemPtr = groupPtr->members[i];
		if (subitemPtr != NULL && subitemPtr->id == id) {
		    *indexPtr = i;
		    return TCL_OK;
		}
	    }
	    goto badIndex;
	} else {
	    return TCL_ERROR;
	}
    }
    return TCL_OK;
}

static int
GroupInsert(canvas, itemPtr, beforeThis, string)
Tk_Canvas canvas;
Tk_Item *itemPtr;
int beforeThis;
Tcl_Obj *string;
{
    TkCanvas *canvasPtr = (TkCanvas *) canvas;
    GroupItem *groupPtr = (GroupItem *) itemPtr;
    Tk_Item *saveGroup  = canvasPtr->activeGroup;
    Tk_State state = Tk_GetItemState(canvas, itemPtr);
    Tcl_Obj **objv;
    int argc;
    int i;
    int id;
    if (Tcl_ListObjGetElements(groupPtr->interp,string,&argc,&objv) == TCL_OK) {
	int count = 0;
	for (i=0; i < argc; i++) {
	    if (Tcl_GetIntFromObj(groupPtr->interp,objv[i],&id) == TCL_OK) {
		Tcl_HashEntry *entryPtr = Tcl_FindHashEntry(&canvasPtr->idTable, (char *) id);
		if (entryPtr != NULL) {
		    Tk_Item *subitemPtr = (Tk_Item *) Tcl_GetHashValue(entryPtr);
		    if (subitemPtr == NULL
			|| subitemPtr == itemPtr
			|| subitemPtr->group == itemPtr) {
			continue;
		    }
		    if (subitemPtr->group != NULL) {
			TkGroupRemoveItem(subitemPtr);
		    }
		    count++;
		}
	    } else {
		return TCL_ERROR;
	    }
	}
	i = count + groupPtr->numMembers;
	if (i > groupPtr->numSlots) {
	    if (groupPtr->members == NULL) {
		groupPtr->members = (Tk_Item **)ckalloc(i*sizeof(Tk_Item *));
	    } else {
		groupPtr->members = (Tk_Item **)ckrealloc((char *)groupPtr->members,
					i*sizeof(Tk_Item *));
	    }
	    if (groupPtr->members != NULL) {
		groupPtr->numSlots = i;
	    } else {
		groupPtr->numMembers = 0;
		groupPtr->numSlots   = 0;
		Tcl_SetResult(groupPtr->interp,"Out of memory",TCL_STATIC);
		return TCL_ERROR;
	    }
	}
	/* Move tail up */
	for (i=groupPtr->numMembers-1; i >= beforeThis; i--) {
		groupPtr->members[i+count] = groupPtr->members[i];
	}
	/* Fill in slots */
	groupPtr->numMembers += count;
	for (i=0; i < argc; i++) {
	    groupPtr->members[beforeThis] = NULL;
	    if (Tcl_GetIntFromObj(groupPtr->interp,objv[i],&id) == TCL_OK) {
		Tcl_HashEntry *entryPtr = Tcl_FindHashEntry(&canvasPtr->idTable, (char *) id);
		if (entryPtr != NULL) {
		    Tk_Item *subitemPtr = (Tk_Item *) Tcl_GetHashValue(entryPtr);
		    if (subitemPtr == NULL
			|| subitemPtr == itemPtr
			|| subitemPtr->group == itemPtr) {
			continue;
		    }
		    subitemPtr->group = itemPtr;
		    subitemPtr->redraw_flags |= FORCE_REDRAW;
		    groupPtr->members[beforeThis] = subitemPtr;
		    beforeThis++;
		    count--;
		}
	    }
	}
	if (count != 0) {
	   abort();
	}
	ComputeGroupBbox(groupPtr->canvas, groupPtr);
	return TCL_OK;
    } else {
	return TCL_ERROR;
    }
}

/* Just like above but with void return to go in the function table */
static void
GroupInsertProc(canvas, itemPtr, beforeThis, string)
Tk_Canvas canvas;
Tk_Item *itemPtr;
int beforeThis;
Tcl_Obj *string;
{
 GroupInsert(canvas, itemPtr, beforeThis, string);
}


static void
GroupDChars(canvas, itemPtr, first, last)
Tk_Canvas canvas;
Tk_Item *itemPtr;
int first;
int last;
{
    TkCanvas *canvasPtr = (TkCanvas *) canvas;
    GroupItem *groupPtr = (GroupItem *) itemPtr;
    Tk_Item *saveGroup  = canvasPtr->activeGroup;
    Tk_State state = Tk_GetItemState(canvas, itemPtr);
    int i;

    if (first < 0) {
	first = 0;
    }
    if (last >=  groupPtr->numMembers) {
	last = groupPtr->numMembers-1;
    }
    if (first > last) {
	return;
    }
    for (i=last; i >= first; i--) {
	TkGroupRemoveItem(groupPtr->members[i]);
    }
    ComputeGroupBbox(groupPtr->canvas, groupPtr);
}

static int
MembersParseProc(clientData,interp,tkwin,value,recordPtr,offset)
ClientData clientData;
Tcl_Interp *interp;
Tk_Window tkwin;
Tcl_Obj * value;
char *recordPtr;
int offset;
{
    Tk_Item *itemPtr    = (Tk_Item *) recordPtr;
    GroupItem *groupPtr = (GroupItem *) itemPtr;
    int code = TCL_OK;
    Tk_CanvasEventuallyRedraw(groupPtr->canvas, itemPtr->x1, itemPtr->y1, itemPtr->x2, itemPtr->y2);
    GroupDChars(groupPtr->canvas, itemPtr, 0, groupPtr->numMembers-1);
    code = GroupInsert(groupPtr->canvas, itemPtr, 0, value);
    Tk_CanvasEventuallyRedraw(groupPtr->canvas, itemPtr->x1, itemPtr->y1, itemPtr->x2, itemPtr->y2);
    return code;
}

static Tcl_Obj *
MembersPrintProc(clientData,tkwin,recordPtr,offset,freeProcPtr)
ClientData clientData;
Tk_Window tkwin;
char *recordPtr;
int offset;
Tcl_FreeProc **freeProcPtr;
{
    GroupItem *groupPtr = (GroupItem *) recordPtr;
    Tcl_Obj *result = Tcl_NewListObj(0,NULL);
    int i;
    for (i=0; i < groupPtr->numMembers; i++) {
	Tk_Item *subitemPtr = groupPtr->members[i];
	if (subitemPtr != NULL) {
	    Tcl_ListObjAppendElement(groupPtr->interp,result,
			Tcl_NewIntObj(subitemPtr->id));
	}
    }
    return result;
}


