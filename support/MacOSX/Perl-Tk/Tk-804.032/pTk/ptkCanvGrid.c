/*
 * ptkCanvGrid.c --
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
 * RCS: @(#) $Id: tkGrid.c,v 1.2 1998/09/14 18:23:16 stanton Exp $
 */

#include "tkPort.h"
#include "tk.h"
#include "tkInt.h"
#include "tkCanvases.h"

/*
 * The structure below defines the record for each rectangle/oval item.
 */

typedef struct GridItem  {
    Tk_Item header;		/* Generic stuff that's the same for all
				 * types.  MUST BE FIRST IN STRUCTURE. */
    Tk_Outline outline;		/* Outline structure */
    double bbox[4];		/* Coordinates of bounding box for rectangle
				 * or oval (x1, y1, x2, y2).  Item includes
				 * x1 and x2 but not y1 and y2. */
    int drawLines;		/* Draw lines rather than dots */
} GridItem;

/*
 * Information used for parsing configuration specs:
 */

static Tk_CustomOption stateOption = {
    TkStateParseProc,
    TkStatePrintProc, (ClientData) 2
};
static Tk_CustomOption tagsOption = {
    Tk_CanvasTagsParseProc,
    Tk_CanvasTagsPrintProc, (ClientData) NULL
};
static Tk_CustomOption dashOption = {
    TkCanvasDashParseProc,
    TkCanvasDashPrintProc, (ClientData) NULL
};
static Tk_CustomOption tileOption = {
    Tk_TileParseProc,
    Tk_TilePrintProc, (ClientData) NULL
};
static Tk_CustomOption offsetOption = {
    Tk_OffsetParseProc,
    Tk_OffsetPrintProc, (ClientData) TK_OFFSET_RELATIVE
};
static Tk_CustomOption pixelOption = {
    Tk_PixelParseProc,
    Tk_PixelPrintProc, (ClientData) NULL
};

static Tk_ConfigSpec configSpecs[] = {
    {TK_CONFIG_CUSTOM, "-dash",          NULL,          NULL,
	         NULL, Tk_Offset(GridItem, outline.dash),
	TK_CONFIG_NULL_OK, &dashOption},
    {TK_CONFIG_PIXELS, "-dashoffset",          NULL,          NULL,
	"0", Tk_Offset(GridItem, outline.offset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-disableddash",          NULL,          NULL,
	         NULL, Tk_Offset(GridItem, outline.disabledDash),
	TK_CONFIG_NULL_OK, &dashOption},
    {TK_CONFIG_COLOR, "-disabledcolor",          NULL,          NULL,
	         NULL, Tk_Offset(GridItem, outline.disabledColor),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_BITMAP, "-disabledstipple",          NULL,          NULL,
	         NULL, Tk_Offset(GridItem, outline.disabledStipple),
	TK_CONFIG_NULL_OK},
#ifdef NOTYET
    {TK_CONFIG_CUSTOM, "-disabledtile",          NULL,          NULL,
	         NULL, Tk_Offset(GridItem, outline.disabledTile),
	TK_CONFIG_NULL_OK, &tileOption},
#endif
    {TK_CONFIG_PIXELS, "-disabledwidth",          NULL,          NULL,
	"0.0", Tk_Offset(GridItem, outline.disabledWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &pixelOption},
    {TK_CONFIG_BOOLEAN, "-lines",          NULL,          NULL,
	"0", Tk_Offset(GridItem, drawLines), TK_CONFIG_NULL_OK},
    /* Previously only -fill was implemented, but -color documented.
       Misuse the "synonym" mechanism to support both. The dbName
       specification is here just to make TK_CONFIG_SYNONYM work.
     */
    {TK_CONFIG_COLOR, "-color",            "canvasGridColor", NULL,
	"black", Tk_Offset(GridItem, outline.color), TK_CONFIG_NULL_OK},
    {TK_CONFIG_SYNONYM, "-fill",           "canvasGridColor", NULL,
        NULL, 0, 0},
    {TK_CONFIG_CUSTOM, "-offset",          NULL,          NULL,
	"0,0", Tk_Offset(GridItem, outline.tsoffset),
	TK_CONFIG_DONT_SET_DEFAULT, &offsetOption},
    {TK_CONFIG_BITMAP, "-stipple",          NULL,          NULL,
	         NULL, Tk_Offset(GridItem, outline.stipple),
	TK_CONFIG_NULL_OK},
#ifdef NOT_YET
    {TK_CONFIG_CUSTOM, "-tile",          NULL,          NULL,
	         NULL, Tk_Offset(GridItem, outline.tile),
	TK_CONFIG_NULL_OK, &tileOption},
#endif
    {TK_CONFIG_CUSTOM, "-state",          NULL,          NULL,
	         NULL, Tk_Offset(Tk_Item, state),TK_CONFIG_NULL_OK,
	&stateOption},
    {TK_CONFIG_CUSTOM, "-tags",          NULL,          NULL,
	         NULL, 0, TK_CONFIG_NULL_OK, &tagsOption},
    {TK_CONFIG_CALLBACK, "-updatecommand",          NULL,          NULL,
	         NULL, Tk_Offset(Tk_Item, updateCmd), TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-width",          NULL,          NULL,
	"1.0", Tk_Offset(GridItem, outline.width),
	TK_CONFIG_DONT_SET_DEFAULT, &pixelOption},
    {TK_CONFIG_END,          NULL,          NULL,          NULL,
	         NULL, 0, 0}
};

/*
 * Prototypes for procedures defined in this file:
 */

static void		ComputeGridBbox _ANSI_ARGS_((Tk_Canvas canvas,
			    GridItem *gridPtr));
static int		ConfigureGrid _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, Tk_Item *itemPtr, int argc,
			    CONST84 Tcl_Obj *CONST *args, int flags));
static int		CreateGrid _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, struct Tk_Item *itemPtr,
			    int argc, CONST84 Tcl_Obj *CONST *args));
static void		DeleteGrid _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, Display *display));
static void		DisplayGrid _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, Display *display, Drawable dst,
			    int x, int y, int width, int height));
static int		GridCoords _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, Tk_Item *itemPtr, int argc,
			    CONST84 Tcl_Obj *CONST *args));
static int		GridToPostscript _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, Tk_Item *itemPtr, int prepass));
static int		GridToArea _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, double *areaPtr));
static double		GridToPoint _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, double *pointPtr));
static void		ScaleGrid _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, double originX, double originY,
			    double scaleX, double scaleY));
static void		TranslateGrid _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, double deltaX, double deltaY));

/*
 * The structures below defines the rectangle and oval item types
 * by means of procedures that can be invoked by generic item code.
 */

Tk_ItemType ptkCanvGridType = {
    "grid",				/* name */
    sizeof(GridItem),			/* itemSize */
    CreateGrid,				/* createProc */
    configSpecs,			/* configSpecs */
    ConfigureGrid,			/* configureProc */
    GridCoords,				/* coordProc */
    DeleteGrid,				/* deleteProc */
    DisplayGrid,			/* displayProc */
    TK_ITEM_ALWAYS_REDRAW|TK_CONFIG_OBJS,/* flags */
    GridToPoint,			/* pointProc */
    GridToArea,				/* areaProc */
    GridToPostscript,			/* postscriptProc */
    ScaleGrid,				/* scaleProc */
    TranslateGrid,			/* translateProc */
    (Tk_ItemIndexProc *) NULL,		/* indexProc */
    (Tk_ItemCursorProc *) NULL,		/* icursorProc */
    (Tk_ItemSelectionProc *) NULL,	/* selectionProc */
    (Tk_ItemInsertProc *) NULL,		/* insertProc */
    (Tk_ItemDCharsProc *) NULL,		/* dTextProc */
    (Tk_ItemType *) NULL,		/* nextPtr */
    (Tk_ItemBboxProc *) ComputeGridBbox,/* bboxProc */
    Tk_Offset(Tk_VisitorType, visitGrid), /* acceptProc */
    NULL,	/* getCoordPtr */
    NULL	/* setCoordPtr */
};


/*
 *--------------------------------------------------------------
 *
 * CreateGrid --
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
CreateGrid(interp, canvas, itemPtr, argc, args)
    Tcl_Interp *interp;			/* For error reporting. */
    Tk_Canvas canvas;			/* Canvas to hold new item. */
    Tk_Item *itemPtr;			/* Record to hold new item;  header
					 * has been initialized by caller. */
    int argc;				/* Number of arguments in args. */
    CONST84 Tcl_Obj *CONST *args;		/* Arguments describing rectangle. */
{
    GridItem *gridPtr = (GridItem *) itemPtr;
    int i;

    if (argc==1) {
	i = 1;
    } else {
	char *arg = Tcl_GetStringFromObj(args[1], NULL);
	if ((argc>1) && (arg[0] == '-')
		&& (arg[1] >= 'a') && (arg[1] <= 'z')) {
	    i = 1;
	} else {
	    i = 4;
	}
    }

    if (argc < i) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
		Tk_PathName(Tk_CanvasTkwin(canvas)), " create ",
		itemPtr->typePtr->name, " x1 y1 x2 y2 ?options?\"",
		         NULL);
	return TCL_ERROR;
    }

    /*
     * Carry out initialization that is needed in order to clean
     * up after errors during the the remainder of this procedure.
     */

    Tk_CreateOutline(&(gridPtr->outline));

    /*
     * Process the arguments to fill in the item record.
     */

    if ((GridCoords(interp, canvas, itemPtr, i, args) != TCL_OK)) {
	goto error;
    }
    if (ConfigureGrid(interp, canvas, itemPtr, argc-i, args+i, 0)
	    == TCL_OK) {
	return TCL_OK;
    }

    error:
    DeleteGrid(canvas, itemPtr, Tk_Display(Tk_CanvasTkwin(canvas)));
    return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * GridCoords --
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
GridCoords(interp, canvas, itemPtr, argc, args)
    Tcl_Interp *interp;			/* Used for error reporting. */
    Tk_Canvas canvas;			/* Canvas containing item. */
    Tk_Item *itemPtr;			/* Item whose coordinates are to be
					 * read or modified. */
    int argc;				/* Number of coordinates supplied in
					 * args. */
    Tcl_Obj *CONST *args;		/* Array of coordinates: x1, y1,
					 * x2, y2, ... */
{
    GridItem *gridPtr = (GridItem *) itemPtr;
    char c0[TCL_DOUBLE_SPACE];

    if (argc == 0) {
	Tcl_Obj *obj = Tcl_NewObj();
	Tcl_Obj *subobj = Tcl_NewDoubleObj(gridPtr->bbox[0]);
	Tcl_ListObjAppendElement(interp, obj, subobj);
	subobj = Tcl_NewDoubleObj(gridPtr->bbox[1]);
	Tcl_ListObjAppendElement(interp, obj, subobj);
	subobj = Tcl_NewDoubleObj(gridPtr->bbox[2]);
	Tcl_ListObjAppendElement(interp, obj, subobj);
	subobj = Tcl_NewDoubleObj(gridPtr->bbox[3]);
	Tcl_ListObjAppendElement(interp, obj, subobj);
	Tcl_SetObjResult(interp, obj);
    } else if ((argc == 1)||(argc == 4)) {
 	if (argc==1) {
	    if (Tcl_ListObjGetElements(interp, args[0], &argc, (Tcl_Obj ***)&args) != TCL_OK) {
		return TCL_ERROR;
	    } else if (argc != 4) {
		sprintf(c0,"%d",argc);
		Tcl_AppendResult(interp, "wrong # coordinates: expected 4, got ",
		c0,          NULL);
		return TCL_ERROR;
	    }
	}
	if ((Tk_CanvasGetCoordFromObj(interp, canvas, args[0],
 		    &gridPtr->bbox[0]) != TCL_OK)
		|| (Tk_CanvasGetCoordFromObj(interp, canvas, args[1],
		    &gridPtr->bbox[1]) != TCL_OK)
		|| (Tk_CanvasGetCoordFromObj(interp, canvas, args[2],
			&gridPtr->bbox[2]) != TCL_OK)
		|| (Tk_CanvasGetCoordFromObj(interp, canvas, args[3],
			&gridPtr->bbox[3]) != TCL_OK)) {
	    return TCL_ERROR;
	}
	ComputeGridBbox(canvas, gridPtr);
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
 * ConfigureGrid --
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
ConfigureGrid(interp, canvas, itemPtr, argc, args, flags)
    Tcl_Interp *interp;		/* Used for error reporting. */
    Tk_Canvas canvas;		/* Canvas containing itemPtr. */
    Tk_Item *itemPtr;		/* Rectangle item to reconfigure. */
    int argc;			/* Number of elements in args.  */
    CONST84 Tcl_Obj *CONST *args;	/* Arguments describing things to configure. */
    int flags;			/* Flags to pass to Tk_ConfigureWidget. */
{
    GridItem *gridPtr = (GridItem *) itemPtr;
    XGCValues gcValues;
    GC newGC;
    unsigned long mask;
    Tk_Window tkwin;
    Tk_Tile tile;
    XColor *color;
    Pixmap stipple;
    Tk_State state;
    Pixmap pixmap;

    tkwin = Tk_CanvasTkwin(canvas);

    if (Tk_ConfigureWidget(interp, tkwin, configSpecs, argc, args,
	    (char *) gridPtr, flags|TK_CONFIG_OBJS) != TCL_OK) {
	return TCL_ERROR;
    }

    /*
     * A few of the options require additional processing, such as
     * graphics contexts.
     */

    itemPtr->redraw_flags &= ~TK_ITEM_STATE_DEPENDANT;

    mask = Tk_ConfigOutlineGC(&gcValues, canvas, itemPtr, &(gridPtr->outline));
    if (mask) {
	gcValues.cap_style = CapProjecting;
	mask |= GCCapStyle;
	newGC = Tk_GetGC(tkwin, mask, &gcValues);
    } else {
	newGC = None;
    }
    if (gridPtr->outline.gc != None) {
	Tk_FreeGC(Tk_Display(tkwin), gridPtr->outline.gc);
    }
    gridPtr->outline.gc = newGC;

    ComputeGridBbox(canvas, gridPtr);

    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * DeleteGrid --
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
DeleteGrid(canvas, itemPtr, display)
    Tk_Canvas canvas;			/* Info about overall widget. */
    Tk_Item *itemPtr;			/* Item that is being deleted. */
    Display *display;			/* Display containing window for
					 * canvas. */
{
    GridItem *gridPtr = (GridItem *) itemPtr;
    Tk_DeleteOutline(display, &(gridPtr->outline));
}

/*
 *--------------------------------------------------------------
 *
 * ComputeGridBbox --
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
ComputeGridBbox(canvas, gridPtr)
    Tk_Canvas canvas;			/* Canvas that contains item. */
    GridItem *gridPtr;		/* Item whose bbox is to be
					 * recomputed. */
{
    TkCanvas *canvasPtr = (TkCanvas *) canvas;
    /* After a change in params we cover the whole (visible) canvas */
    gridPtr->header.x1 = canvasPtr->xOrigin;
    gridPtr->header.y1 = canvasPtr->yOrigin;
    gridPtr->header.x2 = gridPtr->header.x1+Tk_Width(canvasPtr->tkwin);
    gridPtr->header.y2 = gridPtr->header.y1+Tk_Height(canvasPtr->tkwin);
}

/*
 *--------------------------------------------------------------
 *
 * DisplayGrid --
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
DisplayGrid(canvas, itemPtr, display, drawable, x, y, width, height)
    Tk_Canvas canvas;			/* Canvas that contains item. */
    Tk_Item *itemPtr;			/* Item to be displayed. */
    Display *display;			/* Display on which to draw item. */
    Drawable drawable;			/* Pixmap or window in which to draw
					 * item. */
    int x, y, width, height;		/* Describes region of canvas that
					 * must be redisplayed (not used). */
{
    TkCanvas *canvasPtr = (TkCanvas *) canvas;
    GridItem *gridPtr = (GridItem *) itemPtr;
    short x1, y1, x2, y2;
    Tk_State state = Tk_GetItemState(canvas, itemPtr);

    double cx      = (double) x;
    double cy      = (double) y;
    double mx      = cx + (double) width;
    double my      = cy + (double) height;
    double gx      = gridPtr->bbox[0];
    double gy      = gridPtr->bbox[1];
    double deltaX  = gridPtr->bbox[2]-gridPtr->bbox[0];
    double deltaY  = gridPtr->bbox[3]-gridPtr->bbox[1];

    /* Set our bbox to what is drawable, we have "always redraw" set
     * so this ensures we overlap the "old" area during scroll etc.
     * and we will get re-called
     */

    ComputeGridBbox(canvas, gridPtr);

    /* Clip grid to the scroll region */
    if (canvasPtr->regionArg != NULL)
     {
      if (cx < (double) (canvasPtr->scrollX1))
       {
  	cx = (double) (canvasPtr->scrollX1);
       }
      if (cy < (double) (canvasPtr->scrollY1))
       {
  	cy = (double) (canvasPtr->scrollY1);
       }
      if (mx > (double) (canvasPtr->scrollX2))
       {
  	mx = (double) (canvasPtr->scrollX2);
       }
      if (my > (double) (canvasPtr->scrollY2))
       {
  	my = (double) (canvasPtr->scrollY2);
       }
     }

    /* Compute first grid point within the region to be drawn */
    if (cx <= gx)
     {
      gx = cx + fmod((gx-cx), deltaX);
     }
    else
     {
      gx = cx + (deltaX - fmod((cx - gx), deltaX));
     }
    if (cy <= gy)
     {
      gy = cy + fmod((gy-cy), deltaY);
     }
    else
     {
      gy = cy + (deltaY - fmod((cy - gy), deltaY));
     }

    if (gridPtr->outline.gc != None) {
	Tk_ChangeOutlineGC(canvas, itemPtr, &(gridPtr->outline));
	if (gridPtr->drawLines) {
	    while (gx < mx) {
		Tk_CanvasDrawableCoords(canvas, gx, cy, &x1, &y1);
		Tk_CanvasDrawableCoords(canvas, gx, my, &x2, &y2);
		XDrawLine(display, drawable, gridPtr->outline.gc, x1, y1, x2, y2);
		gx += deltaX;
	    }
	    while (gy < my) {
		Tk_CanvasDrawableCoords(canvas, cx, gy, &x1, &y1);
		Tk_CanvasDrawableCoords(canvas, mx, gy, &x2, &y2);
		XDrawLine(display, drawable, gridPtr->outline.gc, x1, y1, x2, y2);
		gy += deltaY;
	    }
	} else {
	    double halfWidth = gridPtr->outline.width/2;
	    double sy = gy;
	    while (gx < mx) {
		gy = sy;
		while (gy < my) {
		    Tk_CanvasDrawableCoords(canvas, gx-halfWidth, gy-halfWidth, &x1, &y1);
		    XFillRectangle(display, drawable, gridPtr->outline.gc, x1, y1,
				   gridPtr->outline.width, gridPtr->outline.width);
		    gy += deltaY;
		}
		gx += deltaX;
	    }
	}
	Tk_ResetOutlineGC(canvas, itemPtr, &(gridPtr->outline));
    }
}

/*
 *--------------------------------------------------------------
 *
 * GridToPoint --
 *
 *	Computes the distance from a given point to a given
 *	rectangle, in canvas units.
 *
 * Results:
 *	The return value is 0 if the point whose x and y coordinates
 *	are coordPtr[0] and coordPtr[1] is inside the rectangle.  If the
 *	point isn't inside the rectangle then the return value is the
 *	distance from the point to the rectangle.  If itemPtr is filled,
 *	then anywhere in the interior is considered "inside"; if
 *	itemPtr isn't filled, then "inside" means only the area
 *	occupied by the outline.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

	/* ARGSUSED */
static double
GridToPoint(canvas, itemPtr, pointPtr)
    Tk_Canvas canvas;		/* Canvas containing item. */
    Tk_Item *itemPtr;		/* Item to check against point. */
    double *pointPtr;		/* Pointer to x and y coordinates. */
{
    GridItem *gridPtr = (GridItem *) itemPtr;
    /* A grid is always a long way from anywhere */
    return 1.0e37;
}


/*
 *--------------------------------------------------------------
 *
 * GridToArea --
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
GridToArea(canvas, itemPtr, areaPtr)
    Tk_Canvas canvas;		/* Canvas containing item. */
    Tk_Item *itemPtr;		/* Item to check against rectangle. */
    double *areaPtr;		/* Pointer to array of four coordinates
				 * (x1, y1, x2, y2) describing rectangular
				 * area.  */
{
    GridItem *rectPtr = (GridItem *) itemPtr;
    /* A grid is never inside any area */
    return -1;
}


/*
 *--------------------------------------------------------------
 *
 * ScaleGrid --
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
ScaleGrid(canvas, itemPtr, originX, originY, scaleX, scaleY)
    Tk_Canvas canvas;			/* Canvas containing rectangle. */
    Tk_Item *itemPtr;			/* Rectangle to be scaled. */
    double originX, originY;		/* Origin about which to scale rect. */
    double scaleX;			/* Amount to scale in X direction. */
    double scaleY;			/* Amount to scale in Y direction. */
{
    GridItem *gridPtr = (GridItem *) itemPtr;
    gridPtr->bbox[0] = originX + scaleX*(gridPtr->bbox[0] - originX);
    gridPtr->bbox[1] = originY + scaleY*(gridPtr->bbox[1] - originY);
    gridPtr->bbox[2] = originX + scaleX*(gridPtr->bbox[2] - originX);
    gridPtr->bbox[3] = originY + scaleY*(gridPtr->bbox[3] - originY);
    ComputeGridBbox(canvas, gridPtr);
}

/*
 *--------------------------------------------------------------
 *
 * TranslateGrid --
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
TranslateGrid(canvas, itemPtr, deltaX, deltaY)
    Tk_Canvas canvas;			/* Canvas containing item. */
    Tk_Item *itemPtr;			/* Item that is being moved. */
    double deltaX, deltaY;		/* Amount by which item is to be
					 * moved. */
{
    GridItem *gridPtr = (GridItem *) itemPtr;

    gridPtr->bbox[0] += deltaX;
    gridPtr->bbox[1] += deltaY;
    gridPtr->bbox[2] += deltaX;
    gridPtr->bbox[3] += deltaY;
    ComputeGridBbox(canvas, gridPtr);
}

/*
 *--------------------------------------------------------------
 *
 * GridToPostscript --
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
GridToPostscript(interp, canvas, itemPtr, prepass)
    Tcl_Interp *interp;			/* Interpreter for error reporting. */
    Tk_Canvas canvas;			/* Information about overall canvas. */
    Tk_Item *itemPtr;			/* Item for which Postscript is
					 * wanted. */
    int prepass;			/* 1 means this is a prepass to
					 * collect font information;  0 means
					 * final Postscript is being created. */
{
    GridItem *gridPtr = (GridItem *) itemPtr;
    return TCL_OK;
}

