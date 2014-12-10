
/*	$Id: tixGrid.h,v 1.1.1.1 2000/05/17 11:08:42 idiscovery Exp $	*/

/*
 * tixGrid.h --
 *
 *	Defines main data structures for tixGrid
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#ifndef _TIX_GRID_H_
#define _TIX_GRID_H_
#include "tkVMacro.h"

#ifndef _TIX_GRID_DATA_H_
#include "tixGrData.h"
#endif

#define TIX_X 0
#define TIX_Y 1


#define TIX_S_MARGIN 0
#define TIX_X_MARGIN 1
#define TIX_Y_MARGIN 2
#define TIX_MAIN     3

#define TIX_SITE_NONE -1

typedef struct TixGrEntry {
    Tix_DItem * iPtr;
    Tcl_HashEntry * entryPtr[2];	/* The index of this entry in the
					 * row/col tables */
} TixGrEntry;

/*----------------------------------------------------------------------
 * 			Render Block
 *
 * Before the Grid is rendered, information is filled into a pseudo 2D
 * array of RenderBlockElem's:
 *
 *	(1) entries are placed in the appropriate (x,y) locations
 *	(2) background and borders are formatted according
 *	(3) highlights are formatted.
 *
 * The widget is redrawn using the render-block. This saves reformatting
 * the next time the widget is exposed.
 *----------------------------------------------------------------------
 */
typedef struct RenderBlockElem {
    TixGrEntry * chPtr;		/* not allocated, don't need to free */
    int borderW[2][2];
    int index[2];

    unsigned int selected : 1;
    unsigned int filled : 1;
} RenderBlockElem;


/* ElmDispSize --
 *
 *	This structure stores the size information of the visible
 *	rows (RenderBlock.dispSize[0][...]) and columns
 *	(RenderBlock.dispSize[1][...])
 */
typedef struct ElmDispSize {
    int preBorder;
    int size;
    int postBorder;

    int total;		/* simple the sum of the above */
} ElmDispSize;

typedef struct RenderBlock {
    int size[2];		/* num of rows and cols in the render block */

    RenderBlockElem **elms;   	/* An Malloc'ed pseudo 2D array (you can do
				 * things like elms[0][0]), Used for the
				 * main body of the Grid.
				 */
    ElmDispSize *dispSize[2];	/* (dispSizes[0][x], dispSizes[1][y])
				 * will be the dimension of the element (x,y)
				 * displayed on the screen (may be bigger
				 * or smaller than its desired size). */
    int visArea[2];		/* visible area (width times height) of
				 * the visible cells on the screen */
}  RenderBlock;

/*----------------------------------------------------------------------
 * 			RenderInfo
 *
 * This stores information for rendering from the RB into an X drawable.
 *
 *----------------------------------------------------------------------
 */
typedef struct RenderInfo {
    Drawable drawable;
    int origin[2];
    int offset[2];
    int size[2];		/* width and height of the area to draw
				 * (number of pixels starting from the offset)
				 * if offset = (2,2) and size = (5,5) we have
				 * to draw the rectangle ((2,2), (6,6));
				 */
    struct {			/* the current valid grid area for the */
	int x1, x2, y1, y2;	/* "format" command */
	int whichArea;
    } fmt;
} RenderInfo;

typedef struct ExposedArea {
    int x1, y1, x2, y2;
} ExposedArea, Rect;

/*----------------------------------------------------------------------
 * 			ColorInfo
 *
 * These colors are used by the format commands. They must be saved
 * or otherwise the colormap may be changed ..
 *----------------------------------------------------------------------
 */
typedef struct ColorInfo {
    struct ColorInfo * next;
    int counter;
    int type;			/* TK_CONFIG_BORDER or TK_CONFIG_COLOR */
    long pixel;
    Tk_3DBorder border;
    XColor * color;
} ColorInfo;

/*----------------------------------------------------------------------
 * 			SelectBlock
 *
 * These structures are arranged in a list and are used to determine
 * where a cell is selected.
 *----------------------------------------------------------------------
 */
#define TIX_GR_CLEAR		1
#define TIX_GR_SET		2
#define TIX_GR_TOGGLE		3

#define TIX_GR_MAX		0x7fffffff

#define TIX_GR_RESIZE		1
#define TIX_GR_REDRAW		2


typedef struct SelectBlock {
    struct SelectBlock * next;
    int range[2][2];		/* the top left and bottom right corners */
    int type;			/* TIX_GR_CLEAR, TIX_GR_SET,
				 * TIX_GR_TOGGLE
				 *
				 * If several SelectBlock covers the same
				 * cell, the last block in the wPtr->selList
				 * determines whether this cell is selected
				 * or not */
} SelectBlock;

/*----------------------------------------------------------------------
 * 			GrSortItem
 *
 * Used to sort the items in the grid
 *----------------------------------------------------------------------
 */
typedef struct Tix_GrSortItem {
    Tcl_Obj * data;			/* is usually a string, but
					 * can be a pointer to an
					 * arbitrary data in C API */
    int index;				/* row or column */
} Tix_GrSortItem;

/*----------------------------------------------------------------------
 * Data structure for iterating the cells inside the grid.
 *
 *----------------------------------------------------------------------
 */

typedef struct Tix_GrDataRowSearch {
    struct TixGridRowCol * row;
    Tcl_HashSearch hashSearch;
    Tcl_HashEntry *hashPtr;
} Tix_GrDataRowSearch;

typedef struct Tix_GrDataCellSearch {
    char * data;
    Tcl_HashSearch hashSearch;
    Tcl_HashEntry *hashPtr;
} Tix_GrDataCellSearch;

/*----------------------------------------------------------------------
 *
 *	        Main data structure of the grid widget.
 *
 *----------------------------------------------------------------------
 */
typedef struct Tix_GridScrollInfo {
    LangCallback *command;

    int max;		/* total size (width or height) of the widget*/
    int offset;		/* The top/left side of the scrolled widget */
    int unit;		/* How much should we scroll when the user */

    double window;	/* visible size, percentage of the total */
}Tix_GridScrollInfo;


typedef struct GridStruct {
    Tix_DispData dispData;

    Tcl_Command widgetCmd;	/* Token for button's widget command. */

    /*
     * Information used when displaying widget:
     */
    int reqSize[2];		/* For app programmer to request size */

    /*
     * Information used when displaying widget:
     */

    /* Border and general drawing */
    int borderWidth;		/* Width of 3-D borders. */
    int selBorderWidth;		/* Width of 3-D borders for selected items */
    int relief;			/* Indicates whether window as a whole is
				 * raised, sunken, or flat. */
    Tk_3DBorder border;		/* Used for drawing the 3d border. */
    Tk_3DBorder selectBorder;	/* Used for selected background. */
    XColor *normalFg;		/* Normal foreground for text. */
    XColor *normalBg;		/* Normal background for  text. */
    XColor *selectFg;		/* Color for drawing selected text. */

    Tk_Uid state;		/* State can only be normal or disabled. */

       /* GC and stuff */
    GC backgroundGC;		/* GC for drawing background. */
    GC selectGC;		/* GC for drawing selected background. */
    GC anchorGC;		/* GC for drawing dotted anchor highlight. */
    TixFont font;		/* Default font used by the DItems. */

    /* Text drawing */
    Tk_Cursor cursor;		/* Current cursor for window, or None. */

    /* For highlights */
    int highlightWidth;		/* Width in pixels of highlight to draw
				 * around widget when it has the focus.
				 * <= 0 means don't draw a highlight. */
    int bdPad;			/* = highlightWidth + borderWidth */
    XColor *highlightColorPtr;	/* Color for drawing traversal highlight. */
    GC highlightGC;		/* For drawing traversal highlight. */

    /*
     * default pad and gap values
     */
    int padX, padY;

    Tk_Uid selectMode;		/* Selection style: single, browse, multiple,
				 * or extended.  This value isn't used in C
				 * code, but the Tcl bindings use it. */
    Tk_Uid selectUnit;		/* Selection unit: cell, row or column.
				 * This value isn't used in C
				 * code, but the Tcl bindings use it. */

    /*
     * The following three sites are used according to the -selectunit.
     * if selectunit is: "cell", [0] and [1] are used; "row", only [0]
     * is used; "column", only [1] is used
     */
    int anchor[2];		/* The current anchor unit */
    int dropSite[2];		/* The current drop site */
    int dragSite[2];		/* The current drop site */

    /*
     * Callback commands.
     */
    LangCallback *command;		/* The command when user double-clicks */
    LangCallback *browseCmd;		/* The command to call when the selection
				 * changes. */
    LangCallback *editNotifyCmd;	/* The command to call to determine whether
				 * a cell is editable. */
    LangCallback *editDoneCmd;		/* The command to call when an entry has
				 * been edited by the user.*/
    LangCallback *formatCmd;		/* The command to call when the Grid widget
				 * needs to be reformatted (e.g, Exposure
				 * events or when contents have been
				 * changed). */
    LangCallback *sizeCmd;		/* The command to call when the size of
				 * the listbox changes. E.g., when the user
				 * add/deletes elements. Useful for auto-
				 * scrollbar geometry managers */

    /*
     * Info for lay-out
     */
    char *takeFocus;		/* Value of -takefocus option;  not used in
				 * the C code, but used by keyboard traversal
				 * scripts.  Malloc'ed, but may be NULL. */

    int serial;			/* this number is incremented before each time
				 * the widget is redisplayed */

    TixGridDataSet * dataSet;
    RenderBlock * mainRB;	/* Malloc'ed */

    int hdrSize[2];		/* number of rows (height of x header, index
				 * [0]) and columns (width of y header, index
				 * [1]) */
    int floatRange[2];		/* Are the num of columns and rows floated?
				 * (if floated, you can scroll past the max
				 * element).*/
    int gridSize[2];		/* the size of the grid where there is data */
    Tix_DItemInfo * diTypePtr;	/* Default item type */
    ExposedArea expArea;

    RenderInfo * renderInfo;	/* only points to stuff in stack */
    Tix_GridScrollInfo scrollInfo[2];
    int fontSize[2];		/* size of the "0" char of the -font option
				 */
    TixGridSize defSize[2];
    Tix_LinkList colorInfo;
    Tix_LinkList selList;
    Tix_LinkList mappedWindows;
    int colorInfoCounter;

    unsigned int hasFocus  : 1;

    unsigned int idleEvent : 1;
    unsigned int toResize  : 1;		/* idle event */
    unsigned int toRedraw : 1;		/* idle event */

    unsigned int toResetRB  : 1; /* Do we need to reset the render block */
    unsigned int toComputeSel  : 1;
    unsigned int toRedrawHighlight : 1;
} Grid;

typedef Grid   WidgetRecord;
typedef Grid * WidgetPtr;

#define DEF_GRID_BG_COLOR		NORMAL_BG
#define DEF_GRID_BG_MONO		WHITE
#define DEF_GRID_BORDER_WIDTH		"2"
#define DEF_GRID_BROWSE_COMMAND		""
#define DEF_GRID_COMMAND		""
#define DEF_GRID_CURSOR			""
#define DEF_GRID_DEFAULT_WIDTH		"40"
#define DEF_GRID_DEFAULT_HEIGHT		"20"
#define DEF_GRID_EDITDONE_COMMAND	""
#define DEF_GRID_EDITNOTIFY_COMMAND	""
#define DEF_GRID_FLOATING_ROWS		"0"
#define DEF_GRID_FLOATING_COLS		"0"
#define DEF_GRID_FONT			"Helvetica -12 bold"
#define DEF_GRID_FG_COLOR		BLACK
#define DEF_GRID_FG_MONO		BLACK
#define DEF_GRID_FORMAT_COMMAND		""
#define DEF_GRID_HEIGHT			"10"
#define DEF_GRID_HIGHLIGHT_COLOR	BLACK
#define DEF_GRID_HIGHLIGHT_MONO		BLACK
#define DEF_GRID_HIGHLIGHT_WIDTH	"2"
#define DEF_GRID_LEFT_MARGIN		"1"
#define DEF_GRID_ITEM_TYPE		"text"
#define DEF_GRID_RELIEF			"sunken"
#define DEF_GRID_PADX			"2"
#define DEF_GRID_PADY			"2"
#define DEF_GRID_SELECT_BG_COLOR	ACTIVE_BG
#define DEF_GRID_SELECT_FG_COLOR	BLACK
#define DEF_GRID_SELECT_BG_MONO		BLACK
#define DEF_GRID_SELECT_FG_MONO		WHITE
#define DEF_GRID_SELECT_MODE		"single"
#define DEF_GRID_SELECT_UNIT		"row"
#define DEF_GRID_SELECT_BORDERWIDTH	"1"
#define DEF_GRID_STATE			"normal"
#define DEF_GRID_SIZE_COMMAND		""
#define DEF_GRID_TAKE_FOCUS 		"1"
#define DEF_GRID_TOP_MARGIN		"1"
#define DEF_GRID_WIDTH			"4"
#define DEF_GRID_Y_SCROLL_COMMAND	""
#define DEF_GRID_X_SCROLL_COMMAND	""

/*
 * common functions
 */

EXTERN void		Tix_GrAddChangedRect _ANSI_ARGS_((
			    WidgetPtr wPtr, int changedRect[2][2],
			    int isSite));
EXTERN int		Tix_GrConfigSize _ANSI_ARGS_((Tcl_Interp *interp,
			    WidgetPtr wPtr, int argc, char **argv,
			    TixGridSize *sizePtr, char * argcErrorMsg,
			    int *changed_ret));
EXTERN void		Tix_GrDoWhenIdle _ANSI_ARGS_((WidgetPtr wPtr,
			    int type));
EXTERN void		Tix_GrCancelDoWhenIdle _ANSI_ARGS_((WidgetPtr wPtr));
EXTERN void		Tix_GrFreeElem _ANSI_ARGS_((TixGrEntry * chPtr));
EXTERN void		Tix_GrFreeUnusedColors _ANSI_ARGS_((WidgetPtr wPtr,
			    int freeAll));
EXTERN void		Tix_GrScrollPage _ANSI_ARGS_((WidgetPtr wPtr,
			    int count, int axis));

/*
 * The dataset functions
 */

EXTERN int		TixGridDataConfigRowColSize _ANSI_ARGS_((
			    Tcl_Interp * interp, WidgetPtr wPtr,
			    TixGridDataSet * dataSet, int which, int index,
			    int argc, char ** argv, char * argcErrorMsg,
			    int *changed_ret));
EXTERN char *		TixGridDataCreateEntry _ANSI_ARGS_((
			    TixGridDataSet * dataSet, int x, int y,
			    char * defaultEntry));
EXTERN int		TixGridDataDeleteEntry _ANSI_ARGS_((
			    TixGridDataSet * dataSet, int x, int y));
EXTERN void		TixGridDataDeleteRange _ANSI_ARGS_((WidgetPtr wPtr,
			    TixGridDataSet * dataSet, int which,
			    int from, int to));
EXTERN void 		TixGridDataDeleteSearchedEntry _ANSI_ARGS_((
			    Tix_GrDataCellSearch * cellSearchPtr));
EXTERN char *		TixGridDataFindEntry _ANSI_ARGS_((
			    TixGridDataSet * dataSet, int x, int y));
EXTERN int		TixGrDataFirstCell _ANSI_ARGS_((
			    Tix_GrDataRowSearch * rowSearchPtr,
			    Tix_GrDataCellSearch * cellSearchPtr));
EXTERN int		TixGrDataFirstRow _ANSI_ARGS_((
			    TixGridDataSet* dataSet,
			    Tix_GrDataRowSearch * rowSearchPtr));
EXTERN int		TixGridDataGetRowColSize _ANSI_ARGS_((
			    WidgetPtr wPtr, TixGridDataSet * dataSet,
			    int which, int index, TixGridSize * defSize,
			    int *pad0, int * pad1));
EXTERN void		TixGridDataGetGridSize _ANSI_ARGS_((
			    TixGridDataSet * dataSet, int *width_ret,
			    int *height_ret));
EXTERN int		TixGridDataGetIndex _ANSI_ARGS_((
			    Tcl_Interp * interp, WidgetPtr wPtr,
			    Tcl_Obj * xStr, Tcl_Obj * yStr, int * xPtr, int * yPtr));
EXTERN void 		TixGridDataInsert _ANSI_ARGS_((
			    TixGridDataSet * dataSet,
			    int x, int y, ClientData data));
EXTERN void		TixGridDataMoveRange _ANSI_ARGS_((WidgetPtr wPtr,
			    TixGridDataSet * dataSet, int which,
			    int from, int to, int by));
EXTERN int		TixGrDataNextCell _ANSI_ARGS_((
			    Tix_GrDataCellSearch * cellSearchPtr));
EXTERN int		TixGrDataNextRow _ANSI_ARGS_((
			    Tix_GrDataRowSearch * rowSearchPtr));
EXTERN TixGridDataSet*	TixGridDataSetInit _ANSI_ARGS_((void));
EXTERN void		TixGridDataSetFree _ANSI_ARGS_((
			    TixGridDataSet* dataSet));
EXTERN int		TixGridDataUpdateSort _ANSI_ARGS_((
			    TixGridDataSet * dataSet, int axis,
			    int start, int end, Tix_GrSortItem *items));

#endif /*_TIX_GRID_H_*/
