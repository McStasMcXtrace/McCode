/* -*-mode: C; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*- */

/*	$Id: tixDiStyle.c,v 1.2.2.1 2002/01/24 08:59:32 idiscovery Exp $	*/

/*
 * tixDiStyle.c --
 *
 *	This file implements the "Display Item Styles" in the Tix library.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *
 */

#include "tixPort.h"
#include "tix.h"
#include "tixInt.h"
#include "tkInt.h"

#ifdef _LANG
#define FORWARD extern
#define LINKAGE
#define DItemStyleParseProc TixDItemStyleParseProc
#define DItemStylePrintProc TixDItemStylePrintProc
#else
#define FORWARD static
#define LINKAGE static
#endif



typedef struct StyleLink {
    Tix_DItemInfo * diTypePtr;
    Tix_DItemStyle* stylePtr;
    struct StyleLink * next;
} StyleLink;

typedef struct StyleInfo {
    Tix_StyleTemplate * tmplPtr;
    Tix_StyleTemplate tmpl;
    StyleLink * linkHead;
} StyleInfo;


FORWARD int   		DItemStyleParseProc _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, Tk_Window tkwin,
			    Tcl_Obj * value,char *widRec, int offset));
FORWARD Tcl_Obj * 		DItemStylePrintProc _ANSI_ARGS_((
			    ClientData clientData, Tk_Window tkwin,
			    char *widRec, int offset,
			    Tcl_FreeProc **freeProcPtr));
static Tix_DItemStyle*	FindDefaultStyle _ANSI_ARGS_((Tcl_Interp *interp,
			    Tix_DItemInfo * diTypePtr, Tk_Window tkwin));
static Tix_DItemStyle*	FindStyle _ANSI_ARGS_((
			    char *styleName, Tcl_Interp *interp));
static Tix_DItemStyle* 	GetDItemStyle  _ANSI_ARGS_((
			    Tix_DispData * ddPtr, Tix_DItemInfo * diTypePtr,
			    char * styleName, int *isNew_ret));
static void 		InitHashTables _ANSI_ARGS_((void));
static void		ListAdd _ANSI_ARGS_((Tix_DItemStyle * stylePtr,
			    Tix_DItem *iPtr));
static void		ListDelete _ANSI_ARGS_((Tix_DItemStyle * stylePtr,
			    Tix_DItem *iPtr));
static void		ListDeleteAll _ANSI_ARGS_((Tix_DItemStyle * stylePtr));
static void		StyleCmdDeletedProc _ANSI_ARGS_((
			    ClientData clientData));
static int		StyleCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
static int		StyleConfigure _ANSI_ARGS_((Tcl_Interp *interp,
			    Tix_DItemStyle* stylePtr, int argc,
			    char **argv, int flags));
static void		StyleDestroy _ANSI_ARGS_((char *clientData));
static void		DeleteStyle _ANSI_ARGS_((Tix_DItemStyle * stylePtr));
static void		DefWindowStructureProc _ANSI_ARGS_((
			    ClientData clientData, XEvent *eventPtr));
static void		RefWindowStructureProc _ANSI_ARGS_((
			    ClientData clientData, XEvent *eventPtr));
static void		SetDefaultStyle _ANSI_ARGS_((Tix_DItemInfo *diTypePtr,
			    Tk_Window tkwin, Tix_DItemStyle * stylePtr));
static Tcl_HashTable *	GetDefaultTable _ANSI_ARGS_((Tcl_Interp *interp));
static Tcl_HashTable *	GetStyleTable   _ANSI_ARGS_((Tcl_Interp *interp));
void                    DestroyDefaultTable _ANSI_ARGS_((ClientData clientData,
			     Tcl_Interp *interp));
void                    DestroyStyleTable _ANSI_ARGS_((ClientData clientData,
			     Tcl_Interp *interp));

static TIX_DECLARE_SUBCMD(StyleConfigCmd);
static TIX_DECLARE_SUBCMD(StyleCGetCmd);
static TIX_DECLARE_SUBCMD(StyleDeleteCmd);
extern TIX_DECLARE_SUBCMD(Tix_ItemStyleCmd);

static int tableInited = 0;

void
DestroyDefaultTable(clientData,interp)
ClientData clientData;
Tcl_Interp *interp;
{
    Tcl_DeleteHashTable((Tcl_HashTable *) clientData);
    ckfree((char *) clientData);
}

void
DestroyStyleTable(clientData,interp)
ClientData clientData;
Tcl_Interp *interp;
{
    Tcl_DeleteHashTable((Tcl_HashTable *) clientData);
    ckfree((char *) clientData);
}

Tcl_HashTable *
GetDefaultTable(interp)
     Tcl_Interp *interp;
{
     Tcl_HashTable *table = (Tcl_HashTable *) Tcl_GetAssocData(interp, "TixDefaultStyle", NULL);
     if (table == NULL) {
	table  = (Tcl_HashTable *) ckalloc(sizeof(Tcl_HashTable));
	Tcl_InitHashTable(table, TCL_ONE_WORD_KEYS);
	Tcl_SetAssocData(interp, "TixDefaultStyle", DestroyDefaultTable,
		(ClientData) table);
     }
     return table;
}

void
InitHashTables _ANSI_ARGS_((void))
{
}

Tcl_HashTable *
GetStyleTable(interp)
     Tcl_Interp *interp;
{
     Tcl_HashTable *table = (Tcl_HashTable *) Tcl_GetAssocData(interp, "TixStyles", NULL);
     if (table == NULL) {
	table  = (Tcl_HashTable *) ckalloc(sizeof(Tcl_HashTable));
	Tcl_InitHashTable(table, TCL_STRING_KEYS);
	Tcl_SetAssocData(interp, "TixStyles", DestroyStyleTable,
		(ClientData) table);
     }
     return table;
}


/*
 *--------------------------------------------------------------
 *
 * TixDItemStyleFree --
 *
 *	When an item does not need a style anymore (when the item
 *	is destroyed, e.g.), it must call this procedute to free the
 *	style).
 *
 * Results:
 *	Nothing
 *
 * Side effects:
 *	The item is freed from the list of attached items in the style.
 *	Also, the style will be freed if it was already destroyed and
 *	it has no more items attached to it.
 *
 *--------------------------------------------------------------
 */
void TixDItemStyleFree(iPtr, stylePtr)
    Tix_DItem *iPtr;
    Tix_DItemStyle * stylePtr;
{
    ListDelete(stylePtr, iPtr);
}

/*
 *--------------------------------------------------------------
 *
 * Tix_ItemStyleCmd --
 *
 *	This procedure is invoked to process the "tixItemStyle" Tcl
 *	command.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	A new widget is created and configured.
 *
 *--------------------------------------------------------------
 */
int
Tix_ItemStyleCmd(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    Tix_DItemInfo * diTypePtr;
    Tk_Window tkwin = (Tk_Window)clientData;
    char * styleName = NULL;
    Tix_DispData dispData;
    char buff[100];
    int i, n;
    static int counter = 0;
    Tix_DItemStyle * stylePtr;

    if (argc < 2) {
	return Tix_ArgcError(interp, argc, argv, 1,
	    "itemtype ?option value ...");
    }

    if ((diTypePtr=Tix_GetDItemType(interp, argv[1])) == NULL) {
	return TCL_ERROR;
    }

    /*
     * Parse the -refwindow option: this tells the style to use this
     * window to query the default values for background, foreground
     * etc. Usually, you should set the -refwindow to the window that
     * holds the display items which are controlled by this style.
     */
    if (argc > 2) {
	size_t len;
	if (argc %2 != 0) {
	    Tcl_AppendResult(interp, "value for \"", argv[argc-1],
		"\" missing", NULL);
	    return TCL_ERROR;
	}
	for (n=i=2; i<argc; i+=2) {
	    len = strlen(argv[i]);
	    if (strncmp(argv[i], "-refwindow", len) == 0) {
		if ((tkwin=Tk_NameToWindow(interp,argv[i+1],tkwin)) == NULL) {
		    return TCL_ERROR;
		}
		continue;
	    }
	    if (strncmp(argv[i], "-stylename", len) == 0) {
		styleName = argv[i+1];
		if (FindStyle(styleName, interp) != NULL) {
		    Tcl_AppendResult(interp, "style \"", argv[i+1],
			"\" already exist", NULL);
		    return TCL_ERROR;
		}
		continue;
	    }

	    if (n!=i) {
#if 0
		objv[n]   = objv[i];
		objv[n+1] = objv[i+1];
#else
		Tcl_SetStringObj(objv[n],Tcl_GetString(objv[i]),-1);
		Tcl_SetStringObj(objv[n+1],Tcl_GetString(objv[i+1]),-1);
		/* XXX ??? return TCL_ERROR; */
#endif
	    }
	    n+=2;
	}
	argc = n;
    }

    if (styleName == NULL) {
	/*
	 * No name is given, we'll make a unique name by default
	 * (ToDo: check if the name has already been used)
	 */
	sprintf(buff, "tixStyle%d", counter++);
	styleName = buff;
    }

    dispData.interp  = interp;
    dispData.display = Tk_Display(tkwin);
    dispData.tkwin   = tkwin;

    if ((stylePtr = GetDItemStyle(&dispData, diTypePtr,
	 styleName, NULL)) == NULL) {
	return TCL_ERROR;
    }
    if (StyleConfigure(interp, stylePtr, argc-2, argv+2, 0) != TCL_OK) {
	DeleteStyle(stylePtr);
	return TCL_ERROR;
    }
    Tk_CreateEventHandler(tkwin, StructureNotifyMask,
	    RefWindowStructureProc, (ClientData)stylePtr);

    Tcl_ResetResult(interp);
    Tcl_SetObjResult(interp, LangObjectObj( interp, styleName));
    return TCL_OK;
}

static int
StyleCmd(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int code;

    static Tix_SubCmdInfo subCmdInfo[] = {
	{TIX_DEFAULT_LEN, "cget", 1, 1, StyleCGetCmd,
	   "option"},
	{TIX_DEFAULT_LEN, "configure", 0, TIX_VAR_ARGS, StyleConfigCmd,
	   "?option? ?value? ?option value ... ?"},
	{TIX_DEFAULT_LEN, "delete", 0, 0, StyleDeleteCmd,
	   ""},
    };

    static Tix_CmdInfo cmdInfo = {
	Tix_ArraySize(subCmdInfo), 1, TIX_VAR_ARGS, "?option? arg ?arg ...?",
    };

    Tcl_Preserve(clientData);
    code = Tix_HandleSubCmds(&cmdInfo, subCmdInfo, clientData,
	interp, argc, argv);
    Tcl_Release(clientData);

    return code;
}

/*----------------------------------------------------------------------
 * "cget" sub command
 *----------------------------------------------------------------------
 */
static int
StyleCGetCmd(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    Tix_DItemStyle* stylePtr= (Tix_DItemStyle*) clientData;

    return Tk_ConfigureValue(interp, stylePtr->base.tkwin,
	stylePtr->base.diTypePtr->styleConfigSpecs,
	(char *)stylePtr, argv[0], 0);
}

/*----------------------------------------------------------------------
 * "configure" sub command
 *----------------------------------------------------------------------
 */
static int
StyleConfigCmd(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    Tix_DItemStyle* stylePtr= (Tix_DItemStyle*) clientData;

    if (argc == 0) {
	return Tk_ConfigureInfo(interp, stylePtr->base.tkwin,
	    stylePtr->base.diTypePtr->styleConfigSpecs,
	    (char *)stylePtr, (char *) NULL, 0);
    } else if (argc == 1) {
	return Tk_ConfigureInfo(interp, stylePtr->base.tkwin,
	    stylePtr->base.diTypePtr->styleConfigSpecs,
	    (char *)stylePtr, argv[0], 0);
    } else {
	return StyleConfigure(interp, stylePtr, argc, argv,
	    TK_CONFIG_ARGV_ONLY);
    }
}

/*----------------------------------------------------------------------
 * "delete" sub command
 *----------------------------------------------------------------------
 */
static int
StyleDeleteCmd(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    Tix_DItemStyle* stylePtr= (Tix_DItemStyle*) clientData;

    if (stylePtr->base.flags & TIX_STYLE_DEFAULT) {
	Tcl_AppendResult(interp, "Cannot delete default item style",
	    NULL);
	return TCL_ERROR;
    }

    DeleteStyle(stylePtr);
    return TCL_OK;
}

static int
StyleConfigure(interp, stylePtr, argc, argv, flags)
    Tcl_Interp *interp;		/* Used for error reporting. */
    Tix_DItemStyle* stylePtr;	/* Information about the style;  may or may
				 * not already have values for some fields. */
    int argc;			/* Number of valid entries in argv. */
    char **argv;		/* Arguments. */
    int flags;			/* Flags to pass to Tk_ConfigureWidget. */
{
    Tix_DItemInfo * diTypePtr = stylePtr->base.diTypePtr;

    if (diTypePtr->styleConfigureProc(stylePtr, argc, argv, flags) != TCL_OK) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*----------------------------------------------------------------------
 * StyleDestroy --
 *
 *	Destroy a display style.
 *----------------------------------------------------------------------
 */
static void
StyleDestroy(clientData)
char *clientData;
{
    Tix_DItemStyle* stylePtr= (Tix_DItemStyle*) clientData;

    if ((stylePtr->base.flags & TIX_STYLE_DEFAULT)) {
	/*
	 * If this is the default style for the display items, we
	 * can't tell the display items that it has lost its style,
	 * otherwise the ditem will just attempt to create the default
	 * style again, and we will go into an infinite loop
	 */
	if (stylePtr->base.refCount != 0) {
	    /*
	     * If the refcount is not zero, this style will NOT be
	     * destroyed.  The real destroy will be triggered if all
	     * DItems associated with this style is destroyed (in the
	     * ListDelete() function).
	     *
	     * If a widget is destroyed, it is the responsibility of the
	     * widget writer to delete all DItems associated with this
	     * widget. We can discover memory leak if the widget is
	     * destroyed but some default styles associated with it still
	     * exist
	     */
	    return;
	}
    } else {
	stylePtr->base.refCount = 0;
    }

    Tcl_DeleteHashTable(&stylePtr->base.items);
    ckfree((char*)stylePtr->base.name);
    stylePtr->base.diTypePtr->styleFreeProc(stylePtr);
}

static void
StyleCmdDeletedProc(clientData)
    ClientData clientData;
{
    Tix_DItemStyle * stylePtr = (Tix_DItemStyle *)clientData;

    stylePtr->base.styleCmd = NULL;
    if (stylePtr->base.flags & TIX_STYLE_DEFAULT) {
	/*
	 * Don't do anything
	 * ToDo: maybe should give a background warning:
	 */
    } else {
	DeleteStyle(stylePtr);
    }
}

static void
DeleteStyle(stylePtr)
    Tix_DItemStyle * stylePtr;
{
    Tcl_HashEntry * hashPtr;

    if (!(stylePtr->base.flags & TIX_STYLE_DELETED)) {
	stylePtr->base.flags |= TIX_STYLE_DELETED;

	if (stylePtr->base.styleCmd != NULL) {
	    Tcl_DeleteCommand(stylePtr->base.interp,
	        Tcl_GetCommandName(stylePtr->base.interp,
	        stylePtr->base.styleCmd));
	}
	hashPtr=Tcl_FindHashEntry(GetStyleTable(stylePtr->base.interp), stylePtr->base.name);
	if (hashPtr != NULL) {
	    Tcl_DeleteHashEntry(hashPtr);
	}
	ListDeleteAll(stylePtr);

	Tcl_EventuallyFree((ClientData)stylePtr, StyleDestroy);
    }
}

/*
 *----------------------------------------------------------------------
 * FindDefaultStyle --
 *
 *	Return the default style of the given type of ditem for the
 *	given tkwin, if such a default style exists.
 *
 * Results:
 *	Pointer to the default style or NULL.
 *
 * Side effects:
 *	None.
 *----------------------------------------------------------------------
 */

static Tix_DItemStyle*
FindDefaultStyle(interp, diTypePtr, tkwin)
    Tcl_Interp *interp;
    Tix_DItemInfo * diTypePtr;
    Tk_Window tkwin;
{
    Tcl_HashEntry *hashPtr;
    StyleInfo * infoPtr;
    StyleLink * linkPtr;

    if ((hashPtr=Tcl_FindHashEntry(GetDefaultTable(interp), (char*)tkwin)) == NULL) {
	return NULL;
    }
    infoPtr = (StyleInfo *)Tcl_GetHashValue(hashPtr);
    for (linkPtr = infoPtr->linkHead; linkPtr; linkPtr=linkPtr->next) {
	if (linkPtr->diTypePtr == diTypePtr) {
	    return linkPtr->stylePtr;
	}
    }
    return NULL;
}

static void SetDefaultStyle(diTypePtr, tkwin, stylePtr)
    Tix_DItemInfo * diTypePtr;
    Tk_Window tkwin;
    Tix_DItemStyle * stylePtr;
{
    Tcl_HashEntry *hashPtr;
    StyleInfo * infoPtr;
    StyleLink * newPtr;
    int isNew;

    newPtr = (StyleLink *)ckalloc(sizeof(StyleLink));
    newPtr->diTypePtr = diTypePtr;
    newPtr->stylePtr  = stylePtr;

    hashPtr = Tcl_CreateHashEntry(GetDefaultTable(stylePtr->base.interp), (char*)tkwin, &isNew);

    if (!isNew) {
	infoPtr = (StyleInfo *)Tcl_GetHashValue(hashPtr);
	if (infoPtr->tmplPtr) {
	    if (diTypePtr->styleSetTemplateProc != NULL) {
		diTypePtr->styleSetTemplateProc(stylePtr,
		    infoPtr->tmplPtr);
	    }
	}
    } else {
	infoPtr = (StyleInfo *)ckalloc(sizeof(StyleInfo));
	infoPtr->linkHead = NULL;
	infoPtr->tmplPtr  = NULL;

	Tk_CreateEventHandler(tkwin, StructureNotifyMask,
	    DefWindowStructureProc, (ClientData)tkwin);
	Tcl_SetHashValue(hashPtr, (char*)infoPtr);
    }
    newPtr->next = infoPtr->linkHead;
    infoPtr->linkHead = newPtr;
}

/*
 *----------------------------------------------------------------------
 * TixGetDefaultDItemStyle --
 *
 *	Gets the default style for an item if the application doesn't
 *	explicitly give it an style with the -style switch.
 *
 * Results:
 *	The default style.
 *
 * Side effects:
 *
 *----------------------------------------------------------------------
 */

Tix_DItemStyle*
TixGetDefaultDItemStyle(ddPtr, diTypePtr, iPtr, oldStylePtr)
    Tix_DispData * ddPtr;		/* Info about the display. */
    Tix_DItemInfo * diTypePtr;		/* Info about the DItem type. */
    Tix_DItem *iPtr;			/* Get default style for this DItem. */
    Tix_DItemStyle* oldStylePtr;	/* ?? */
{
    Tcl_DString dString;
    Tix_DItemStyle* stylePtr;
    int isNew;

    stylePtr = FindDefaultStyle(ddPtr->interp, diTypePtr, ddPtr->tkwin);
    if (stylePtr == NULL) {
	/*
	 * Format default name for this style+window
	 */
	Tcl_DStringInit(&dString);
	Tcl_DStringAppend(&dString, "style", 5);
	if (ddPtr->tkwin) {
	    Tcl_DStringAppend(&dString, Tk_PathName(ddPtr->tkwin),
		strlen(Tk_PathName(ddPtr->tkwin)));
	}
	Tcl_DStringAppend(&dString, ":", 1);
	Tcl_DStringAppend(&dString, diTypePtr->name, strlen(diTypePtr->name));

	/*
	 * Create the new style
	 */
	stylePtr = GetDItemStyle(ddPtr, diTypePtr, Tcl_DStringValue(&dString), &isNew);
	if (isNew) {
	    diTypePtr->styleConfigureProc(stylePtr, 0, NULL, 0);
	    stylePtr->base.flags |= TIX_STYLE_DEFAULT;
	}

	SetDefaultStyle(diTypePtr, ddPtr->tkwin, stylePtr);
	Tcl_DStringFree(&dString);
    }

    if (oldStylePtr) {
	ListDelete(oldStylePtr, iPtr);
    }
    if (stylePtr) {
	ListAdd(stylePtr, iPtr);
    }

    return stylePtr;
}

void Tix_SetDefaultStyleTemplate(tkwin, tmplPtr)
    Tk_Window tkwin;
    Tix_StyleTemplate * tmplPtr;
{
    TkWindow *winPtr = (TkWindow *) tkwin;
    Tcl_Interp *interp = winPtr->mainPtr->interp;
    Tcl_HashEntry * hashPtr;
    StyleInfo * infoPtr;
    StyleLink * linkPtr;
    int isNew;

    hashPtr=Tcl_CreateHashEntry(GetDefaultTable(interp), (char*)tkwin, &isNew);
    if (!isNew) {
	infoPtr = (StyleInfo *)Tcl_GetHashValue(hashPtr);
	infoPtr->tmplPtr = &infoPtr->tmpl;
	infoPtr->tmpl = *tmplPtr;

	for (linkPtr = infoPtr->linkHead; linkPtr; linkPtr=linkPtr->next) {
	    if (linkPtr->diTypePtr->styleSetTemplateProc != NULL) {
		linkPtr->diTypePtr->styleSetTemplateProc(linkPtr->stylePtr,
		    tmplPtr);
	    }
	}
    } else {
	infoPtr = (StyleInfo *)ckalloc(sizeof(StyleInfo));
	infoPtr->linkHead = NULL;
	infoPtr->tmplPtr = &infoPtr->tmpl;
	infoPtr->tmpl = *tmplPtr;

	Tk_CreateEventHandler(tkwin, StructureNotifyMask,
	    DefWindowStructureProc, (ClientData)tkwin);
	Tcl_SetHashValue(hashPtr, (char*)infoPtr);
    }
}

/*
 *----------------------------------------------------------------------
 * GetDItemStyle --
 *
 *	Returns an ItemStyle with the given name.
 *
 * Results:
 *	Pointer to the given Tix_DItsmStyle.
 *
 * Side effects:
 *	If the style doesn't already exist, it is allocated.
 *----------------------------------------------------------------------
 */

static Tix_DItemStyle*
GetDItemStyle(ddPtr, diTypePtr, styleName, isNew_ret)
    Tix_DispData * ddPtr;
    Tix_DItemInfo * diTypePtr;
    char * styleName;
    int * isNew_ret;
{
    Tcl_HashEntry *hashPtr;
    int isNew;
    Tix_DItemStyle * stylePtr;

    if (tableInited == 0) {
	InitHashTables();
    }

    hashPtr = Tcl_CreateHashEntry(GetStyleTable(ddPtr->interp), styleName, &isNew);
    if (!isNew) {
	stylePtr = (Tix_DItemStyle *)Tcl_GetHashValue(hashPtr);
    }
    else {
	stylePtr = diTypePtr->styleCreateProc(ddPtr->interp,
	    ddPtr->tkwin, diTypePtr, styleName);
	stylePtr->base.styleCmd = Lang_CreateObject(ddPtr->interp,
	    styleName, StyleCmd, (ClientData)stylePtr, StyleCmdDeletedProc);
	stylePtr->base.interp 	 = ddPtr->interp;
	stylePtr->base.tkwin  	 = ddPtr->tkwin;
	stylePtr->base.diTypePtr = diTypePtr;
	stylePtr->base.name      = tixStrDup(styleName);
	stylePtr->base.pad[0] 	 = 0;
	stylePtr->base.pad[1] 	 = 0;
	stylePtr->base.anchor 	 = TK_ANCHOR_CENTER;
	stylePtr->base.refCount  = 0;
	stylePtr->base.flags     = 0;
	Tcl_InitHashTable(&stylePtr->base.items, TCL_ONE_WORD_KEYS);

	Tcl_SetHashValue(hashPtr, (char*)stylePtr);
    }

    if (isNew_ret != NULL) {
	* isNew_ret = isNew;
    }
    return stylePtr;
}

static Tix_DItemStyle* FindStyle(styleName, interp)
    char *styleName;
    Tcl_Interp *interp;		/* Current interpreter. */
{
    Tcl_HashEntry *hashPtr;

    if (tableInited == 0) {
	InitHashTables();
    }
    if ((hashPtr=Tcl_FindHashEntry(GetStyleTable(interp), styleName)) == NULL) {
	return NULL;
    }

    return (Tix_DItemStyle *)Tcl_GetHashValue(hashPtr);
}

/*----------------------------------------------------------------------
 * TixDItemStyleChanged --
 *
 *	Tell each Ditem that are affected by this style that the style
 *	has changed. The Ditems will respond by updating their
 *	attributes according to the new values of the style.
 *----------------------------------------------------------------------
 */

void TixDItemStyleChanged(diTypePtr, stylePtr)
    Tix_DItemInfo * diTypePtr;
    Tix_DItemStyle * stylePtr;
{
    Tcl_HashSearch hashSearch;
    Tcl_HashEntry *hashPtr;
    Tix_DItem * iPtr;

    for (hashPtr = Tcl_FirstHashEntry(&stylePtr->base.items, &hashSearch);
	 hashPtr;
	 hashPtr = Tcl_NextHashEntry(&hashSearch)) {

	iPtr = (Tix_DItem *)Tcl_GetHashValue(hashPtr);
	diTypePtr->styleChangedProc(iPtr);
    }
}

/*----------------------------------------------------------------------
 * ListAdd --
 *
 *	Add an item to the list of items affected by a style.
 *----------------------------------------------------------------------
 */

static void
ListAdd(stylePtr, iPtr)
    Tix_DItemStyle * stylePtr;
    Tix_DItem *iPtr;
{
    Tcl_HashEntry *hashPtr;
    int isNew;

    hashPtr = Tcl_CreateHashEntry(&stylePtr->base.items, (char*)iPtr, &isNew);
    if (!isNew) {
	panic("DItem is already associated with style");
    } else {
	Tcl_SetHashValue(hashPtr, (char*)iPtr);
    }
    ++ stylePtr->base.refCount;
}

static void
ListDelete(stylePtr, iPtr)
    Tix_DItemStyle * stylePtr;
    Tix_DItem *iPtr;
{
    Tcl_HashEntry *hashPtr;

    hashPtr = Tcl_FindHashEntry(&stylePtr->base.items, (char*)iPtr);
    if (hashPtr == NULL) {
	panic("DItem is not associated with style");
    }
    Tcl_DeleteHashEntry(hashPtr);
    stylePtr->base.refCount--;

    if ((stylePtr->base.refCount == 0) &&
            (stylePtr->base.flags & TIX_STYLE_DELETED) &&
	    (stylePtr->base.flags & TIX_STYLE_DEFAULT)) {
	Tcl_EventuallyFree((ClientData)stylePtr, (Tix_FreeProc *)StyleDestroy);
    }
}

static void
ListDeleteAll(stylePtr)
    Tix_DItemStyle * stylePtr;
{
    Tcl_HashSearch hashSearch;
    Tcl_HashEntry *hashPtr;
    Tix_DItem * iPtr;

    for (hashPtr = Tcl_FirstHashEntry(&stylePtr->base.items, &hashSearch);
	 hashPtr;
	 hashPtr = Tcl_NextHashEntry(&hashSearch)) {

	iPtr = (Tix_DItem *)Tcl_GetHashValue(hashPtr);
	if (stylePtr->base.diTypePtr->lostStyleProc != NULL) {
	    stylePtr->base.diTypePtr->lostStyleProc(iPtr);
	}
	Tcl_DeleteHashEntry(hashPtr);
    }
}
/*
 *--------------------------------------------------------------
 *
 * DefWindowStructureProc --
 *
 *	This procedure is invoked whenever StructureNotify events
 *	occur for a window that has some default style(s) associated with it
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The style(s) associated with this window will all be deleted.
 *
 *--------------------------------------------------------------
 */

static void
DefWindowStructureProc(clientData, eventPtr)
    ClientData clientData;	/* Pointer to record describing window item. */
    XEvent *eventPtr;		/* Describes what just happened. */
{
    Tk_Window tkwin = (Tk_Window)clientData;
    TkWindow *winPtr = (TkWindow *) tkwin;
    Tcl_Interp *interp = winPtr->mainPtr->interp;
    Tcl_HashEntry *hashPtr;
    StyleInfo * infoPtr;
    StyleLink * linkPtr, *toFree;

    if (eventPtr->type != DestroyNotify) {
	return;
    }
    if ((hashPtr=Tcl_FindHashEntry(GetDefaultTable(interp), (char*)tkwin)) == NULL) {
	return;
    }
    infoPtr = (StyleInfo *)Tcl_GetHashValue(hashPtr);
    for (linkPtr = infoPtr->linkHead; linkPtr; ) {
	toFree = linkPtr;
	linkPtr=linkPtr->next;

	DeleteStyle(toFree->stylePtr);
	ckfree((char*)toFree);
    }

    ckfree((char*)infoPtr);
    Tcl_DeleteHashEntry(hashPtr);
}

/*
 *--------------------------------------------------------------
 *
 * RefWindowStructureProc --
 *
 *	This procedure is invoked when the refwindow of a non-default
 *	style is deleted.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The style is deleted.
 *
 *--------------------------------------------------------------
 */

static void
RefWindowStructureProc(clientData, eventPtr)
    ClientData clientData;	/* Pointer to record describing window item. */
    XEvent *eventPtr;		/* Describes what just happened. */
{
    Tix_DItemStyle * stylePtr = (Tix_DItemStyle *)clientData;

    if (eventPtr->type == DestroyNotify) {
	/*
	 * If some DItems are still associated with this window, they
	 * will receive a "LostStyle" notification.
	 */
	DeleteStyle(stylePtr);
    }
}

/*----------------------------------------------------------------------
 *
 *		 The Tix Customed Config Options
 *
 *----------------------------------------------------------------------
 */

/*
 * The global data structures to use in widget configSpecs arrays
 *
 * These are declared in <tix.h>
 */

#ifndef _LANG
Tk_CustomOption tixConfigItemStyle = {
    DItemStyleParseProc, DItemStylePrintProc, 0,
};
#endif

/*----------------------------------------------------------------------
 *  DItemStyleParseProc --
 *
 *	Parse the text string and store the Tix_DItemStyleType information
 *	inside the widget record.
 *----------------------------------------------------------------------
 */
LINKAGE int DItemStyleParseProc(clientData, interp, tkwin, value, widRec,offset)
    ClientData clientData;
    Tcl_Interp *interp;
    Tk_Window tkwin;
    Tcl_Obj * value;
    char *widRec;		/* Must point to a valid Tix_DItem struct */
    int offset;
{
    Tix_DItem       * iPtr = (Tix_DItem *)widRec;
    Tix_DItemStyle ** ptr = (Tix_DItemStyle **)(widRec + offset);
    Tix_DItemStyle  * oldPtr = *ptr;
    Tix_DItemStyle  * newPtr;

    if (value == NULL || strlen(Tcl_GetString(value)) == 0) {
	/*
	 * User gives a NULL string -- meaning he wants the default
	 * style
	 */
	if (oldPtr && oldPtr->base.flags & TIX_STYLE_DEFAULT) {
	    /*
	     * This ditem is already associated with a default style. Let's
	     * keep it.
	     */
	    newPtr = oldPtr;
	} else {
	    if (oldPtr) {
		ListDelete(oldPtr, iPtr);
	    }
	    newPtr = NULL;
	}
    } else {
	if ((newPtr = FindStyle(Tcl_GetString(value), interp)) == NULL) {
	    goto not_found;
	}
	if (newPtr->base.flags & TIX_STYLE_DELETED) {
	    goto not_found;
	}
	if (newPtr->base.diTypePtr != iPtr->base.diTypePtr) {
	    Tcl_AppendResult(interp, "Style type mismatch ",
	        "Needed ", iPtr->base.diTypePtr->name, " style but got ",
	        newPtr->base.diTypePtr->name, " style", NULL);
	    return TCL_ERROR;
	}
	if (oldPtr != newPtr) {
	    if (oldPtr != NULL) {
		ListDelete(oldPtr, iPtr);
	    }
	    ListAdd(newPtr, iPtr);
	}
    }

    *ptr = newPtr;
    return TCL_OK;

not_found:
    Tcl_AppendResult(interp, "Display style \"", value,
	"\" not found", NULL);
    return TCL_ERROR;
}

LINKAGE Tcl_Obj *
DItemStylePrintProc(clientData, tkwin, widRec,offset, freeProcPtr)
    ClientData clientData;
    Tk_Window tkwin;
    char *widRec;
    int offset;
    Tcl_FreeProc **freeProcPtr;
{
    Tix_DItemStyle *stylePtr = *((Tix_DItemStyle**)(widRec+offset));
    Tcl_Obj * result = NULL;
    if (stylePtr != NULL && !(stylePtr->base.flags & TIX_STYLE_DEFAULT)) {
        LangSetObj(&result,LangObjectObj(stylePtr->base.interp,
                                         stylePtr->base.name));
    }
    return result;
}


