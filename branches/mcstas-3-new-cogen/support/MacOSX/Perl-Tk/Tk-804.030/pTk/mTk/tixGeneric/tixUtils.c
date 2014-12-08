
/*	$Id: tixUtils.c,v 1.3 2000/10/12 04:06:58 idiscovery Exp $	*/

/*
 * tixUtils.c --
 *
 *	This file contains some utility functions for Tix, such as the
 *	subcommand handling functions and option handling functions.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */
#include "tixPort.h"
#include "tixInt.h"

/*
 * Forward declarations for procedures defined later in this file:
 */

static void		Prompt _ANSI_ARGS_((Tcl_Interp *interp, int partial));
static void		StdinProc _ANSI_ARGS_((ClientData clientData,
			    int mask));
static int		 ReliefParseProc _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp,
			    Tk_Window tkwin,
			    Tcl_Obj * avalue,
			    char *widRec,
			    int offset));
static Tcl_Obj *		 ReliefPrintProc _ANSI_ARGS_((ClientData clientData,
			    Tk_Window tkwin,
			    char *widRec,
			    int offset,
			    Tcl_FreeProc **freeProcPtr));
/*
 * Global vars used in this file
 */
static Tcl_DString command;	/* Used to assemble lines of terminal input
				 * into Tcl commands. */


#define WRONG_ARGC 1
#define NO_MATCH   2


#ifndef _LANG
/*----------------------------------------------------------------------
 * TixSaveInterpState --
 *
 *	Save the current application-visible state of the interpreter.
 *	This can later be restored by the TixSaveInterpState() function.
 *	These two functions are useful if you want to evaluate a Tcl
 *	command, which may cause errors, inside a command function.
 *
 *	Each TixSaveInterpState() call much be matched by one
 *	TixRestoreInterpState() call with the same statePtr. statePtr
 *	should be allocated by the calling function, usually
 *	as a variable on the stack.
 *----------------------------------------------------------------------
 */

void
TixSaveInterpState(interp, statePtr)
    Tcl_Interp * interp;
    TixInterpState * statePtr;
{
    char * p;
    if (interp->result) {
	statePtr->result = tixStrDup(interp->result);
    } else {
	statePtr->result = NULL;
    }

    p = Tcl_GetVar2(interp, "errorInfo", NULL, TCL_GLOBAL_ONLY);
    if (p) {
	statePtr->errorInfo = tixStrDup(p);
    } else {
	statePtr->errorInfo = NULL;
    }

    p = Tcl_GetVar2(interp, "errorCode", NULL, TCL_GLOBAL_ONLY);
    if (p) {
	statePtr->errorCode = tixStrDup(p);
    } else {
	statePtr->errorCode = NULL;
    }
}

/*----------------------------------------------------------------------
 * TixRestoreInterpState --
 *
 *	See TixSaveInterpState above.
 *----------------------------------------------------------------------
 */

void
TixRestoreInterpState(interp, statePtr)
    Tcl_Interp * interp;
    TixInterpState * statePtr;
{
    if (statePtr->result) {
	Tcl_SetResult(interp, statePtr->result, TCL_DYNAMIC);
    }
    if (statePtr->errorInfo) {
	Tcl_SetVar2(interp, "errorInfo", NULL, statePtr->errorInfo,
		TCL_GLOBAL_ONLY);
	ckfree((char*)statePtr->errorInfo);
    } else {
	Tcl_UnsetVar2(interp, "errorInfo", NULL, TCL_GLOBAL_ONLY);
    }
    if (statePtr->errorCode) {
	Tcl_SetVar2(interp, "errorCode", NULL, statePtr->errorCode,
		TCL_GLOBAL_ONLY);
	ckfree((char*)statePtr->errorCode);
    } else {
	Tcl_UnsetVar2(interp, "errorCode", NULL, TCL_GLOBAL_ONLY);
    }
}
#endif

/*----------------------------------------------------------------------
 * Tix_HandleSubCmds --
 *
 *	This function makes it easier to write major-minor style TCL
 *	commands.  It matches the minor command (sub-command) names
 *	with names defined in the cmdInfo structure and call the
 *	appropriate sub-command functions for you. This function will
 *	automatically generate error messages when the user calls an
 *	invalid sub-command or calls a sub-command with incorrect
 *	number of arguments.
 *
 *----------------------------------------------------------------------
 */

int Tix_HandleSubCmds(cmdInfo, subCmdInfo, clientData, interp, argc, argv)
    Tix_CmdInfo * cmdInfo;
    Tix_SubCmdInfo * subCmdInfo;
    ClientData clientData;	/* Main window associated with
				 * interpreter. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{

    int i;
    int len;
    int error = NO_MATCH;
    Tix_SubCmdInfo * s;

    /*
     * First check if the number of arguments to the major command
     * is correct
     */
    argc -= 1;
    if (argc < cmdInfo->minargc ||
	(cmdInfo->maxargc != TIX_VAR_ARGS && argc > cmdInfo->maxargc)) {

	Tcl_AppendResult(interp, "wrong # args: should be \"",
	    argv[0], " ", cmdInfo->info, "\".", (char *) NULL);

	return TCL_ERROR;
    }

    /*
     * Now try to match the subcommands with argv[1]
     */
    argc -= 1;
    len = strlen(argv[1]);

    for (i = 0, s = subCmdInfo; i < cmdInfo->numSubCmds; i++, s++) {
	if (s->name == TIX_DEFAULT_SUBCMD) {
	    if (s->checkArgvProc) {
	      if (!((*s->checkArgvProc)(clientData, interp, argc+1, argv+1))) {
		    /* Some improper argv in the arguments of the default
		     * subcommand
		     */
		    break;
		}
	    }
	    return (*s->proc)(clientData, interp, argc+1, argv+1);
	}

	if (s->namelen == TIX_DEFAULT_LEN) {
	    s->namelen = strlen(s->name);
	}
	if (s->name[0] == argv[1][0] && strncmp(argv[1],s->name,len)==0) {
	    if (argc < s->minargc) {
		error = WRONG_ARGC;
		break;
	    }

	    if (s->maxargc != TIX_VAR_ARGS &&
		argc > s->maxargc) {
		error = WRONG_ARGC;
		break;
	    }

	    /*
	     * Here we have a matched argc and command name --> go for it!
	     */
	    return (*s->proc)(clientData, interp, argc, argv+2);
	}
    }

    if (error == WRONG_ARGC) {
	/*
	 * got a match but incorrect number of arguments
	 */
	Tcl_AppendResult(interp, "wrong # args: should be \"",
	    argv[0], " ", argv[1], " ", s->info, "\"", (char *) NULL);
    } else {
	int max;

	/*
	 * no match: let print out all the options
	 */
	Tcl_AppendResult(interp, "unknown option \"",
	    argv[1], "\".",  (char *) NULL);

	if (cmdInfo->numSubCmds == 0) {
	    max = 0;
	} else {
	    if (subCmdInfo[cmdInfo->numSubCmds-1].name == TIX_DEFAULT_SUBCMD) {
		max = cmdInfo->numSubCmds-1;
	    } else {
		max = cmdInfo->numSubCmds;
	    }
	}

	if (max == 0) {
	    Tcl_AppendResult(interp,
		" This command does not take any options.",
		(char *) NULL);
	} else if (max == 1) {
	    Tcl_AppendResult(interp,
		" Must be ", subCmdInfo->name, ".", (char *)NULL);
	} else {
	    Tcl_AppendResult(interp, " Must be ", (char *) NULL);

	    for (i = 0, s = subCmdInfo; i < max; i++, s++) {
		if (i == max-1) {
		    Tcl_AppendResult(interp,"or ",s->name, ".", (char *) NULL);
		} else if (i == max-2) {
		    Tcl_AppendResult(interp, s->name, " ", (char *) NULL);
		} else {
		    Tcl_AppendResult(interp, s->name, ", ", (char *) NULL);
		}
	    }
	}
    }
    return TCL_ERROR;
}

#ifndef _LANG

/*----------------------------------------------------------------------
 * Tix_Exit --
 *
 *	Call the "exit" tcl command so that things can be cleaned up
 *	before calling the unix exit(2);
 *
 *----------------------------------------------------------------------
 */
void Tix_Exit(interp, code)
    Tcl_Interp* interp;
    int code;
{
    if (code != 0 && interp && interp->result != 0) {
	fprintf(stderr, "%s\n", interp->result);
	fprintf(stderr, "%s\n",
	    Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY));
    }

    if (interp) {
	Tcl_GlobalEval(interp, tixStrDup("exit"));
    }
    exit(code);
}

/*
 *----------------------------------------------------------------------
 *
 * Tix_LoadTclLibrary --
 *
 *	Loads in a TCL library for an application according to
 *	the library settings.
 *
 * Results:
 *	TCL_OK or TCL_ERROR
 *
 * envName	the environment variable that indicates the library
 * tclName	the TCL variable that points to the TCL library.
 * initFile	the file to load in during initialization.
 * defDir	the default directory to search if the user hasn't set
 *		the environment variable.
 * appName	the name of the application.
 *----------------------------------------------------------------------
 */

/* Some compilers can't handle multi-line character strings very well ...
 * So I just using this big lump of mess here.
 */

static char _format[] = "lappend auto_path $%s \nif [file exists $%s/%s] {\nsource $%s/%s\n} else {\nset msg \"\ncan't find $%s/%s;\\nperhaps you \"\nappend msg \"need to install %s\\nor set your %s \"\nappend msg \"environment variable?\"\nerror $msg\n}";

int
Tix_LoadTclLibrary(interp, envName, tclName, initFile, defDir, appName)
    Tcl_Interp *interp;
    char *envName;
    char *tclName;
    char *initFile;
    char *defDir;
    char *appName;
{
    char * libDir, *initCmd;
    size_t size;
    int code;
    char *format;
    format = _format;

    libDir = getenv(envName);
    if (libDir == NULL) {
	libDir = defDir;
    }

    /*
     * This size should be big enough.
     */

    size = strlen(format) + strlen(tclName)*4 + strlen(initFile)*3
	+ strlen(appName) + strlen(envName) + 100;
    initCmd = ckalloc(sizeof(char) * size);

    Tcl_SetVar(interp, tclName, libDir, TCL_GLOBAL_ONLY);

    sprintf(initCmd, format,
	tclName,
	tclName, initFile,
	tclName, initFile,
	tclName, initFile,
	appName, envName
    );

    code =  Tcl_GlobalEval(interp, initCmd);
    ckfree(initCmd);
    return code;
}

/*----------------------------------------------------------------------
 * Tix_CreateCommands --
 *
 *
 *	Creates a list of commands stored in the array "commands"
 *----------------------------------------------------------------------
 */

static int initialized = 0;

void Tix_CreateCommands(interp, commands, clientData, deleteProc)
    Tcl_Interp *interp;
    Tix_TclCmd *commands;
    ClientData clientData;
    Tcl_CmdDeleteProc *deleteProc;
{
    Tix_TclCmd * cmdPtr;

    if (!initialized) {
	char *version = Tcl_PkgRequire(interp, "Tcl", NULL, 0);
	initialized = 1;
	if (version[0] == '8') {
	    struct CmdInfo {
		int isNativeObjectProc;
		Tcl_ObjCmdProc *objProc;
		ClientData objClientData;
		VOID *dummy[10]; /* worst case space that could be written
				  * by Tcl_GetCommandInfo() */
	    } cmdInfo;
	    if (!Tcl_GetCommandInfo(interp,"image", (Tcl_CmdInfo *) &cmdInfo)) {
		panic("cannot find the \"image\" command");
	    } else if (cmdInfo.isNativeObjectProc == 1) {
		initialized = 2; /* we use objects */
	    }
	}
    }
    for (cmdPtr = commands; cmdPtr->name != NULL; cmdPtr++) {
	Tcl_CreateCommand(interp, cmdPtr->name,
	     cmdPtr->cmdProc, clientData, deleteProc);
    }
}

#endif


/*----------------------------------------------------------------------
 * Tix_DrawAnchorLines --
 *
 * 	Draw dotted anchor lines around anchor elements
 *----------------------------------------------------------------------
 */

void Tix_DrawAnchorLines(display, drawable, gc, x, y, w, h)
    Display *display;
    Drawable drawable;
    GC gc;
    int x;
    int y;
    int w;
    int h;
{
    TixpDrawAnchorLines(display, drawable, gc, x, y, w, h);
}

/*----------------------------------------------------------------------
 * Tix_CreateSubWindow --
 *
 *	Creates a subwindow for a widget (usually used to draw headers,
 *	e.g, HList and Grid widgets)
 *----------------------------------------------------------------------
 */

Tk_Window
Tix_CreateSubWindow(interp, tkwin, subPath)
    Tcl_Interp * interp;
    Tk_Window tkwin;
    char * subPath;
{
    Tcl_DString dString;
    Tk_Window subwin;

    Tcl_DStringInit(&dString);
    Tcl_DStringAppend(&dString, Tk_PathName(tkwin),
	strlen(Tk_PathName(tkwin)));
    Tcl_DStringAppend(&dString, ".tixsw:", 7);
    Tcl_DStringAppend(&dString, subPath, strlen(subPath));

    subwin = Tk_CreateWindowFromPath(interp, tkwin, Tcl_DStringValue(&dString),
	(char *) NULL);

    Tcl_DStringFree(&dString);

    return subwin;
}

/*----------------------------------------------------------------------
 * Tix_GetRenderBuffer --
 *
 *	Returns a drawable for rendering a widget. If there is sufficient
 *	resource, a pixmap is returned so that double-buffering can
 *	be done. However, if resource is insufficient, then the
 *	windowId is returned. In the second case happens, the caller
 *	of this function has two choices: (1) draw to the window directly
 *	(which may lead to flashing on the screen) or (2) try to allocate
 *	smaller pixmaps.
 *----------------------------------------------------------------------
 */

static int
ErrorProc(clientData, errorEventPtr)
    ClientData clientData;
    XErrorEvent *errorEventPtr;		/* unused */
{
    int * badAllocPtr = (int*) clientData;

    * badAllocPtr = 1;
    return 0;				/* return 0 means error has been
					 * handled properly */
}

Drawable Tix_GetRenderBuffer(display, windowId, width, height, depth)
    Display *display;
    Window windowId;
    int width;
    int height;
    int depth;
{
    Tk_ErrorHandler handler;
    Pixmap pixmap;
    int badAlloc = 0;

    handler= Tk_CreateErrorHandler(display, BadAlloc,
	-1, -1, (Tk_ErrorProc *) ErrorProc, (ClientData) &badAlloc);
    pixmap = Tk_GetPixmap(display, windowId, width, height, depth);

#ifndef _WINDOWS
    /*
     * This XSync call is necessary because X may delay the delivery of the
     * error message. This will make our graphics a bit slower, though,
     * especially over slow lines
     */
    XSync(display, 0);
#endif
    /* If ErrorProc() is eevr called, it is called before XSync returns */

    Tk_DeleteErrorHandler(handler);

    if (!badAlloc) {
	return pixmap;
    } else {
	return windowId;
    }
}

#ifndef _LANG
/*
 *----------------------------------------------------------------------
 *
 * Tix_GlobalVarEval --
 *
 *	Given a variable number of string arguments, concatenate them
 *	all together and execute the result as a Tcl command in the global
 *	scope.
 *
 * Results:
 *	A standard Tcl return result.  An error message or other
 *	result may be left in interp->result.
 *
 * Side effects:
 *	Depends on what was done by the command.
 *
 *----------------------------------------------------------------------
 */
	/* VARARGS2 */ /* ARGSUSED */
int
#ifdef TCL_VARARGS_DEF
Tix_GlobalVarEval TCL_VARARGS_DEF(Tcl_Interp *,arg1)
#else
#ifndef lint
Tix_GlobalVarEval(va_alist)
#else
Tix_GlobalVarEval(iPtr, p, va_alist)
    Tcl_Interp *iPtr;		/* Interpreter in which to execute command. */
    char *p;			/* One or more strings to concatenate,
				 * terminated with a NULL string. */
#endif
    va_dcl
#endif
{
    va_list argList;
    Tcl_DString buf;
    char *string;
    Tcl_Interp *interp;
    int result;

#ifdef TCL_VARARGS_DEF
    /*
     * Copy the strings one after the other into a single larger
     * string.  Use stack-allocated space for small commands, but if
     * the command gets too large than call ckalloc to create the
     * space.
     */

    interp = TCL_VARARGS_START(Tcl_Interp *,arg1,argList);
    Tcl_DStringInit(&buf);
    while (1) {
	string = va_arg(argList, char *);
	if (string == NULL) {
	    break;
	}
	Tcl_DStringAppend(&buf, string, -1);
    }
    va_end(argList);

    result = Tcl_GlobalEval(interp, Tcl_DStringValue(&buf));
    Tcl_DStringFree(&buf);
    return result;
#else
    va_start(argList);
    interp = va_arg(argList, Tcl_Interp *);
    Tcl_DStringInit(&buf);
    while (1) {
	string = va_arg(argList, char *);
	if (string == NULL) {
	    break;
	}
	Tcl_DStringAppend(&buf, string, -1);
    }
    va_end(argList);

    result = Tcl_GlobalEval(interp, Tcl_DStringValue(&buf));
    Tcl_DStringFree(&buf);
    return result;
#endif
}
#endif

/*----------------------------------------------------------------------
 * TixGetHashTable --
 *
 *	This functions makes it possible to keep one hash table per
 *	interpreter. This way, Tix classes can be used in multiple
 *	interpreters.
 *
 *----------------------------------------------------------------------
 */

#ifdef TK_4_1_OR_LATER

static void		DeleteHashTableProc _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp * interp));
static void
DeleteHashTableProc(clientData, interp)
    ClientData clientData;
    Tcl_Interp * interp;
{
    Tcl_HashTable * htPtr = (Tcl_HashTable *)clientData;
    Tcl_HashSearch hashSearch;
    Tcl_HashEntry * hashPtr;

    for (hashPtr = Tcl_FirstHashEntry(htPtr, &hashSearch);
	    hashPtr;
	    hashPtr = Tcl_NextHashEntry(&hashSearch)) {
	Tcl_DeleteHashEntry(hashPtr);
    }

    Tcl_DeleteHashTable(htPtr);
    ckfree((char*)htPtr);
}

Tcl_HashTable *
TixGetHashTable(interp, name, deleteProc)
    Tcl_Interp * interp;
    char * name;
    Tcl_InterpDeleteProc *deleteProc;
{
    Tcl_HashTable * htPtr;

    htPtr = (Tcl_HashTable*)Tcl_GetAssocData(interp, name, NULL);
    if (htPtr == NULL) {
	htPtr = (Tcl_HashTable *)ckalloc(sizeof(Tcl_HashTable));
	Tcl_InitHashTable(htPtr, TCL_STRING_KEYS);
	Tcl_SetAssocData(interp, name, NULL, (ClientData)htPtr);
	if (deleteProc) {
	    Tcl_CallWhenDeleted(interp, deleteProc, (ClientData)htPtr);
	} else {
	    Tcl_CallWhenDeleted(interp, DeleteHashTableProc,
		    (ClientData)htPtr);
	}
    }

    return htPtr;
}

#else

Tcl_HashTable *
TixGetHashTable(interp, name)
    Tcl_Interp * interp;	/* Current interpreter. */
    char * name;		/* Textual name of the hash table. */
{
    static int inited = 0;
    static Tcl_HashTable classTable;
    static Tcl_HashTable methodTable;
    static Tcl_HashTable specTable;

    if (!inited) {
	Tcl_InitHashTable(&classTable, TCL_STRING_KEYS);
	Tcl_InitHashTable(&methodTable, TCL_STRING_KEYS);
	Tcl_InitHashTable(&specTable, TCL_STRING_KEYS);
	inited = 1;
    }

    if (strcmp(name, "tixClassTab") == 0) {
	return &classTable;
    } else if (strcmp(name, "tixSpecTab") == 0) {
	return &specTable;
    } else if (strcmp(name, "tixMethodTab") == 0) {
	return &methodTable;
    } else {
	panic("Unknown hash table %s", name);
    }
}
#endif

/*----------------------------------------------------------------------
 *
 *		 The Tix Customed Config Options
 *
 *----------------------------------------------------------------------
 */

/*----------------------------------------------------------------------
 *  ReliefParseProc --
 *
 *	Parse the text string and store the Tix_Relief information
 *	inside the widget record.
 *----------------------------------------------------------------------
 */
static int ReliefParseProc(clientData, interp, tkwin, avalue, widRec,offset)
    ClientData clientData;
    Tcl_Interp *interp;
    Tk_Window tkwin;
    Tcl_Obj * avalue;
    char *widRec;		/* Must point to a valid Tix_DItem struct */
    int offset;
{
    Tix_Relief * ptr = (Tix_Relief *)(widRec + offset);
    Tix_Relief   newVal;
    char *value = Tcl_GetString(avalue);

    if (value != NULL) {
	size_t len = strlen(value);

	if (strncmp(value, "raised", len) == 0) {
	    newVal = TIX_RELIEF_RAISED;
	} else if (strncmp(value, "flat", len) == 0) {
	    newVal = TIX_RELIEF_FLAT;
	} else if (strncmp(value, "sunken", len) == 0) {
	    newVal = TIX_RELIEF_SUNKEN;
	} else if (strncmp(value, "groove", len) == 0) {
	    newVal = TIX_RELIEF_GROOVE;
	} else if (strncmp(value, "ridge", len) == 0) {
	    newVal = TIX_RELIEF_RIDGE;
	} else if (strncmp(value, "solid", len) == 0) {
	    newVal = TIX_RELIEF_SOLID;
	} else {
	    goto error;
	}
    } else {
	value = "";
	goto error;
    }

    *ptr = newVal;
    return TCL_OK;

  error:
    Tcl_AppendResult(interp, "bad relief type \"", value,
	"\":  must be flat, groove, raised, ridge, solid or sunken", NULL);
    return TCL_ERROR;
}

static Tcl_Obj *
ReliefPrintProc(clientData, tkwin, widRec,offset, freeProcPtr)
    ClientData clientData;
    Tk_Window tkwin;
    char *widRec;
    int offset;
    Tix_FreeProc **freeProcPtr;
{
    Tix_Relief *ptr = (Tix_Relief*)(widRec+offset);
    Tcl_Obj * result = NULL;

    switch (*ptr) {
      case TIX_RELIEF_RAISED:
	LangSetString(&result,"raised");
        break;
      case TIX_RELIEF_FLAT:
	LangSetString(&result,"flat");
        break;
      case TIX_RELIEF_SUNKEN:
	LangSetString(&result,"sunken");
        break;
      case TIX_RELIEF_GROOVE:
	LangSetString(&result,"groove");
        break;
      case TIX_RELIEF_RIDGE:
	LangSetString(&result,"ridge");
        break;
      case TIX_RELIEF_SOLID:
	LangSetString(&result,"solid");
        break;
      default:
	LangSetString(&result,"unknown");
        break;
    }
    return result;
}
/*
 * The global data structures to use in widget configSpecs arrays
 *
 * These are declared in <tix.h>
 */
#ifndef _LANG
Tk_CustomOption tixConfigRelief = {
    ReliefParseProc, ReliefPrintProc, 0,
};
#endif

/* Tix_SetRcFileName --
 *
 *	Sets a user-specific startup file in a way that's compatible with
 *	different versions of Tclsh
 */
#ifndef _LANG
void Tix_SetRcFileName(interp, rcFileName)
    Tcl_Interp * interp;
    char * rcFileName;
{
#ifdef TCL_7_5_OR_LATER
    /*
     * Starting from TCL 7.5, the symbol tcl_rcFileName is no longer
     * exported by libtcl.a. Instead, this variable must be set using
     * a TCL global variable
     */
    Tcl_SetVar(interp, "tcl_rcFileName", rcFileName, TCL_GLOBAL_ONLY);
#else
    tcl_RcFileName = rcFileName;
#endif
}
#endif

#if (TK_MAJOR_VERSION > 4)

/*
 * The TkComputeTextGeometry function is no longer supported in Tk 8.0+
 */

/*
 *----------------------------------------------------------------------
 *
 * TixComputeTextGeometry --
 *
 *	This procedure computes the amount of screen space needed to
 *	display a multi-line string of text.
 *
 * Results:
 *	There is no return value.  The dimensions of the screen area
 *	needed to display the text are returned in *widthPtr, and *heightPtr.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

void
TixComputeTextGeometry(font, string, numChars, wrapLength,
	widthPtr, heightPtr)
    TixFont font;		/* Font that will be used to display text. */
    char *string;		/* String whose dimensions are to be
				 * computed. */
    int numChars;		/* Number of characters to consider from
				 * string. */
    int wrapLength;		/* Longest permissible line length, in
				 * pixels.  <= 0 means no automatic wrapping:
				 * just let lines get as long as needed. */
    int *widthPtr;		/* Store width of string here. */
    int *heightPtr;		/* Store height of string here. */
{
    Tk_TextLayout textLayout;

    /*
     * The justification itself doesn't affect the geometry (size) of
     * the text string. We pass TK_JUSTIFY_LEFT.
     */

    textLayout = Tk_ComputeTextLayout(font,
	string, -1, wrapLength, TK_JUSTIFY_LEFT, 0,
	widthPtr, heightPtr);
    Tk_FreeTextLayout(textLayout);
}

/*
 *----------------------------------------------------------------------
 *
 * TixDisplayText --
 *
 *	Display a text string on one or more lines.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The text given by "string" gets displayed at the given location
 *	in the given drawable with the given font etc.
 *
 *----------------------------------------------------------------------
 */

void
TixDisplayText(display, drawable, font, string, numChars, x, y,
	length, justify, underline, gc)
    Display *display;		/* X display to use for drawing text. */
    Drawable drawable;		/* Window or pixmap in which to draw the
				 * text. */
    TixFont font;		/* Font that determines geometry of text
				 * (should be same as font in gc). */
    char *string;		/* String to display;  may contain embedded
				 * newlines. */
    int numChars;		/* Number of characters to use from string. */
    int x, y;			/* Pixel coordinates within drawable of
				 * upper left corner of display area. */
    int length;			/* Line length in pixels;  used to compute
				 * word wrap points and also for
				 * justification.   Must be > 0. */
    Tk_Justify justify;		/* How to justify lines. */
    int underline;		/* Index of character to underline, or < 0
				 * for no underlining. */
    GC gc;			/* Graphics context to use for drawing text. */
{
    Tk_TextLayout textLayout;
    int width;
    int height;

    /* FIXME: Needs attention?
        - incoming numChars is not used, possibly due to confusion
          as to whether it is bytes or chars
     */

    textLayout = Tk_ComputeTextLayout(font,
	string, -1, length, justify, 0,
	&width, &height);

    switch (justify) {
        case TK_JUSTIFY_RIGHT:
	    x += length-width;
	    break;
        case TK_JUSTIFY_CENTER:
	    x += (length-width)/2;
	    break;
	default:
	case TK_JUSTIFY_LEFT:
	    break;
    }

    Tk_DrawTextLayout(display, drawable, gc, textLayout,
	    x, y, 0, -1);
    Tk_UnderlineTextLayout(display, drawable, gc,
	    textLayout, x, y, underline);

    Tk_FreeTextLayout(textLayout);
}
#endif

#if TK_MAJOR_VERSION < 8

/*
 * Procedure types defined by Tcl:
 */

typedef void (Tcl_FreeInternalRepProc) _ANSI_ARGS_((struct Tcl_Obj *objPtr));
typedef void (Tcl_DupInternalRepProc) _ANSI_ARGS_((struct Tcl_Obj *srcPtr,
        struct Tcl_Obj *dupPtr));
typedef void (Tcl_UpdateStringProc) _ANSI_ARGS_((struct Tcl_Obj *objPtr));
typedef int (Tcl_SetFromAnyProc) _ANSI_ARGS_((Tcl_Interp *interp,
	struct Tcl_Obj *objPtr));
typedef int (Tcl_ObjCmdProc) _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, int objc, struct Tcl_Obj *CONST objv[]));

/*
 * The following structure represents a type of object, which is a
 * particular internal representation for an object plus a set of
 * procedures that provide standard operations on objects of that type.
 */

typedef struct Tcl_ObjType {
    char *name;			/* Name of the type, e.g. "int". */
    Tcl_FreeInternalRepProc *freeIntRepProc;
				/* Called to free any storage for the type's
				 * internal rep. NULL if the internal rep
				 * does not need freeing. */
    Tcl_DupInternalRepProc *dupIntRepProc;
    				/* Called to create a new object as a copy
				 * of an existing object. */
    Tcl_UpdateStringProc *updateStringProc;
    				/* Called to update the string rep from the
				 * type's internal representation. */
    Tcl_SetFromAnyProc *setFromAnyProc;
    				/* Called to convert the object's internal
				 * rep to this type. Frees the internal rep
				 * of the old type. Returns TCL_ERROR on
				 * failure. */
} Tcl_ObjType;

/*
 * One of the following structures exists for each object in the Tcl
 * system.  An object stores a value as either a string, some internal
 * representation, or both.
 */

typedef struct Tcl_Obj {
    int refCount;		/* When 0 the object will be freed. */
    char *bytes;		/* This points to the first byte of the
				 * object's string representation. The
				 * array must be followed by a null byte
				 * (i.e., at offset length) but may also
				 * contain embedded null characters. The
				 * array's storage is allocated by
				 * ckalloc. NULL indicates the string
				 * rep is empty or invalid and must be
				 * regenerated from the internal rep.
				 * Clients should use Tcl_GetStringFromObj
				 * to get a pointer to the byte array
				 * as a readonly value.  */
    int length;			/* The number of bytes at *bytes, not
				 * including the terminating null. */
    Tcl_ObjType *typePtr;	/* Denotes the object's type. Always
				 * corresponds to the type of the object's
				 * internal rep. NULL indicates the object
				 * has no internal rep (has no type). */
    union {			/* The internal representation: */
	long longValue;		/*   - an long integer value */
	double doubleValue;	/*   - a double-precision floating value */
	VOID *otherValuePtr;	/*   - another, type-specific value */
	struct {		/*   - internal rep as two pointers */
	    VOID *ptr1;
	    VOID *ptr2;
	} twoPtrValue;
    } internalRep;
} Tcl_Obj;

#endif

/*
 *----------------------------------------------------------------------
 *
 * TixGetStringFromObj --
 *
 *	Returns the string representation's byte array pointer and length
 *	for an object.
 *
 * Results:
 *	Returns a pointer to the string representation of objPtr.  If
 *	lengthPtr isn't NULL, the length of the string representation is
 *	stored at *lengthPtr. The byte array referenced by the returned
 *	pointer must not be modified by the caller. Furthermore, the
 *	caller must copy the bytes if they need to retain them since the
 *	object's string rep can change as a result of other operations.
 *      REMARK: This function reacts a little bit different than
 *	Tcl_GetStringFromObj():
 *	- objPtr is allowed to be NULL. In that case the NULL pointer
 *	  will be returned, and the length will be reported to be 0;
 *	In the Img code there is never a distinction between en empty
 *	string and a NULL pointer, while the latter is easier to check
 *	for. That's the reason for this difference.
 *
 * Side effects:
 *	May call the object's updateStringProc to update the string
 *	representation from the internal representation.
 *
 *----------------------------------------------------------------------
 */


char *
TixGetStringFromObj(objPtr, lengthPtr)
    char *objPtr;		/* Object whose string rep byte pointer
				 * should be returned, or NULL */
    register int *lengthPtr;	/* If non-NULL, the location where the
				 * string rep's byte array length should be
				 * stored. If NULL, no length is stored. */
{
    Tcl_Obj *obj = (Tcl_Obj *) objPtr;
    int length;
    if (!lengthPtr)
      lengthPtr = &length;
    if (!obj) {
	if (lengthPtr != NULL) {
	    *lengthPtr = 0;
	}
	return (char *) NULL;
    }
#ifdef _LANG
    objPtr = Tcl_GetStringFromObj(obj,lengthPtr);
    if (*lengthPtr)
     return NULL;
    return objPtr;
#else
    if (initialized & 2) {
	if (obj->bytes != NULL) {
	    if (lengthPtr != NULL) {
		*lengthPtr = obj->length;
	    }
	    return (obj->length) ? obj->bytes : (char *) NULL;
	}

	if (obj->typePtr == NULL) {
	    if (lengthPtr != NULL) {
		*lengthPtr = 0;
	    }
	    return "";
	}

	obj->typePtr->updateStringProc(obj);
	if (lengthPtr != NULL) {
	    *lengthPtr = obj->length;
	}
	return (obj->length) ? obj->bytes : (char *) NULL;
    } else {
	if (lengthPtr != NULL) {
	    *lengthPtr = objPtr ? strlen(objPtr) : 0;
	}
	return objPtr;
    }
#endif /* _LANG */
}

/*----------------------------------------------------------------------
 * TixStartSubRegionDraw --
 *
 *	Limits the subsequent drawing operations into the prescribed
 *	rectangle region. This takes effect up to a matching
 *	TixEndSubRegionDraw() call.
 *
 * Return value:
 *	none.
 *----------------------------------------------------------------------
 */

void
TixpStartSubRegionDraw(ddPtr, drawable, gc, subRegPtr, origX, origY,
	x, y, width, height, needWidth, needHeight)
    Tix_DispData *ddPtr;
    Drawable drawable;
    GC gc;
    TixpSubRegion * subRegPtr;
    int origX;
    int origY;
    int x;
    int y;
    int width;
    int height;
    int needWidth;
    int needHeight;
{
    Display *display = ddPtr->display;
    int depth;

    if ((width < needWidth) || (height < needHeight)) {
	subRegPtr->origX  = origX;
	subRegPtr->origY  = origY;
	subRegPtr->x	  = x;
	subRegPtr->y	  = y;
	subRegPtr->width  = width;
	subRegPtr->height = height;

	/*
	 * Find out the depth of the drawable and create a pixmap of
	 * the same depth.
	 */
        depth = Tk_Depth(ddPtr->tkwin);

	subRegPtr->pixmap = Tk_GetPixmap(display, drawable, width, height,
		depth);

	if (subRegPtr->pixmap != None) {
	    /*
	     * It could be None if we have somehow exhausted the Windows
	     * GDI resources.
	     */
	    XCopyArea(display, drawable, subRegPtr->pixmap, gc, x, y,
		    width, height, 0, 0);
	}
    } else {
	subRegPtr->pixmap = None;
    }
}

/*----------------------------------------------------------------------
 * TixpEndSubRegionDraw --
 *
 *
 *----------------------------------------------------------------------
 */
void
TixpEndSubRegionDraw(display, drawable, gc, subRegPtr)
    Display *display;
    Drawable drawable;
    GC gc;
    TixpSubRegion * subRegPtr;
{
    if (subRegPtr->pixmap != None) {
	XCopyArea(display, subRegPtr->pixmap, drawable, gc, 0, 0,
		subRegPtr->width, subRegPtr->height,
		subRegPtr->x, subRegPtr->y);
	Tk_FreePixmap(display, subRegPtr->pixmap);
	subRegPtr->pixmap = None;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * TixpSubRegDisplayText --
 *
 *	Display a text string on one or more lines in a sub region.
 *
 * Results:
 *	See TkDisplayText
 *
 * Side effects:
 *	See TkDisplayText
 *
 *----------------------------------------------------------------------
 */

void
TixpSubRegDisplayText(display, drawable, gc, subRegPtr, font, string,
	numChars, x, y,	length, justify, underline)
    Display *display;		/* X display to use for drawing text. */
    Drawable drawable;		/* Window or pixmap in which to draw the
				 * text. */
    GC gc;			/* Graphics context to use for drawing text. */
    TixpSubRegion * subRegPtr;	/* Information about the subregion */
    TixFont font;		/* Font that determines geometry of text
				 * (should be same as font in gc). */
    char *string;		/* String to display;  may contain embedded
				 * newlines. */
    int numChars;		/* Number of characters to use from string. */
    int x, y;			/* Pixel coordinates within drawable of
				 * upper left corner of display area. */
    int length;			/* Line length in pixels;  used to compute
				 * word wrap points and also for
				 * justification.   Must be > 0. */
    Tk_Justify justify;		/* How to justify lines. */
    int underline;		/* Index of character to underline, or < 0
				 * for no underlining. */
{
    if (subRegPtr->pixmap != None) {
	TixDisplayText(display, subRegPtr->pixmap, font, string,
		numChars, x - subRegPtr->x, y - subRegPtr->y,
		length, justify, underline, gc);
    } else {
	TixDisplayText(display, drawable, font, string,
		numChars, x, y,	length, justify, underline, gc);
    }
}

/*----------------------------------------------------------------------
 * TixpSubRegFillRectangle --
 *
 *
 *----------------------------------------------------------------------
 */

void
TixpSubRegFillRectangle(display, drawable, gc, subRegPtr, x, y, width, height)
    Display *display;		/* X display to use for drawing rectangle. */
    Drawable drawable;		/* Window or pixmap in which to draw the
				 * rectangle. */
    GC gc;			/* Graphics context to use for drawing. */
    TixpSubRegion * subRegPtr;	/* Information about the subregion */
    int x, y;			/* Pixel coordinates within drawable of
				 * upper left corner of display area. */
    int width, height;		/* Size of the rectangle. */
{
    if (subRegPtr->pixmap != None) {
	XFillRectangle(display, subRegPtr->pixmap, gc,
		x - subRegPtr->x, y - subRegPtr->x, width, height);
    } else {
	XFillRectangle(display, drawable, gc, x, y, width, height);
    }
}

/*----------------------------------------------------------------------
 * TixpSubRegDrawImage	--
 *
 *	Draws a Tk image in a subregion.
 *----------------------------------------------------------------------
 */

void
TixpSubRegDrawImage(subRegPtr, image, imageX, imageY, width, height,
	drawable, drawableX, drawableY)
    TixpSubRegion * subRegPtr;
    Tk_Image image;
    int imageX;
    int imageY;
    int width;
    int height;
    Drawable drawable;
    int drawableX;
    int drawableY;
{
    if (subRegPtr->pixmap != None) {
	Tk_RedrawImage(image, imageX, imageY, width, height, subRegPtr->pixmap,
	        drawableX - subRegPtr->x, drawableY - subRegPtr->y);
    } else {
	Tk_RedrawImage(image, imageX, imageY, width, height, drawable,
	        drawableX, drawableY);
    }
}

void
TixpSubRegDrawBitmap(display, drawable, gc, subRegPtr, bitmap, src_x, src_y,
	width, height, dest_x, dest_y, plane)
    Display *display;
    Drawable drawable;
    GC gc;
    TixpSubRegion * subRegPtr;
    Pixmap bitmap;
    int src_x, src_y;
    int width, height;
    int dest_x, dest_y;
    unsigned long plane;
{
    XSetClipOrigin(display, gc, dest_x, dest_y);
    if (subRegPtr->pixmap != None) {
	XCopyPlane(display, bitmap, subRegPtr->pixmap, gc, src_x, src_y,
		width, height, dest_x - subRegPtr->x, dest_y - subRegPtr->y,
		plane);
    } else {
	XCopyPlane(display, bitmap, drawable, gc, src_x, src_y, width, height,
	        dest_x, dest_y, plane);
    }
    XSetClipOrigin(display, gc, 0, 0);
}

