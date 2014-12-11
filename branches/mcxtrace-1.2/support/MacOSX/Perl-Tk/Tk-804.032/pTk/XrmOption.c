/*
 * XrmOption.c --
 *
 * Copyright (c) 1990-1994 The Regents of the University of California.
 * Copyright (c) 1994-1995 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#if !defined(__WIN32__) && !defined(_WIN32) && !defined(__PM__)

static char sccsid[] = "@(#) tkOption.c 1.41 95/06/25 15:30:42";

#include "tkPort.h"
#include "tkInt.h"
#include "tkOption.h"
#include "tkOption_f.h"
#include "tkVMacro.h"
#include <X11/Xresource.h>
#include "tkXrm.h"

#if !defined(XlibSpecificationRelease) || XlibSpecificationRelease < 5

#define XrmPermStringToQuark(x) XrmStringToQuark(x)

static XrmDatabase
XrmGetDatabase(display)
Display *display;
{
 return (XrmDatabase) NULL;
}

static void
XrmSetDatabase(display, db)
Display *display;
XrmDatabase db;
{

}

static void
XrmCombineFileDatabase(filename, target_db, override)
char *filename;
XrmDatabase *target_db;
Bool override;
{
 XrmDatabase source_db = XrmGetFileDatabase(filename);
 if (override || !*target_db)
  XrmMergeDatabases(source_db, target_db);
 else
  {
   XrmMergeDatabases(*target_db, &source_db);
   *target_db = source_db;
  }
}

#endif

/*
 * Flags in Element structures:
 *
 * CLASS -                                                                                                                                                                                                                                                      Non-zero means this element refers to a class,
 *                                                                                                                                                                                                                                                                                                                                                                                              Zero means this element refers to a name.
 * NODE -                                                                                                                                                                                                                                                       Zero means this is a leaf element (the child
 *                                                                                                                                                                                                                                                                                                                                                                                              field is a value, not a pointer to another node).
 *                                                                                                                                                                                                                                                                                                                                                                                              One means this is a node element.
 * WILDCARD -                                                                                                                                                                                                                                                   Non-zero means this there was a star in the
 *                                                                                                                                                                                                                                                                                                                                                                                              original specification just before this element.
 *                                                                                                                                                                                                                                                                                                                                                                                              Zero means there was a dot.
 */

typedef void ElArray;

static void OptionInit _ANSI_ARGS_((TkMainInfo * mainPtr));
static int ParsePriority _ANSI_ARGS_((Tcl_Interp * interp,
                                      char *string));
static void ClearOptionTree _ANSI_ARGS_((TkMainInfo * mainPtr));
static int ReadOptionFile _ANSI_ARGS_((Tcl_Interp * interp,
                                Tk_Window tkwin, char *fileName, int priority));
static int SetupQuarks _ANSI_ARGS_((TkWindow *winPtr,int depth));


static TkWindow *cachedWindow = NULL;
static XrmQuarkList Qname;      /* name Quark list (malloced) */
static XrmQuarkList Qclass;     /* class Quark list (malloced) */
static int Qsize  = 0;          /* Number of slots in Quark lists */
static int Qindex = 0;          /* index just beyond cacheWindow in Quark list */


#ifdef XRM_DEBUG
static void
Qshow(FILE *f,char *lab,XrmQuarkList l)
{
 int prefix = '"';
 fprintf(f," %s:",lab);
 while (*l != NULLQUARK)
  {
   fprintf(f,"%c%s",prefix,XrmQuarkToString(*l++));
   prefix = '.';
  }
 fprintf(f,"\"\n");
}
#endif

static int
SetupQuarks(winPtr,depth)
TkWindow *winPtr;
int depth;
{
 if (cachedWindow != NULL && cachedWindow->mainPtr == winPtr->mainPtr)
  {
   TkWindow *c = cachedWindow;
   int index = Qindex;
   while (c != NULL)
    {
     if (winPtr == c)
      {
       if (index + depth > Qsize)
        {size_t size = (Qsize = depth+Qindex+5) * sizeof(XrmQuark);
         Qname  = (XrmQuarkList) ckrealloc((char *)Qname,size);
         Qclass = (XrmQuarkList) ckrealloc((char *)Qclass,size);
        }
#ifdef XRM_DEBUG
       printf("using %d for %s\n",index,Tk_PathName(c));
#endif
       return index;
      }
     index--;
     c = c->parentPtr;
    }
  }
 if (winPtr->parentPtr != NULL)
  depth = SetupQuarks(winPtr->parentPtr,depth+1);
 else
  {
   if (depth > Qsize)
    {
     size_t size = (Qsize = depth+5) * sizeof(XrmQuark);
     Qname  = (XrmQuarkList)((Qname) ? ckrealloc((char *)Qname,size) : ckalloc(size));
     Qclass = (XrmQuarkList)((Qclass) ? ckrealloc((char *)Qclass,size) : ckalloc(size));
    }
   depth = 0;
  }
 Qname[depth]  = XrmPermStringToQuark(Tk_Name(winPtr));
 Qclass[depth] = XrmPermStringToQuark(Tk_Class(winPtr));
 return depth+1;
}

/*
 *--------------------------------------------------------------
 *
 * Xrm_GetOption  --
 *
 *  Retrieve  an  option  from  the  option  database.
 *
 * Results:
 *  The  return  value  is  the  value  specified  in  the  option
 *  database  for  the  given  name  and  class  on  the  given
 *  window.  If  there  is  nothing  specified  in  the  database
 *  for  that  option,  then  NULL  is  returned.
 *
 * Side  effects:
 *  The  internal  caches  used  to  speed  up  option  mapping
 *  may  be  modified,  if  this  tkwin  is  different  from  the
 *  last  tkwin  used  for  option  retrieval.
 *
 *--------------------------------------------------------------
 */

Tk_Uid
Xrm_GetOption(tkwin, name, className)
Tk_Window tkwin;                  /* Token for window that option is
                                     * associated with. */
CONST char *name;                 /* Name of option. */
CONST char *className;            /* Class of option.  NULL means there
                                     * is no class for this option:  just
                                     * check for name. */
{
 TkWindow *winPtr = (TkWindow *) tkwin;
 XrmDatabase database;
 XrmRepresentation type = NULLQUARK;
 XrmValue value;

 if (winPtr->mainPtr->optionRootPtr == NULL)
  {
   OptionInit(winPtr->mainPtr);
  }

 if (winPtr != cachedWindow)
  {
   Qindex = SetupQuarks(winPtr,3);
   cachedWindow = winPtr;
  }

#ifdef DEBUGGING
 if (Qindex + 1 > Qsize)
  abort();
#endif

 Qname[Qindex] = XrmStringToQuark(name);
 Qclass[Qindex] = XrmStringToQuark(className);

 Qname[Qindex+1]  = NULLQUARK;
 Qclass[Qindex+1] = NULLQUARK;

#ifdef XRM_DEBUG
 printf("%s:\n",Tk_PathName(winPtr));
 Qshow(stdout,"name",Qname);
 Qshow(stdout,"Class",Qclass);
#endif

 database = (XrmDatabase) winPtr->mainPtr->optionRootPtr;

 memset(&value,0,sizeof(value));

 if (database && XrmQGetResource(database, Qname, Qclass, &type, &value))
  {
#ifdef XRM_DEBUG
   fprintf(stdout, "%s(%s/%s) type=%s val=%.*s\n",
           Tk_PathName(winPtr), name, className,
           XrmQuarkToString(type), (int) value.size, value.addr);
#endif
   return Tk_GetUid(value.addr);
  }
 return NULL;
}


/*
 *--------------------------------------------------------------
 *
 * Xrm_AddOption --
 *
 *  Add a new option to the option database.
 *
 * Results:
 *  None.
 *
 * Side effects:
 *  Information is added to the option database.
 *
 *--------------------------------------------------------------
 */

#undef Xrm_AddOption
void
Xrm_AddOption(tkwin, name, value, priority)
Tk_Window tkwin;                  /* Window token;  option will be associated
                                     * with main window for this window. */
CONST char *name;                       /* Multi-element name of option. */
CONST char *value;                      /* String value for option. */
int priority;                     /* Overall priority level to use for
                                     * this option, such as TK_USER_DEFAULT_PRIO
                                     * or TK_INTERACTIVE_PRIO.  Must be between
                                     * 0 and TK_MAX_PRIO. */
{
 TkWindow *winPtr = ((TkWindow *) tkwin)->mainPtr->winPtr;
 XrmDatabase database;

 if (winPtr->mainPtr->optionRootPtr == NULL)
  {
   OptionInit(winPtr->mainPtr);
  }
 database = (XrmDatabase) winPtr->mainPtr->optionRootPtr;

 /* XrmMergeDatabases ? */

 XrmPutStringResource(&database, name, value);
#ifdef XRM_DEBUG
 fprintf(stdout, "Xrm_AddOption %p %s %s %d\n", database, name, value, priority);
#endif
}

/*
 *--------------------------------------------------------------
 *
 *  Xrm_OptionCmd  --
 *
 *  This  procedure  is  invoked  to  process  the  "option"  Tcl  command.
 *  See  the  user  documentation  for  details  on  what  it  does.
 *
 *  Results:
 *  A  standard  Tcl  result.
 *
 *  Side  effects:
 *  See  the  user  documentation.
 *
 *--------------------------------------------------------------
 */

int
Xrm_OptionCmd(clientData, interp, argc, args)
ClientData clientData;            /* Main window associated with
                                     * interpreter. */
Tcl_Interp *interp;               /* Current interpreter. */
int argc;                         /* Number of arguments. */
Tcl_Obj *CONST args[];                  /* Argument strings. */
{
 Tk_Window tkwin = (Tk_Window) clientData;
 size_t length;
 char c;

 if (argc < 2)
  {
   Tcl_AppendResult(interp, "wrong # args: should be \"", Tcl_GetString(args[0]),
                    " cmd arg ?arg ...?\"", NULL);
   return TCL_ERROR;
  }
 c = Tcl_GetString(args[1])[0];
 length = strlen(Tcl_GetString(args[1]));
 if ((c == 'a') && (strncmp(Tcl_GetString(args[1]), "add", length) == 0))
  {
   int priority;

   if ((argc != 4) && (argc != 5))
    {
     Tcl_AppendResult(interp, "wrong # args: should be \"",
                  Tcl_GetString(args[0]), " add pattern value ?priority?\"", NULL);
     return TCL_ERROR;
    }
   if (argc == 4)
    {
     priority = TK_INTERACTIVE_PRIO;
    }
   else
    {
     priority = ParsePriority(interp, Tcl_GetString(args[4]));
     if (priority < 0)
      {
       return TCL_ERROR;
      }
    }
   Xrm_AddOption(tkwin, Tcl_GetString(args[2]), Tcl_GetString(args[3]), priority);
   return TCL_OK;
  }
 else if ((c == 'c') && (strncmp(Tcl_GetString(args[1]), "clear", length) == 0))
  {
   TkMainInfo *mainPtr;

   if (argc != 2)
    {
     Tcl_AppendResult(interp, "wrong # args: should be \"",
                      Tcl_GetString(args[0]), " clear\"", NULL);
     return TCL_ERROR;
    }
   mainPtr = ((TkWindow *) tkwin)->mainPtr;
   if (mainPtr->optionRootPtr != NULL)
    {
     ClearOptionTree(mainPtr);
     mainPtr->optionRootPtr = NULL;
    }
   cachedWindow = NULL;
   Qindex = 0;
   return TCL_OK;
  }
 else if ((c == 'g') && (strncmp(Tcl_GetString(args[1]), "get", length) == 0))
  {
   Tk_Window window;
   Tk_Uid value;

   if (argc != 5)
    {
     Tcl_AppendResult(interp, "wrong # args: should be \"",
                      Tcl_GetString(args[0]), " get window name class\"", NULL);
     return TCL_ERROR;
    }
   window = Tk_NameToWindow(interp, Tcl_GetString(args[2]), tkwin);
   if (window == NULL)
    {
     return TCL_ERROR;
    }
   value = Xrm_GetOption(window, Tcl_GetString(args[3]), Tcl_GetString(args[4]));
   if (value != NULL)
    {
     Tcl_SetResult(interp, (char *) value, TCL_STATIC);
    }
   return TCL_OK;
  }
 else if ((c == 'r') && (strncmp(Tcl_GetString(args[1]), "readfile", length) == 0))
  {
   int priority;

   if ((argc != 3) && (argc != 4))
    {
     Tcl_AppendResult(interp, "wrong # args:  should be \"",
                      Tcl_GetString(args[0]), " readfile fileName ?priority?\"",
                      NULL);
     return TCL_ERROR;
    }
   if (argc == 4)
    {
     priority = ParsePriority(interp, Tcl_GetString(args[3]));
     if (priority < 0)
      {
       return TCL_ERROR;
      }
    }
   else
    {
     priority = TK_INTERACTIVE_PRIO;
    }
   return ReadOptionFile(interp, tkwin, Tcl_GetString(args[2]), priority);
  }
 else
  {
   Tcl_AppendResult(interp, "bad option \"", Tcl_GetString(args[1]),
                    "\": must be add, clear, get, or readfile", NULL);
   return TCL_ERROR;
  }
}

/*
 *--------------------------------------------------------------
 *
 *  TkOptionDeadWindow  --
 *
 *  This  procedure  is  called  whenever  a  window  is  deleted.
 *  It  cleans  up  any  option-related  stuff  associated  with
 *  the  window.
 *
 *  Results:
 *  None.
 *
 *  Side  effects:
 *  Option-related  resources  are  freed.  See  code  below
 *  for  details.
 *
 *--------------------------------------------------------------
 */

void
XrmOptionDeadWindow(winPtr)
register TkWindow *winPtr;        /* Window to be cleaned up. */
{
 /*
  * If this window was a main window, then delete its option
  * database.
  */

 XrmOptionClassChanged(winPtr);

 if ((winPtr->mainPtr != NULL) && (winPtr->mainPtr->winPtr == winPtr)
	    && (winPtr->mainPtr->optionRootPtr != NULL))
  {
   if (winPtr->dispPtr->refCount <= 0)
    {
     XrmDestroyDatabase((XrmDatabase) winPtr->mainPtr->optionRootPtr);
     XrmSetDatabase(winPtr->display,(XrmDatabase) NULL);
    }
   winPtr->mainPtr->optionRootPtr = NULL;
  }
}

/*
 *----------------------------------------------------------------------
 *
 * TkOptionClassChanged --
 *
 *  This  procedure  is  invoked  when  a  window's  class  changes.  If
 *  the  window  is  on  the  option  cache,  this  procedure  flushes
 *  any  information  for  the  window,  since  the  new  class  could  change
 *  what  is  relevant.
 *
 *  Results:
 *  None.
 *
 *  Side  effects:
 *  The  option  cache  may  be  flushed  in  part  or  in  whole.
 *
 *----------------------------------------------------------------------
 */
#undef TkOptionClassChanged
void
XrmOptionClassChanged(winPtr)
TkWindow *winPtr;                 /* Window whose class changed. */
{
 if (winPtr == cachedWindow)
  {
   if (cachedWindow->parentPtr)
    {
     cachedWindow = cachedWindow->parentPtr;
     Qindex--;
    }
   else
    {
     cachedWindow = NULL;
     Qindex = 0;
    }
  }
}

/*
 *----------------------------------------------------------------------
 *
 *  ParsePriority  --
 *
 *  Parse  a  string  priority  value.
 *
 *  Results:
 *  The  return  value  is  the  integer  priority  level  corresponding
 *  to  string,  or  -1  if  string  doesn't  point  to  a  valid  priority  level.
 *  In  this  case,  an  error  message  is  left  in  Tcl_GetResult(interp).
 *
 *  Side  effects:
 *  None.
 *
 *----------------------------------------------------------------------
 */

static int
ParsePriority(interp, string)
Tcl_Interp *interp;               /* Interpreter to use for error reporting. */
char *string;                     /* Describes a priority level, either
                                     * symbolically or numerically. */
{
 int priority,
     c;
 size_t length;

 c = string[0];
 length = strlen(string);
 if ((c == 'w') && (strncmp(string, "widgetDefault", length) == 0))
  {

   return TK_WIDGET_DEFAULT_PRIO;
  }
 else if ((c == 's') && (strncmp(string, "startupFile", length) == 0))
  {

   return TK_STARTUP_FILE_PRIO;
  }
 else if ((c == 'u') && (strncmp(string, "userDefault", length) == 0))
  {

   return TK_USER_DEFAULT_PRIO;
  }
 else if ((c == 'i') && (strncmp(string, "interactive", length) == 0))
  {

   return TK_INTERACTIVE_PRIO;
  }
 else
  {
   char *end;

   priority = strtoul(string, &end, 0);
   if ((end == string) || (*end != 0) || (priority < 0)
       || (priority > 100))
    {
     Tcl_AppendResult(interp, "bad priority level \"", string,
                      "\": must be widgetDefault, startupFile, userDefault, ",
                      "interactive, or a number between 0 and 100",
                      NULL);
     return -1;
    }
  }
 return priority;
}

/*
 *----------------------------------------------------------------------
 *
 * ReadOptionFile --
 *
 *   Read a file of options ("resources" in the old X terminology)
 *   and load them into the option database.
 *
 * Results:
 *   The return value is a standard Tcl return code.   In the case of
 *   an error in parsing string, TCL_ERROR will be returned and an
 *   error message will be left in Tcl_GetResult(interp).
 *
 * Side effects:
 *   None.
 *
 *----------------------------------------------------------------------
 */

static int
ReadOptionFile(interp, tkwin, fileName, priority)
Tcl_Interp *interp;               /* Interpreter to use for reporting results. */
Tk_Window tkwin;                  /* Token for window:  options are entered
                                     * for this window's main window. */
char *fileName;                   /* Name of file containing options. */
int priority;                     /* Priority level to use for options in
                                     * this file, such as TK_USER_DEFAULT_PRIO
                                     * or TK_INTERACTIVE_PRIO.  Must be between
                                     * 0 and TK_MAX_PRIO. */
{
 int result = TCL_OK;
 Tcl_DString newName;
 char *realName = Tcl_TranslateFileName(interp, fileName, &newName);

 if (realName != NULL)
  {
   XrmDatabase database = XrmGetFileDatabase(realName);
   if (database)
    {
     TkWindow *winPtr = (TkWindow *) tkwin;
     if (priority > TK_WIDGET_DEFAULT_PRIO &&
       winPtr->mainPtr->optionRootPtr == NULL)
      {
       OptionInit(winPtr->mainPtr);
      }
     XrmCombineFileDatabase(realName, (XrmDatabase *)(&winPtr->mainPtr->optionRootPtr),
                            (priority > TK_STARTUP_FILE_PRIO));
    }
   else
    {
     Tcl_AppendResult(interp, "couldn't read file \"", realName, "\"",
                      NULL);
     result = TCL_ERROR;
    }
  }
 else
  {
   return TCL_ERROR;
  }
 Tcl_DStringFree(&newName);
 return result;
}

/*
 *--------------------------------------------------------------
 *
 * OptionInit --
 *
 *   Initialize data structures for option handling.
 *
 * Results:
 *   None.
 *
 * Side effects:
 *   Option-related data structures get initialized.
 *
 *--------------------------------------------------------------
 */

static void
OptionInit(mainPtr)
register TkMainInfo *mainPtr;     /* Top-level information about
                                     * window that isn't initialized
                                     * yet. */
{
 TkWindow *winPtr = mainPtr->winPtr;
 static int initialized = 0;
 /*
  * First, once-only initialization.
  */

 if (!initialized)
  {
   size_t size = (Qsize = 32) * sizeof(XrmQuark);
   XrmInitialize();
   Qindex = 0;
   Qname  = (XrmQuarkList) ckalloc(size);
   Qclass = (XrmQuarkList) ckalloc(size);
   initialized = 1;
  }

 /*
  * Then, per-main-window initialization.
  */

 mainPtr->optionRootPtr = (void *) XrmGetDatabase(winPtr->display);

 if (!mainPtr->optionRootPtr)
  {
   /*
    * Try the RESOURCE_MANAGER property on the root window first.
    */

   if (XResourceManagerString(winPtr->display) != NULL)
    {
     mainPtr->optionRootPtr = (void *) XrmGetStringDatabase(XResourceManagerString(winPtr->display));
    }
   else
    {
     char *regProp = NULL;
     int actualFormat;
     unsigned long numItems, bytesAfter;
     Atom actualType;
     int result = XGetWindowProperty(winPtr->display,
                                 RootWindow(winPtr->display, 0),
                                 XA_RESOURCE_MANAGER, 0, 100000,
                                 False, XA_STRING, &actualType, &actualFormat,
                             &numItems, &bytesAfter, (unsigned char **) &regProp);

     if ((result == Success) && (actualType == XA_STRING) && (actualFormat == 8))
      {
       mainPtr->optionRootPtr = (void *) XrmGetStringDatabase(regProp);
       XFree(regProp);
      }
     else
      {
       char *home = getenv("HOME");
       /*
        * No luck there.  Try a .Xdefaults file in the user's home
        * directory.
        */
       if (regProp != NULL)
        XFree(regProp);
       if (home != NULL)
        {
         char *fileName = (char *) ckalloc((unsigned) (strlen(home) + 20));
         sprintf(fileName, "%s/.Xdefaults", home);
         mainPtr->optionRootPtr = (void *) XrmGetFileDatabase(fileName);
         ckfree(fileName);
        }
      }
    }
   if (mainPtr->optionRootPtr)
    {
     XrmSetDatabase(winPtr->display, (XrmDatabase) mainPtr->optionRootPtr);
    }
  }
}

/*
 *--------------------------------------------------------------
 *
 * ClearOptionTree --
 *
 *   This procedure is called to erase everything in a
 *   hierarchical option database.
 *
 * Results:
 *   None.
 *
 * Side effects:
 *   All the options associated with arrayPtr are deleted,
 *   along with all option subtrees.   The space pointed to
 *   by arrayPtr is freed.
 *
 *--------------------------------------------------------------
 */

static void
ClearOptionTree(mainPtr)
TkMainInfo *mainPtr;
{
 if (mainPtr->optionRootPtr)
  {
   mainPtr->optionRootPtr = NULL;
  }
}

#endif

void
Xrm_import(class)
char *class;
{
#if !defined(__WIN32__) && !defined(_WIN32) && !defined(__PM__)
 /* This is sneaky - we patch up the function tables so
     that calls to Tk*Option*() map to Xrm versions.
 */
 LangOptionCommand = Xrm_OptionCmd;
 TkoptionVptr->V_Tk_AddOption = Xrm_AddOption;
 TkoptionVptr->V_Tk_GetOption = Xrm_GetOption;
 TkoptionVptr->V_TkOptionClassChanged = XrmOptionClassChanged;
 TkoptionVptr->V_TkOptionDeadWindow = XrmOptionDeadWindow;
#endif
}


