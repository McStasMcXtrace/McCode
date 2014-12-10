#ifndef _TKGLUE
#define _TKGLUE

#ifndef BASEEXT
#define BASEEXT "Tk"
#endif

#ifndef _TKOPTION
#include "pTk/tkOption.h"
#include "pTk/tkOption_f.h"
#endif

typedef struct EventAndKeySym
 {XEvent event;
  KeySym keySym;
  Tcl_Interp  *interp;
  Tk_Window   tkwin;
  SV    *window;
 } EventAndKeySym;

typedef struct Lang_CmdInfo
 {Tcl_CmdInfo Tk;
  Tcl_Interp  *interp;
  Tk_Window   tkwin;
  SV          *image;
  Tk_Font     tkfont;
 } Lang_CmdInfo;

#include "vtab.def"


#define VTABLE_INIT() IMPORT_VTABLES

extern Lang_CmdInfo *WindowCommand _ANSI_ARGS_((SV *win,HV **hptr, int moan));
extern Tk_Window SVtoWindow _ANSI_ARGS_((SV *win));
extern Tk_Font SVtoFont _ANSI_ARGS_((SV *win));
extern int Call_Tk _ANSI_ARGS_((Lang_CmdInfo *info,int argc, SV **args));
extern HV *InterpHv _ANSI_ARGS_((Tcl_Interp *interp,int fatal));
extern SV *WidgetRef _ANSI_ARGS_((Tcl_Interp *interp, char *path));
extern SV *ObjectRef _ANSI_ARGS_((Tcl_Interp *interp, char *path));
extern SV *TkToWidget _ANSI_ARGS_((Tk_Window tkwin,Tcl_Interp **pinterp));
extern SV *FindTkVarName _ANSI_ARGS_((CONST char *varName,int flags));
extern void EnterWidgetMethods _ANSI_ARGS_((char *package, ...));
extern SV *MakeReference _ANSI_ARGS_((SV * sv));
extern Tk_Window TkToMainWindow _ANSI_ARGS_((Tk_Window tkwin));
extern void Lang_TkSubCommand _ANSI_ARGS_ ((char *name, Tcl_ObjCmdProc *proc));
extern void Lang_TkCommand _ANSI_ARGS_ ((char *name, Tcl_ObjCmdProc *proc));
extern SV *XEvent_Info _((EventAndKeySym *obj,char *s));
extern EventAndKeySym *SVtoEventAndKeySym _((SV *arg));
extern int XSTkCommand _ANSI_ARGS_((CV *cv, int mwcd, Tcl_ObjCmdProc *proc, int items, SV **args));

extern XS(XStoWidget);

EXTERN void ClearErrorInfo _ANSI_ARGS_((SV *interp));
EXTERN Tk_Window mainWindow;
EXTERN void DumpStack _ANSI_ARGS_((CONST char *who));
EXTERN void  Boot_Glue (pTHX);
EXTERN void  Boot_Tix  (pTHX);
EXTERN void install_vtab _ANSI_ARGS_((char *name, void *table, size_t size));
extern SV *TagIt _((SV *sv, char *type));
extern void Font_DESTROY _((SV *sv));
struct pTkCheckChain;
extern void Tk_CheckHash _((SV *sv,struct pTkCheckChain *chain));

extern int	has_highbit(CONST char *s,int l);
extern SV *	sv_maybe_utf8(SV *sv);
extern SV *	Lang_SystemEncoding(void);

#ifdef WIN32
#include "pTk/tkWinInt.h"
#endif

#ifndef WIN32
#define HWND void *
#endif
EXTERN HWND SVtoHWND _ANSI_ARGS_((SV *win));

#endif

