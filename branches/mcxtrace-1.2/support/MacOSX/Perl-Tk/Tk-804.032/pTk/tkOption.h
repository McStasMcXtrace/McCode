#ifndef _TKOPTION
#define _TKOPTION
EXTERN void		Tk_AddOption _ANSI_ARGS_((Tk_Window tkwin,
				CONST char * name, CONST char * value,
				int priority));
EXTERN Tk_Uid		Tk_GetOption _ANSI_ARGS_((Tk_Window tkwin, CONST char *name,
			    CONST char *className));
EXTERN int		Tk_OptionObjCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, Tcl_Obj *CONST *args));
#ifndef _TKINT
struct TkWindow;
#endif
EXTERN void		TkOptionClassChanged _ANSI_ARGS_((struct TkWindow *winPtr));
EXTERN void		TkOptionDeadWindow _ANSI_ARGS_((struct TkWindow *winPtr));
#endif  /* _TKOPTION */


