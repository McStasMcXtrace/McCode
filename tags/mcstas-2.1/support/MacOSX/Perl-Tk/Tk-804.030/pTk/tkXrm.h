EXTERN void		Xrm_AddOption _ANSI_ARGS_((Tk_Window tkwin, CONST char *name,
			    CONST char *value, int priority));
EXTERN Tk_Uid		Xrm_GetOption _ANSI_ARGS_((Tk_Window tkwin, CONST char *name,
			    CONST char *className));
EXTERN int		Xrm_OptionCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, Tcl_Obj *CONST args[]));
EXTERN void		XrmOptionClassChanged _ANSI_ARGS_((TkWindow *winPtr));
EXTERN void		XrmOptionDeadWindow _ANSI_ARGS_((TkWindow *winPtr));
EXTERN void		Xrm_import _ANSI_ARGS_((char *class));



