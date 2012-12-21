#ifdef _TKOPTION
#ifndef TkOptionClassChanged
VFUNC(void,TkOptionClassChanged,V_TkOptionClassChanged,_ANSI_ARGS_((struct TkWindow *winPtr)))
#endif /* #ifndef TkOptionClassChanged */

#ifndef TkOptionDeadWindow
VFUNC(void,TkOptionDeadWindow,V_TkOptionDeadWindow,_ANSI_ARGS_((struct TkWindow *winPtr)))
#endif /* #ifndef TkOptionDeadWindow */

#ifndef Tk_AddOption
VFUNC(void,Tk_AddOption,V_Tk_AddOption,_ANSI_ARGS_((Tk_Window tkwin,
				CONST char * name, CONST char * value,
				int priority)))
#endif /* #ifndef Tk_AddOption */

#ifndef Tk_GetOption
VFUNC(Tk_Uid,Tk_GetOption,V_Tk_GetOption,_ANSI_ARGS_((Tk_Window tkwin, CONST char *name,
			    CONST char *className)))
#endif /* #ifndef Tk_GetOption */

#ifndef Tk_OptionObjCmd
VFUNC(int,Tk_OptionObjCmd,V_Tk_OptionObjCmd,_ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, Tcl_Obj *CONST *args)))
#endif /* #ifndef Tk_OptionObjCmd */

#endif /* _TKOPTION */
