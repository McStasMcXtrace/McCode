EXTERN int		Tk_BellObjCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int objc,
			    Tcl_Obj *CONST objv[]));
EXTERN int		Tk_BindCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_BindtagsCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_ButtonCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_CanvasObjCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, Tcl_Obj *CONST objv[]));
EXTERN int		Tk_CheckbuttonCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_ClipboardCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int              Tk_ChooseColorCmd _ANSI_ARGS_((ClientData clientData,
                            Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_DestroyCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_EntryCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_EventObjCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, Tcl_Obj *CONST objv[]));
EXTERN int		Tk_FrameCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_FocusObjCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, Tcl_Obj *CONST objv[]));
EXTERN int		Tk_FontObjCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int objc,
			    Tcl_Obj *CONST objv[]));
EXTERN int              Tk_GetOpenFileCmd _ANSI_ARGS_((ClientData clientData,
                            Tcl_Interp *interp, int argc, char **argv));
EXTERN int              Tk_GetSaveFileCmd _ANSI_ARGS_((ClientData clientData,
                            Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_GrabCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_GridCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_ImageObjCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, Tcl_Obj *CONST objv[]));
EXTERN int		Tk_LabelCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_ListboxCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_LowerCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_MenuCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_MenubuttonCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int              Tk_MessageBoxCmd _ANSI_ARGS_((ClientData clientData,
                            Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_MessageCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_OptionCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_PackCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_PlaceCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_PropertyCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_RadiobuttonCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_RaiseCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_ScaleCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_ScrollbarCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_SelectionCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_SendCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_TextCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_TkObjCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int objc,
			    Tcl_Obj *CONST objv[]));
EXTERN int		Tk_TkwaitCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_ToplevelCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_UpdateCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
EXTERN int		Tk_WinfoObjCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int objc,
			    Tcl_Obj *CONST objv[]));
EXTERN int		Tk_WmCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));

EXTERN int	TkTileParseProc _ANSI_ARGS_((
		    ClientData clientData, Tcl_Interp *interp,
		    Tk_Window tkwin, Tcl_Obj * value, char *widgRec,
		    int offset));
EXTERN Tcl_Obj *	TkTilePrintProc _ANSI_ARGS_((
		    ClientData clientData, Tk_Window tkwin,
		    char *widgRec, int offset,
		    Tcl_FreeProc **freeProcPtr));
EXTERN int	TkOffsetParseProc _ANSI_ARGS_((
		    ClientData clientData, Tcl_Interp *interp,
		    Tk_Window tkwin, Tcl_Obj * value, char *widgRec,
		    int offset));
EXTERN Tcl_Obj *	TkOffsetPrintProc _ANSI_ARGS_((
		    ClientData clientData, Tk_Window tkwin,
		    char *widgRec, int offset,
		    Tcl_FreeProc **freeProcPtr));
EXTERN int	TkStateParseProc _ANSI_ARGS_((
		    ClientData clientData, Tcl_Interp *interp,
		    Tk_Window tkwin, Tcl_Obj * value,
		    char *widgRec, int offset));
EXTERN Tcl_Obj *	TkStatePrintProc _ANSI_ARGS_((
		    ClientData clientData, Tk_Window tkwin,
		    char *widgRec, int offset,
		    Tcl_FreeProc **freeProcPtr));
EXTERN int	TkOrientParseProc _ANSI_ARGS_((
		    ClientData clientData, Tcl_Interp *interp,
		    Tk_Window tkwin, Tcl_Obj * value,
		    char *widgRec, int offset));
EXTERN Tcl_Obj *	TkOrientPrintProc _ANSI_ARGS_((
		    ClientData clientData, Tk_Window tkwin,
		    char *widgRec, int offset,
		    Tcl_FreeProc **freeProcPtr));
EXTERN int	TkPixelParseProc _ANSI_ARGS_((
		    ClientData clientData, Tcl_Interp *interp,
		    Tk_Window tkwin, Tcl_Obj * value,
		    char *widgRec, int offset));
EXTERN Tcl_Obj *	TkPixelPrintProc _ANSI_ARGS_((
		    ClientData clientData, Tk_Window tkwin,
		    char *widgRec, int offset,
		    Tcl_FreeProc **freeProcPtr));
