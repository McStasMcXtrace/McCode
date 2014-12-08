#ifdef _TIX
#ifndef TixDItemParseProc
VFUNC(int,TixDItemParseProc,V_TixDItemParseProc,_ANSI_ARGS_((ClientData clientData,
		Tcl_Interp *interp, Tk_Window tkwin, Tcl_Obj * value,
		char *widRec, int offset)))
#endif /* #ifndef TixDItemParseProc */

#ifndef TixDItemPrintProc
VFUNC(Tcl_Obj *,TixDItemPrintProc,V_TixDItemPrintProc,_ANSI_ARGS_((
		ClientData clientData, Tk_Window tkwin, char *widRec,
		int offset, Tcl_FreeProc **freeProcPtr)))
#endif /* #ifndef TixDItemPrintProc */

#ifndef TixDItemStyleParseProc
VFUNC(int,TixDItemStyleParseProc,V_TixDItemStyleParseProc,_ANSI_ARGS_((ClientData clientData,
		Tcl_Interp *interp, Tk_Window tkwin, Tcl_Obj * value,
		char *widRec, int offset)))
#endif /* #ifndef TixDItemStyleParseProc */

#ifndef TixDItemStylePrintProc
VFUNC(Tcl_Obj *,TixDItemStylePrintProc,V_TixDItemStylePrintProc,_ANSI_ARGS_((
		ClientData clientData, Tk_Window tkwin, char *widRec,
		int offset, Tcl_FreeProc **freeProcPtr)))
#endif /* #ifndef TixDItemStylePrintProc */

#ifndef TixGetStringFromObj
VFUNC(char *,TixGetStringFromObj,V_TixGetStringFromObj,_ANSI_ARGS_((
			    char *objPtr,int *lengthPtr)))
#endif /* #ifndef TixGetStringFromObj */

#ifndef Tix_ArgcError
VFUNC(int,Tix_ArgcError,V_Tix_ArgcError,_ANSI_ARGS_((Tcl_Interp *interp,
			    int argc, Tcl_Obj *CONST *objv, int prefixCount,
			    char *message)))
#endif /* #ifndef Tix_ArgcError */

#ifndef Tix_CreateSubWindow
VFUNC(Tk_Window,Tix_CreateSubWindow,V_Tix_CreateSubWindow,_ANSI_ARGS_((
			    Tcl_Interp * interp, Tk_Window tkwin,
			    char * subPath)))
#endif /* #ifndef Tix_CreateSubWindow */

#ifndef Tix_DrawAnchorLines
VFUNC(void,Tix_DrawAnchorLines,V_Tix_DrawAnchorLines,_ANSI_ARGS_((
			    Display *display, Drawable drawable,
			    GC gc, int x, int y, int w, int h)))
#endif /* #ifndef Tix_DrawAnchorLines */

#ifndef Tix_GetRenderBuffer
VFUNC(Pixmap,Tix_GetRenderBuffer,V_Tix_GetRenderBuffer,_ANSI_ARGS_((Display *display,
			    Drawable d, int width, int height, int depth)))
#endif /* #ifndef Tix_GetRenderBuffer */

#ifndef Tix_HandleSubCmds
VFUNC(int,Tix_HandleSubCmds,V_Tix_HandleSubCmds,_ANSI_ARGS_((
			    Tix_CmdInfo * cmdInfo,
			    Tix_SubCmdInfo * subCmdInfo,
			    ClientData clientData, Tcl_Interp *interp,
			    int argc, Tcl_Obj *CONST *objv)))
#endif /* #ifndef Tix_HandleSubCmds */

#ifndef Tix_LinkListAppend
VFUNC(void,Tix_LinkListAppend,V_Tix_LinkListAppend,_ANSI_ARGS_((Tix_ListInfo * infoPtr,
			    Tix_LinkList * lPtr, char * itemPtr, int flags)))
#endif /* #ifndef Tix_LinkListAppend */

#ifndef Tix_LinkListDelete
VFUNC(void,Tix_LinkListDelete,V_Tix_LinkListDelete,_ANSI_ARGS_((Tix_ListInfo * infoPtr,
			    Tix_LinkList * lPtr, Tix_ListIterator * liPtr)))
#endif /* #ifndef Tix_LinkListDelete */

#ifndef Tix_LinkListDeleteRange
VFUNC(int,Tix_LinkListDeleteRange,V_Tix_LinkListDeleteRange,_ANSI_ARGS_((
			    Tix_ListInfo * infoPtr, Tix_LinkList * lPtr,
			    char * fromPtr, char * toPtr,
			    Tix_ListIterator * liPtr)))
#endif /* #ifndef Tix_LinkListDeleteRange */

#ifndef Tix_LinkListFind
VFUNC(int,Tix_LinkListFind,V_Tix_LinkListFind,_ANSI_ARGS_((
			    Tix_ListInfo * infoPtr, Tix_LinkList * lPtr,
			    char * itemPtr, Tix_ListIterator * liPtr)))
#endif /* #ifndef Tix_LinkListFind */

#ifndef Tix_LinkListFindAndDelete
VFUNC(int,Tix_LinkListFindAndDelete,V_Tix_LinkListFindAndDelete,_ANSI_ARGS_((
			    Tix_ListInfo * infoPtr, Tix_LinkList * lPtr,
			    char * itemPtr, Tix_ListIterator * liPtr)))
#endif /* #ifndef Tix_LinkListFindAndDelete */

#ifndef Tix_LinkListInit
VFUNC(void,Tix_LinkListInit,V_Tix_LinkListInit,_ANSI_ARGS_((Tix_LinkList * lPtr)))
#endif /* #ifndef Tix_LinkListInit */

#ifndef Tix_LinkListInsert
VFUNC(void,Tix_LinkListInsert,V_Tix_LinkListInsert,_ANSI_ARGS_((
			    Tix_ListInfo * infoPtr,
			    Tix_LinkList * lPtr, char * itemPtr,
			    Tix_ListIterator * liPtr)))
#endif /* #ifndef Tix_LinkListInsert */

#ifndef Tix_LinkListIteratorInit
VFUNC(void,Tix_LinkListIteratorInit,V_Tix_LinkListIteratorInit,_ANSI_ARGS_(( Tix_ListIterator * liPtr)))
#endif /* #ifndef Tix_LinkListIteratorInit */

#ifndef Tix_LinkListNext
VFUNC(void,Tix_LinkListNext,V_Tix_LinkListNext,_ANSI_ARGS_((Tix_ListInfo * infoPtr,
			    Tix_LinkList * lPtr, Tix_ListIterator * liPtr)))
#endif /* #ifndef Tix_LinkListNext */

#ifndef Tix_LinkListStart
VFUNC(void,Tix_LinkListStart,V_Tix_LinkListStart,_ANSI_ARGS_((Tix_ListInfo * infoPtr,
			    Tix_LinkList * lPtr, Tix_ListIterator * liPtr)))
#endif /* #ifndef Tix_LinkListStart */

#ifndef Tix_SimpleListAppend
VFUNC(void,Tix_SimpleListAppend,V_Tix_SimpleListAppend,_ANSI_ARGS_((
			    Tix_LinkList * lPtr, char * itemPtr, int flags)))
#endif /* #ifndef Tix_SimpleListAppend */

#ifndef Tix_SimpleListDelete
VFUNC(void,Tix_SimpleListDelete,V_Tix_SimpleListDelete,_ANSI_ARGS_((
			    Tix_LinkList * lPtr, Tix_ListIterator * liPtr)))
#endif /* #ifndef Tix_SimpleListDelete */

#ifndef Tix_SimpleListDeleteRange
VFUNC(int,Tix_SimpleListDeleteRange,V_Tix_SimpleListDeleteRange,_ANSI_ARGS_((
			    Tix_LinkList * lPtr,
			    char * fromPtr, char * toPtr,
			    Tix_ListIterator * liPtr)))
#endif /* #ifndef Tix_SimpleListDeleteRange */

#ifndef Tix_SimpleListFind
VFUNC(int,Tix_SimpleListFind,V_Tix_SimpleListFind,_ANSI_ARGS_((
			    Tix_LinkList * lPtr,
			    char * itemPtr, Tix_ListIterator * liPtr)))
#endif /* #ifndef Tix_SimpleListFind */

#ifndef Tix_SimpleListFindAndDelete
VFUNC(int,Tix_SimpleListFindAndDelete,V_Tix_SimpleListFindAndDelete,_ANSI_ARGS_((
			    Tix_LinkList * lPtr, char * itemPtr,
			    Tix_ListIterator * liPtr)))
#endif /* #ifndef Tix_SimpleListFindAndDelete */

#ifndef Tix_SimpleListInit
VFUNC(void,Tix_SimpleListInit,V_Tix_SimpleListInit,_ANSI_ARGS_((Tix_LinkList * lPtr)))
#endif /* #ifndef Tix_SimpleListInit */

#ifndef Tix_SimpleListInsert
VFUNC(void,Tix_SimpleListInsert,V_Tix_SimpleListInsert,_ANSI_ARGS_((
			    Tix_LinkList * lPtr, char * itemPtr,
			    Tix_ListIterator * liPtr)))
#endif /* #ifndef Tix_SimpleListInsert */

#ifndef Tix_SimpleListIteratorInit
VFUNC(void,Tix_SimpleListIteratorInit,V_Tix_SimpleListIteratorInit,_ANSI_ARGS_((
			    Tix_ListIterator * liPtr)))
#endif /* #ifndef Tix_SimpleListIteratorInit */

#ifndef Tix_SimpleListNext
VFUNC(void,Tix_SimpleListNext,V_Tix_SimpleListNext,_ANSI_ARGS_((
			    Tix_LinkList * lPtr, Tix_ListIterator * liPtr)))
#endif /* #ifndef Tix_SimpleListNext */

#ifndef Tix_SimpleListStart
VFUNC(void,Tix_SimpleListStart,V_Tix_SimpleListStart,_ANSI_ARGS_((
			    Tix_LinkList * lPtr, Tix_ListIterator * liPtr)))
#endif /* #ifndef Tix_SimpleListStart */

#endif /* _TIX */
