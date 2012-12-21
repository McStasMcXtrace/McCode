#ifdef _IMGINT
#ifndef ImgFixChanMatchProc
#ifndef RESOURCE_INCLUDED
VFUNC(void,ImgFixChanMatchProc,V_ImgFixChanMatchProc,_ANSI_ARGS_((Tcl_Interp **interp, Tcl_Channel *chan,
	Tcl_Obj **file, Tcl_Obj **format, int **width, int **height)))
#endif /* #ifndef RESOURCE_INCLUDED */
#endif /* #ifndef ImgFixChanMatchProc */

#ifndef ImgFixObjMatchProc
#ifndef RESOURCE_INCLUDED
VFUNC(void,ImgFixObjMatchProc,V_ImgFixObjMatchProc,_ANSI_ARGS_((Tcl_Interp **interp, Tcl_Obj **data,
	Tcl_Obj **format, int **width, int **height)))
#endif /* #ifndef RESOURCE_INCLUDED */
#endif /* #ifndef ImgFixObjMatchProc */

#ifndef ImgFixStringWriteProc
#ifndef RESOURCE_INCLUDED
VFUNC(void,ImgFixStringWriteProc,V_ImgFixStringWriteProc,_ANSI_ARGS_((Tcl_DString *data, Tcl_Interp **interp,
	Tcl_DString **dataPtr, Tcl_Obj **format, Tk_PhotoImageBlock **blockPtr)))
#endif /* #ifndef RESOURCE_INCLUDED */
#endif /* #ifndef ImgFixStringWriteProc */

#ifndef ImgGetByteArrayFromObj
#ifndef RESOURCE_INCLUDED
VFUNC(char *,ImgGetByteArrayFromObj,V_ImgGetByteArrayFromObj,_ANSI_ARGS_((Tcl_Obj *objPtr,
	int *lengthPtr)))
#endif /* #ifndef RESOURCE_INCLUDED */
#endif /* #ifndef ImgGetByteArrayFromObj */

#ifndef ImgGetc
#ifndef RESOURCE_INCLUDED
VFUNC(int,ImgGetc,V_ImgGetc,_ANSI_ARGS_((MFile *handle)))
#endif /* #ifndef RESOURCE_INCLUDED */
#endif /* #ifndef ImgGetc */

#ifndef ImgListObjGetElements
#ifndef RESOURCE_INCLUDED
VFUNC(int,ImgListObjGetElements,V_ImgListObjGetElements,_ANSI_ARGS_((Tcl_Interp *interp,
	Tcl_Obj *objPtr, int *argc, Tcl_Obj ***objv)))
#endif /* #ifndef RESOURCE_INCLUDED */
#endif /* #ifndef ImgListObjGetElements */

#ifndef ImgObjInit
#ifndef RESOURCE_INCLUDED
VFUNC(int,ImgObjInit,V_ImgObjInit,_ANSI_ARGS_((Tcl_Interp *interp)))
#endif /* #ifndef RESOURCE_INCLUDED */
#endif /* #ifndef ImgObjInit */

#ifndef ImgOpenFileChannel
#ifndef RESOURCE_INCLUDED
VFUNC(Tcl_Channel,ImgOpenFileChannel,V_ImgOpenFileChannel,_ANSI_ARGS_((Tcl_Interp *interp,
	CONST char *fileName, int permissions)))
#endif /* #ifndef RESOURCE_INCLUDED */
#endif /* #ifndef ImgOpenFileChannel */

#ifndef ImgPhotoPutBlock
#ifndef RESOURCE_INCLUDED
VFUNC(int,ImgPhotoPutBlock,V_ImgPhotoPutBlock,_ANSI_ARGS_((Tk_PhotoHandle handle,
	Tk_PhotoImageBlock *blockPtr, int x, int y, int width, int height)))
#endif /* #ifndef RESOURCE_INCLUDED */
#endif /* #ifndef ImgPhotoPutBlock */

#ifndef ImgPutc
#ifndef RESOURCE_INCLUDED
VFUNC(int,ImgPutc,V_ImgPutc,_ANSI_ARGS_((int c, MFile *handle)))
#endif /* #ifndef RESOURCE_INCLUDED */
#endif /* #ifndef ImgPutc */

#ifndef ImgRead
#ifndef RESOURCE_INCLUDED
VFUNC(int,ImgRead,V_ImgRead,_ANSI_ARGS_((MFile *handle, char *dst, int count)))
#endif /* #ifndef RESOURCE_INCLUDED */
#endif /* #ifndef ImgRead */

#ifndef ImgReadInit
#ifndef RESOURCE_INCLUDED
VFUNC(int,ImgReadInit,V_ImgReadInit,_ANSI_ARGS_((Tcl_Obj *data, int c, MFile *handle)))
#endif /* #ifndef RESOURCE_INCLUDED */
#endif /* #ifndef ImgReadInit */

#ifndef ImgWrite
#ifndef RESOURCE_INCLUDED
VFUNC(int,ImgWrite,V_ImgWrite,_ANSI_ARGS_((MFile *handle, CONST char *src, int count)))
#endif /* #ifndef RESOURCE_INCLUDED */
#endif /* #ifndef ImgWrite */

#ifndef ImgWriteInit
#ifndef RESOURCE_INCLUDED
VFUNC(void,ImgWriteInit,V_ImgWriteInit,_ANSI_ARGS_((Tcl_DString *buffer, MFile *handle)))
#endif /* #ifndef RESOURCE_INCLUDED */
#endif /* #ifndef ImgWriteInit */

#endif /* _IMGINT */
