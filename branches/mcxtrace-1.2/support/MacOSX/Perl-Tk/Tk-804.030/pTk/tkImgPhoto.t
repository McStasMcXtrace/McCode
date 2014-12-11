#ifdef _TKIMGPHOTO
VVAR(Tk_PhotoImageFormat,tkImgFmtPPM,V_tkImgFmtPPM)
#ifndef Tk_CreateOldPhotoImageFormat
VFUNC(void,Tk_CreateOldPhotoImageFormat,V_Tk_CreateOldPhotoImageFormat,_ANSI_ARGS_((
				Tk_PhotoImageFormat *formatPtr)))
#endif /* #ifndef Tk_CreateOldPhotoImageFormat */

#ifndef Tk_CreatePhotoImageFormat
VFUNC(void,Tk_CreatePhotoImageFormat,V_Tk_CreatePhotoImageFormat,_ANSI_ARGS_((
			    Tk_PhotoImageFormat *formatPtr)))
#endif /* #ifndef Tk_CreatePhotoImageFormat */

#ifndef Tk_DitherPhoto
VFUNC(void,Tk_DitherPhoto,V_Tk_DitherPhoto,_ANSI_ARGS_((Tk_PhotoHandle handle,
				int x, int y, int width, int height)))
#endif /* #ifndef Tk_DitherPhoto */

#ifndef Tk_FindPhoto
VFUNC(Tk_PhotoHandle,Tk_FindPhoto,V_Tk_FindPhoto,_ANSI_ARGS_((Tcl_Interp *interp,
			    CONST char *imageName)))
#endif /* #ifndef Tk_FindPhoto */

#ifndef Tk_PhotoBlank
VFUNC(void,Tk_PhotoBlank,V_Tk_PhotoBlank,_ANSI_ARGS_((Tk_PhotoHandle handle)))
#endif /* #ifndef Tk_PhotoBlank */

#ifndef Tk_PhotoExpand
VFUNC(void,Tk_PhotoExpand,V_Tk_PhotoExpand,_ANSI_ARGS_((Tk_PhotoHandle handle,
			    int width, int height )))
#endif /* #ifndef Tk_PhotoExpand */

#ifndef Tk_PhotoFormatName
VFUNC(char *,Tk_PhotoFormatName,V_Tk_PhotoFormatName,_ANSI_ARGS_((Tcl_Interp *interp,
			    Tcl_Obj *formatString)))
#endif /* #ifndef Tk_PhotoFormatName */

#ifndef Tk_PhotoGetImage
VFUNC(int,Tk_PhotoGetImage,V_Tk_PhotoGetImage,_ANSI_ARGS_((Tk_PhotoHandle handle,
			    Tk_PhotoImageBlock *blockPtr)))
#endif /* #ifndef Tk_PhotoGetImage */

#ifndef Tk_PhotoGetSize
VFUNC(void,Tk_PhotoGetSize,V_Tk_PhotoGetSize,_ANSI_ARGS_((Tk_PhotoHandle handle,
			    int *widthPtr, int *heightPtr)))
#endif /* #ifndef Tk_PhotoGetSize */

#ifndef Tk_PhotoPutBlock
VFUNC(void,Tk_PhotoPutBlock,V_Tk_PhotoPutBlock,_ANSI_ARGS_((Tk_PhotoHandle handle,
			    Tk_PhotoImageBlock *blockPtr, int x, int y,
			    int width, int height, int compRule)))
#endif /* #ifndef Tk_PhotoPutBlock */

#ifndef Tk_PhotoPutZoomedBlock
VFUNC(void,Tk_PhotoPutZoomedBlock,V_Tk_PhotoPutZoomedBlock,_ANSI_ARGS_((
			    Tk_PhotoHandle handle,
			    Tk_PhotoImageBlock *blockPtr, int x, int y,
			    int width, int height, int zoomX, int zoomY,
			    int subsampleX, int subsampleY, int compRule)))
#endif /* #ifndef Tk_PhotoPutZoomedBlock */

#ifndef Tk_PhotoSetSize
VFUNC(void,Tk_PhotoSetSize,V_Tk_PhotoSetSize,_ANSI_ARGS_((Tk_PhotoHandle handle,
			    int width, int height)))
#endif /* #ifndef Tk_PhotoSetSize */

#endif /* _TKIMGPHOTO */
