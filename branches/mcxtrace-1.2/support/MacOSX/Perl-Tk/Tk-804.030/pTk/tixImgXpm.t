#ifdef _TIXIMGXPM
#ifndef TixpInitPixmapInstance
VFUNC(void,TixpInitPixmapInstance,V_TixpInitPixmapInstance,_ANSI_ARGS_((
			    PixmapMaster *masterPtr,
			    PixmapInstance *instancePtr)))
#endif /* #ifndef TixpInitPixmapInstance */

#ifndef TixpXpmAllocTmpBuffer
VFUNC(void,TixpXpmAllocTmpBuffer,V_TixpXpmAllocTmpBuffer,_ANSI_ARGS_((
			    PixmapMaster * masterPtr,
			    PixmapInstance * instancePtr,
			    XImage ** imagePtr, XImage ** maskPtr)))
#endif /* #ifndef TixpXpmAllocTmpBuffer */

#ifndef TixpXpmDisplay
VFUNC(void,TixpXpmDisplay,V_TixpXpmDisplay,_ANSI_ARGS_((ClientData clientData,
			    Display *display, Drawable drawable,
			    int imageX, int imageY, int width, int height,
			    int drawableX, int drawableY)))
#endif /* #ifndef TixpXpmDisplay */

#ifndef TixpXpmFreeInstanceData
VFUNC(void,TixpXpmFreeInstanceData,V_TixpXpmFreeInstanceData,_ANSI_ARGS_((
			    PixmapInstance *instancePtr, int delete,
			    Display *display)))
#endif /* #ifndef TixpXpmFreeInstanceData */

#ifndef TixpXpmFreeTmpBuffer
VFUNC(void,TixpXpmFreeTmpBuffer,V_TixpXpmFreeTmpBuffer,_ANSI_ARGS_((
			    PixmapMaster * masterPtr,
			    PixmapInstance * instancePtr,
			    XImage * image, XImage * mask)))
#endif /* #ifndef TixpXpmFreeTmpBuffer */

#ifndef TixpXpmRealizePixmap
VFUNC(void,TixpXpmRealizePixmap,V_TixpXpmRealizePixmap,_ANSI_ARGS_((
			    PixmapMaster * masterPtr,
			    PixmapInstance * instancePtr,
			    XImage * image, XImage * mask, int isTransp)))
#endif /* #ifndef TixpXpmRealizePixmap */

#ifndef TixpXpmSetPixel
VFUNC(void,TixpXpmSetPixel,V_TixpXpmSetPixel,_ANSI_ARGS_((
			    PixmapInstance * instancePtr, XImage * image,
			    XImage * mask, int x, int y, XColor * colorPtr,
			    int * isTranspPtr)))
#endif /* #ifndef TixpXpmSetPixel */

#endif /* _TIXIMGXPM */
