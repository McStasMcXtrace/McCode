#ifndef _TKIMGPHOTO_VM
#define _TKIMGPHOTO_VM
#include "tkImgPhoto_f.h"
#ifndef NO_VTABLES
#define tkImgFmtPPM (*TkimgphotoVptr->V_tkImgFmtPPM)
#ifndef Tk_CreateOldPhotoImageFormat
#  define Tk_CreateOldPhotoImageFormat (*TkimgphotoVptr->V_Tk_CreateOldPhotoImageFormat)
#endif

#ifndef Tk_CreatePhotoImageFormat
#  define Tk_CreatePhotoImageFormat (*TkimgphotoVptr->V_Tk_CreatePhotoImageFormat)
#endif

#ifndef Tk_DitherPhoto
#  define Tk_DitherPhoto (*TkimgphotoVptr->V_Tk_DitherPhoto)
#endif

#ifndef Tk_FindPhoto
#  define Tk_FindPhoto (*TkimgphotoVptr->V_Tk_FindPhoto)
#endif

#ifndef Tk_PhotoBlank
#  define Tk_PhotoBlank (*TkimgphotoVptr->V_Tk_PhotoBlank)
#endif

#ifndef Tk_PhotoExpand
#  define Tk_PhotoExpand (*TkimgphotoVptr->V_Tk_PhotoExpand)
#endif

#ifndef Tk_PhotoFormatName
#  define Tk_PhotoFormatName (*TkimgphotoVptr->V_Tk_PhotoFormatName)
#endif

#ifndef Tk_PhotoGetImage
#  define Tk_PhotoGetImage (*TkimgphotoVptr->V_Tk_PhotoGetImage)
#endif

#ifndef Tk_PhotoGetSize
#  define Tk_PhotoGetSize (*TkimgphotoVptr->V_Tk_PhotoGetSize)
#endif

#ifndef Tk_PhotoPutBlock
#  define Tk_PhotoPutBlock (*TkimgphotoVptr->V_Tk_PhotoPutBlock)
#endif

#ifndef Tk_PhotoPutZoomedBlock
#  define Tk_PhotoPutZoomedBlock (*TkimgphotoVptr->V_Tk_PhotoPutZoomedBlock)
#endif

#ifndef Tk_PhotoSetSize
#  define Tk_PhotoSetSize (*TkimgphotoVptr->V_Tk_PhotoSetSize)
#endif

#endif /* NO_VTABLES */
#endif /* _TKIMGPHOTO_VM */
