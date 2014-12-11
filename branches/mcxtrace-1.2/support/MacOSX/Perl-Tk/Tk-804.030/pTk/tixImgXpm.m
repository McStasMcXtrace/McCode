#ifndef _TIXIMGXPM_VM
#define _TIXIMGXPM_VM
#include "tixImgXpm_f.h"
#ifndef NO_VTABLES
#ifndef TixpInitPixmapInstance
#  define TixpInitPixmapInstance (*TiximgxpmVptr->V_TixpInitPixmapInstance)
#endif

#ifndef TixpXpmAllocTmpBuffer
#  define TixpXpmAllocTmpBuffer (*TiximgxpmVptr->V_TixpXpmAllocTmpBuffer)
#endif

#ifndef TixpXpmDisplay
#  define TixpXpmDisplay (*TiximgxpmVptr->V_TixpXpmDisplay)
#endif

#ifndef TixpXpmFreeInstanceData
#  define TixpXpmFreeInstanceData (*TiximgxpmVptr->V_TixpXpmFreeInstanceData)
#endif

#ifndef TixpXpmFreeTmpBuffer
#  define TixpXpmFreeTmpBuffer (*TiximgxpmVptr->V_TixpXpmFreeTmpBuffer)
#endif

#ifndef TixpXpmRealizePixmap
#  define TixpXpmRealizePixmap (*TiximgxpmVptr->V_TixpXpmRealizePixmap)
#endif

#ifndef TixpXpmSetPixel
#  define TixpXpmSetPixel (*TiximgxpmVptr->V_TixpXpmSetPixel)
#endif

#endif /* NO_VTABLES */
#endif /* _TIXIMGXPM_VM */
