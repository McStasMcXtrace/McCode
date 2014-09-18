#ifndef _IMGINT_VM
#define _IMGINT_VM
#include "imgInt_f.h"
#ifndef NO_VTABLES
#ifndef ImgFixChanMatchProc
#  define ImgFixChanMatchProc (*ImgintVptr->V_ImgFixChanMatchProc)
#endif

#ifndef ImgFixObjMatchProc
#  define ImgFixObjMatchProc (*ImgintVptr->V_ImgFixObjMatchProc)
#endif

#ifndef ImgFixStringWriteProc
#  define ImgFixStringWriteProc (*ImgintVptr->V_ImgFixStringWriteProc)
#endif

#ifndef ImgGetByteArrayFromObj
#  define ImgGetByteArrayFromObj (*ImgintVptr->V_ImgGetByteArrayFromObj)
#endif

#ifndef ImgGetc
#  define ImgGetc (*ImgintVptr->V_ImgGetc)
#endif

#ifndef ImgListObjGetElements
#  define ImgListObjGetElements (*ImgintVptr->V_ImgListObjGetElements)
#endif

#ifndef ImgObjInit
#  define ImgObjInit (*ImgintVptr->V_ImgObjInit)
#endif

#ifndef ImgOpenFileChannel
#  define ImgOpenFileChannel (*ImgintVptr->V_ImgOpenFileChannel)
#endif

#ifndef ImgPhotoPutBlock
#  define ImgPhotoPutBlock (*ImgintVptr->V_ImgPhotoPutBlock)
#endif

#ifndef ImgPutc
#  define ImgPutc (*ImgintVptr->V_ImgPutc)
#endif

#ifndef ImgRead
#  define ImgRead (*ImgintVptr->V_ImgRead)
#endif

#ifndef ImgReadInit
#  define ImgReadInit (*ImgintVptr->V_ImgReadInit)
#endif

#ifndef ImgWrite
#  define ImgWrite (*ImgintVptr->V_ImgWrite)
#endif

#ifndef ImgWriteInit
#  define ImgWriteInit (*ImgintVptr->V_ImgWriteInit)
#endif

#endif /* NO_VTABLES */
#endif /* _IMGINT_VM */
