#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>
#include "tkGlue.def"

#include "pTk/tixInt.h"
#include "pTk/tixImgXpm.h"
#include "pTk/tkVMacro.h"
#include "tkGlue.h"

extern Tix_DItemInfo tix_TextItemType;
extern Tix_DItemInfo tix_ImageTextItemType;
extern Tix_DItemInfo tix_ImageItemType;
extern Tix_DItemInfo tix_WindowItemType;


void
Boot_Tix (pTHX)
{
 install_vtab("TixVtab",TixVGet(),sizeof(TixVtab));
 install_vtab("TixintVtab",TixintVGet(),sizeof(TixintVtab));
 install_vtab("TiximgxpmVtab",TiximgxpmVGet(),sizeof(TiximgxpmVtab));
 tixNormalUid = Tk_GetUid("normal");
 tixDisabledUid = Tk_GetUid("disabled");
 tixCellUid     = Tk_GetUid("cell");
 tixRowUid      = Tk_GetUid("row");
 tixColumnUid   = Tk_GetUid("column");
 Tix_AddDItemType(&tix_TextItemType);
 Tix_AddDItemType(&tix_ImageItemType);
 Tix_AddDItemType(&tix_ImageTextItemType);
 Tix_AddDItemType(&tix_WindowItemType);
}

