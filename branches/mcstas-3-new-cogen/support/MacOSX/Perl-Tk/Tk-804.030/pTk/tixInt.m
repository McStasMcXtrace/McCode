#ifndef _TIXINT_VM
#define _TIXINT_VM
#include "tixInt_f.h"
#ifndef NO_VTABLES
#define tixCellUid (*TixintVptr->V_tixCellUid)
#define tixColumnUid (*TixintVptr->V_tixColumnUid)
#define tixDisabledUid (*TixintVptr->V_tixDisabledUid)
#define tixNormalUid (*TixintVptr->V_tixNormalUid)
#define tixRowUid (*TixintVptr->V_tixRowUid)
#ifndef TixComputeTextGeometry
#  define TixComputeTextGeometry (*TixintVptr->V_TixComputeTextGeometry)
#endif

#ifndef TixDItemGetAnchor
#  define TixDItemGetAnchor (*TixintVptr->V_TixDItemGetAnchor)
#endif

#ifndef TixDItemStyleChanged
#  define TixDItemStyleChanged (*TixintVptr->V_TixDItemStyleChanged)
#endif

#ifndef TixDItemStyleFree
#  define TixDItemStyleFree (*TixintVptr->V_TixDItemStyleFree)
#endif

#ifndef TixDisplayText
#  define TixDisplayText (*TixintVptr->V_TixDisplayText)
#endif

#ifndef TixGetColorDItemGC
#  define TixGetColorDItemGC (*TixintVptr->V_TixGetColorDItemGC)
#endif

#ifndef TixGetDefaultDItemStyle
#  define TixGetDefaultDItemStyle (*TixintVptr->V_TixGetDefaultDItemStyle)
#endif

#ifndef TixGetHashTable
#  define TixGetHashTable (*TixintVptr->V_TixGetHashTable)
#endif

#ifndef Tix_AddDItemType
#  define Tix_AddDItemType (*TixintVptr->V_Tix_AddDItemType)
#endif

#ifndef Tix_ConfigureInfo2
#  define Tix_ConfigureInfo2 (*TixintVptr->V_Tix_ConfigureInfo2)
#endif

#ifndef Tix_ConfigureValue2
#  define Tix_ConfigureValue2 (*TixintVptr->V_Tix_ConfigureValue2)
#endif

#ifndef Tix_DItemCalculateSize
#  define Tix_DItemCalculateSize (*TixintVptr->V_Tix_DItemCalculateSize)
#endif

#ifndef Tix_DItemComponent
#  define Tix_DItemComponent (*TixintVptr->V_Tix_DItemComponent)
#endif

#ifndef Tix_DItemConfigure
#  define Tix_DItemConfigure (*TixintVptr->V_Tix_DItemConfigure)
#endif

#ifndef Tix_DItemCreate
#  define Tix_DItemCreate (*TixintVptr->V_Tix_DItemCreate)
#endif

#ifndef Tix_DItemDisplay
#  define Tix_DItemDisplay (*TixintVptr->V_Tix_DItemDisplay)
#endif

#ifndef Tix_DItemDrawBackground
#  define Tix_DItemDrawBackground (*TixintVptr->V_Tix_DItemDrawBackground)
#endif

#ifndef Tix_DItemFree
#  define Tix_DItemFree (*TixintVptr->V_Tix_DItemFree)
#endif

#ifndef Tix_FreeArgumentList
#  define Tix_FreeArgumentList (*TixintVptr->V_Tix_FreeArgumentList)
#endif

#ifndef Tix_GetDItemType
#  define Tix_GetDItemType (*TixintVptr->V_Tix_GetDItemType)
#endif

#ifndef Tix_GetScrollFractions
#  define Tix_GetScrollFractions (*TixintVptr->V_Tix_GetScrollFractions)
#endif

#ifndef Tix_InitScrollInfo
#  define Tix_InitScrollInfo (*TixintVptr->V_Tix_InitScrollInfo)
#endif

#ifndef Tix_MultiConfigureInfo
#  define Tix_MultiConfigureInfo (*TixintVptr->V_Tix_MultiConfigureInfo)
#endif

#ifndef Tix_SetDefaultStyleTemplate
#  define Tix_SetDefaultStyleTemplate (*TixintVptr->V_Tix_SetDefaultStyleTemplate)
#endif

#ifndef Tix_SetScrollBarView
#  define Tix_SetScrollBarView (*TixintVptr->V_Tix_SetScrollBarView)
#endif

#ifndef Tix_SetWindowItemSerial
#  define Tix_SetWindowItemSerial (*TixintVptr->V_Tix_SetWindowItemSerial)
#endif

#ifndef Tix_SplitConfig
#  define Tix_SplitConfig (*TixintVptr->V_Tix_SplitConfig)
#endif

#ifndef Tix_UnmapInvisibleWindowItems
#  define Tix_UnmapInvisibleWindowItems (*TixintVptr->V_Tix_UnmapInvisibleWindowItems)
#endif

#ifndef Tix_UpdateScrollBar
#  define Tix_UpdateScrollBar (*TixintVptr->V_Tix_UpdateScrollBar)
#endif

#ifndef Tix_WidgetConfigure2
#  define Tix_WidgetConfigure2 (*TixintVptr->V_Tix_WidgetConfigure2)
#endif

#ifndef Tix_WindowItemListRemove
#  define Tix_WindowItemListRemove (*TixintVptr->V_Tix_WindowItemListRemove)
#endif

#ifndef TixpDrawAnchorLines
#  define TixpDrawAnchorLines (*TixintVptr->V_TixpDrawAnchorLines)
#endif

#ifndef TixpDrawTmpLine
#  define TixpDrawTmpLine (*TixintVptr->V_TixpDrawTmpLine)
#endif

#ifndef TixpEndSubRegionDraw
#  define TixpEndSubRegionDraw (*TixintVptr->V_TixpEndSubRegionDraw)
#endif

#ifndef TixpStartSubRegionDraw
#  define TixpStartSubRegionDraw (*TixintVptr->V_TixpStartSubRegionDraw)
#endif

#ifndef TixpSubRegDisplayText
#  define TixpSubRegDisplayText (*TixintVptr->V_TixpSubRegDisplayText)
#endif

#ifndef TixpSubRegDrawBitmap
#  define TixpSubRegDrawBitmap (*TixintVptr->V_TixpSubRegDrawBitmap)
#endif

#ifndef TixpSubRegDrawImage
#  define TixpSubRegDrawImage (*TixintVptr->V_TixpSubRegDrawImage)
#endif

#ifndef TixpSubRegFillRectangle
#  define TixpSubRegFillRectangle (*TixintVptr->V_TixpSubRegFillRectangle)
#endif

#ifndef tixStrDup
#  define tixStrDup (*TixintVptr->V_tixStrDup)
#endif

#endif /* NO_VTABLES */
#endif /* _TIXINT_VM */
