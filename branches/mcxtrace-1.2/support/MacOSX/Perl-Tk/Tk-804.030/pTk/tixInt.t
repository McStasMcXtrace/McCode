#ifdef _TIXINT
VVAR(Tk_Uid,tixCellUid,V_tixCellUid)
VVAR(Tk_Uid,tixColumnUid,V_tixColumnUid)
VVAR(Tk_Uid,tixDisabledUid,V_tixDisabledUid)
VVAR(Tk_Uid,tixNormalUid,V_tixNormalUid)
VVAR(Tk_Uid,tixRowUid,V_tixRowUid)
#ifndef TixComputeTextGeometry
VFUNC(void,TixComputeTextGeometry,V_TixComputeTextGeometry,_ANSI_ARGS_((
			    TixFont fontStructPtr, char *string,
			    int numChars, int wrapLength, int *widthPtr,
			    int *heightPtr)))
#endif /* #ifndef TixComputeTextGeometry */

#ifndef TixDItemGetAnchor
VFUNC(void,TixDItemGetAnchor,V_TixDItemGetAnchor,_ANSI_ARGS_((Tk_Anchor anchor,
			    int x, int y, int cav_w, int cav_h,
			    int width, int height, int * x_ret, int * y_ret)))
#endif /* #ifndef TixDItemGetAnchor */

#ifndef TixDItemStyleChanged
VFUNC(void,TixDItemStyleChanged,V_TixDItemStyleChanged,_ANSI_ARGS_((
			    Tix_DItemInfo * diTypePtr,
			    Tix_DItemStyle * stylePtr)))
#endif /* #ifndef TixDItemStyleChanged */

#ifndef TixDItemStyleFree
VFUNC(void,TixDItemStyleFree,V_TixDItemStyleFree,_ANSI_ARGS_((Tix_DItem *iPtr,
			    Tix_DItemStyle * stylePtr)))
#endif /* #ifndef TixDItemStyleFree */

#ifndef TixDisplayText
VFUNC(void,TixDisplayText,V_TixDisplayText,_ANSI_ARGS_((Display *display,
			    Drawable drawable, TixFont font,
			    char *string, int numChars, int x, int y,
			    int length, Tk_Justify justify, int underline,
			    GC gc)))
#endif /* #ifndef TixDisplayText */

#ifndef TixGetColorDItemGC
VFUNC(void,TixGetColorDItemGC,V_TixGetColorDItemGC,_ANSI_ARGS_((
			    Tix_DItem * iPtr, GC * backGC_ret,
			    GC * foreGC_ret, int flags)))
#endif /* #ifndef TixGetColorDItemGC */

#ifndef TixGetDefaultDItemStyle
VFUNC(Tix_DItemStyle*,TixGetDefaultDItemStyle,V_TixGetDefaultDItemStyle,_ANSI_ARGS_((
			    Tix_DispData * ddPtr, Tix_DItemInfo * diTypePtr,
			    Tix_DItem *iPtr, Tix_DItemStyle* oldStylePtr)))
#endif /* #ifndef TixGetDefaultDItemStyle */

#ifndef TixGetHashTable
VFUNC(Tcl_HashTable *,TixGetHashTable,V_TixGetHashTable,_ANSI_ARGS_((Tcl_Interp * interp,
			    char * name, Tcl_InterpDeleteProc *deleteProc)))
#endif /* #ifndef TixGetHashTable */

#ifndef Tix_AddDItemType
VFUNC(void,Tix_AddDItemType,V_Tix_AddDItemType,_ANSI_ARGS_((
			    Tix_DItemInfo * diTypePtr)))
#endif /* #ifndef Tix_AddDItemType */

#ifndef Tix_ConfigureInfo2
VFUNC(int,Tix_ConfigureInfo2,V_Tix_ConfigureInfo2,_ANSI_ARGS_((
			    Tcl_Interp *interp, Tk_Window tkwin,
			    char *entRec, Tk_ConfigSpec *entConfigSpecs,
			    Tix_DItem * iPtr, char *argvName, int flags)))
#endif /* #ifndef Tix_ConfigureInfo2 */

#ifndef Tix_ConfigureValue2
VFUNC(int,Tix_ConfigureValue2,V_Tix_ConfigureValue2,_ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Window tkwin, char * entRec,
			    Tk_ConfigSpec *entConfigSpecs, Tix_DItem * iPtr,
			    char *argvName, int flags)))
#endif /* #ifndef Tix_ConfigureValue2 */

#ifndef Tix_DItemCalculateSize
VFUNC(void,Tix_DItemCalculateSize,V_Tix_DItemCalculateSize,_ANSI_ARGS_((
			    Tix_DItem * iPtr)))
#endif /* #ifndef Tix_DItemCalculateSize */

#ifndef Tix_DItemComponent
VFUNC(char *,Tix_DItemComponent,V_Tix_DItemComponent,_ANSI_ARGS_((Tix_DItem * diPtr,
			    int x, int y)))
#endif /* #ifndef Tix_DItemComponent */

#ifndef Tix_DItemConfigure
VFUNC(int,Tix_DItemConfigure,V_Tix_DItemConfigure,_ANSI_ARGS_((
			    Tix_DItem * diPtr, int argc,
			    Tcl_Obj *CONST *objv, int flags)))
#endif /* #ifndef Tix_DItemConfigure */

#ifndef Tix_DItemCreate
VFUNC(Tix_DItem *,Tix_DItemCreate,V_Tix_DItemCreate,_ANSI_ARGS_((Tix_DispData * ddPtr,
			    char * type)))
#endif /* #ifndef Tix_DItemCreate */

#ifndef Tix_DItemDisplay
VFUNC(void,Tix_DItemDisplay,V_Tix_DItemDisplay,_ANSI_ARGS_((
			    Pixmap pixmap, GC gc, Tix_DItem * iPtr,
			    int x, int y, int width, int height, int flag)))
#endif /* #ifndef Tix_DItemDisplay */

#ifndef Tix_DItemDrawBackground
VFUNC(void,Tix_DItemDrawBackground,V_Tix_DItemDrawBackground,_ANSI_ARGS_((
			    Pixmap pixmap, GC gc, Tix_DItem * iPtr,
			    int x, int y, int width, int height, int flags)))
#endif /* #ifndef Tix_DItemDrawBackground */

#ifndef Tix_DItemFree
VFUNC(void,Tix_DItemFree,V_Tix_DItemFree,_ANSI_ARGS_((
			    Tix_DItem * iPtr)))
#endif /* #ifndef Tix_DItemFree */

#ifndef Tix_FreeArgumentList
VFUNC(void,Tix_FreeArgumentList,V_Tix_FreeArgumentList,_ANSI_ARGS_((
			    Tix_ArgumentList *argListPtr)))
#endif /* #ifndef Tix_FreeArgumentList */

#ifndef Tix_GetDItemType
VFUNC(Tix_DItemInfo *,Tix_GetDItemType,V_Tix_GetDItemType,_ANSI_ARGS_((
			    Tcl_Interp * interp, char *type)))
#endif /* #ifndef Tix_GetDItemType */

#ifndef Tix_GetScrollFractions
VFUNC(void,Tix_GetScrollFractions,V_Tix_GetScrollFractions,_ANSI_ARGS_((
			    Tix_ScrollInfo * siPtr,
			    double * first_ret, double * last_ret)))
#endif /* #ifndef Tix_GetScrollFractions */

#ifndef Tix_InitScrollInfo
VFUNC(void,Tix_InitScrollInfo,V_Tix_InitScrollInfo,_ANSI_ARGS_((
			    Tix_ScrollInfo * siPtr, int type)))
#endif /* #ifndef Tix_InitScrollInfo */

#ifndef Tix_MultiConfigureInfo
VFUNC(int,Tix_MultiConfigureInfo,V_Tix_MultiConfigureInfo,_ANSI_ARGS_((
			    Tcl_Interp * interp,
			    Tk_Window tkwin, Tk_ConfigSpec **specsList,
			    int numLists, char **widgRecList, char *argvName,
			    int flags, int request)))
#endif /* #ifndef Tix_MultiConfigureInfo */

#ifndef Tix_SetDefaultStyleTemplate
VFUNC(void,Tix_SetDefaultStyleTemplate,V_Tix_SetDefaultStyleTemplate,_ANSI_ARGS_((
			    Tk_Window tkwin, Tix_StyleTemplate * tmplPtr)))
#endif /* #ifndef Tix_SetDefaultStyleTemplate */

#ifndef Tix_SetScrollBarView
VFUNC(int,Tix_SetScrollBarView,V_Tix_SetScrollBarView,_ANSI_ARGS_((
			    Tcl_Interp *interp, Tix_ScrollInfo * siPtr,
			    int argc, Tcl_Obj *CONST *objv, int compat)))
#endif /* #ifndef Tix_SetScrollBarView */

#ifndef Tix_SetWindowItemSerial
VFUNC(void,Tix_SetWindowItemSerial,V_Tix_SetWindowItemSerial,_ANSI_ARGS_((
			    Tix_LinkList * lPtr, Tix_DItem * iPtr,
			    int serial)))
#endif /* #ifndef Tix_SetWindowItemSerial */

#ifndef Tix_SplitConfig
VFUNC(int,Tix_SplitConfig,V_Tix_SplitConfig,_ANSI_ARGS_((Tcl_Interp * interp,
			    Tk_Window tkwin, Tk_ConfigSpec  ** specsList,
			    int numLists, int argc, Tcl_Obj *CONST *objv,
			    Tix_ArgumentList * argListPtr)))
#endif /* #ifndef Tix_SplitConfig */

#ifndef Tix_UnmapInvisibleWindowItems
VFUNC(void,Tix_UnmapInvisibleWindowItems,V_Tix_UnmapInvisibleWindowItems,_ANSI_ARGS_((
			    Tix_LinkList * lPtr, int serial)))
#endif /* #ifndef Tix_UnmapInvisibleWindowItems */

#ifndef Tix_UpdateScrollBar
VFUNC(void,Tix_UpdateScrollBar,V_Tix_UpdateScrollBar,_ANSI_ARGS_((
			    Tcl_Interp *interp, Tix_ScrollInfo * siPtr)))
#endif /* #ifndef Tix_UpdateScrollBar */

#ifndef Tix_WidgetConfigure2
VFUNC(int,Tix_WidgetConfigure2,V_Tix_WidgetConfigure2,_ANSI_ARGS_((
			    Tcl_Interp *interp, Tk_Window tkwin, char * entRec,
			    Tk_ConfigSpec *entConfigSpecs,
			    Tix_DItem * iPtr, int argc, Tcl_Obj *CONST *objv,
			    int flags, int forced, int * sizeChanged_ret)))
#endif /* #ifndef Tix_WidgetConfigure2 */

#ifndef Tix_WindowItemListRemove
VFUNC(void,Tix_WindowItemListRemove,V_Tix_WindowItemListRemove,_ANSI_ARGS_((
			    Tix_LinkList * lPtr, Tix_DItem * iPtr)))
#endif /* #ifndef Tix_WindowItemListRemove */

#ifndef TixpDrawAnchorLines
VFUNC(void,TixpDrawAnchorLines,V_TixpDrawAnchorLines,_ANSI_ARGS_((Display *display,
			    Drawable drawable, GC gc, int x, int y,
			    int w, int h)))
#endif /* #ifndef TixpDrawAnchorLines */

#ifndef TixpDrawTmpLine
VFUNC(void,TixpDrawTmpLine,V_TixpDrawTmpLine,_ANSI_ARGS_((int x1, int y1,
			    int x2, int y2, Tk_Window tkwin)))
#endif /* #ifndef TixpDrawTmpLine */

#ifndef TixpEndSubRegionDraw
VFUNC(void,TixpEndSubRegionDraw,V_TixpEndSubRegionDraw,_ANSI_ARGS_((Display *display,
			     Drawable drawable, GC gc,
			     TixpSubRegion * subRegPtr)))
#endif /* #ifndef TixpEndSubRegionDraw */

#ifndef TixpStartSubRegionDraw
VFUNC(void,TixpStartSubRegionDraw,V_TixpStartSubRegionDraw,_ANSI_ARGS_((Tix_DispData *ddPtr,
			     Drawable drawable, GC gc,
			     TixpSubRegion * subRegPtr, int origX,
			     int origY, int x, int y, int width, int height,
			     int needWidth, int needHeight)))
#endif /* #ifndef TixpStartSubRegionDraw */

#ifndef TixpSubRegDisplayText
VFUNC(void,TixpSubRegDisplayText,V_TixpSubRegDisplayText,_ANSI_ARGS_((Display *display,
			    Drawable drawable, GC gc,
			    TixpSubRegion * subRegPtr,
			    TixFont font, char *string,
			    int numChars, int x, int y, int length,
			    Tk_Justify justify, int underline)))
#endif /* #ifndef TixpSubRegDisplayText */

#ifndef TixpSubRegDrawBitmap
VFUNC(void,TixpSubRegDrawBitmap,V_TixpSubRegDrawBitmap,_ANSI_ARGS_((Display *display,
			    Drawable drawable, GC gc,
			    TixpSubRegion * subRegPtr, Pixmap bitmap,
			    int src_x, int src_y, int width, int height,
			    int dest_x, int dest_y, unsigned long plane)))
#endif /* #ifndef TixpSubRegDrawBitmap */

#ifndef TixpSubRegDrawImage
VFUNC(void,TixpSubRegDrawImage,V_TixpSubRegDrawImage,_ANSI_ARGS_((
			    TixpSubRegion * subRegPtr, Tk_Image image,
			    int imageX, int imageY, int width, int height,
			    Drawable drawable, int drawableX, int drawableY)))
#endif /* #ifndef TixpSubRegDrawImage */

#ifndef TixpSubRegFillRectangle
VFUNC(void,TixpSubRegFillRectangle,V_TixpSubRegFillRectangle,_ANSI_ARGS_((Display *display,
			    Drawable drawable, GC gc,
			    TixpSubRegion * subRegPtr, int x, int y,
			    int width, int height)))
#endif /* #ifndef TixpSubRegFillRectangle */

#ifndef tixStrDup
VFUNC(char *,tixStrDup,V_tixStrDup,_ANSI_ARGS_(( CONST char * s)))
#endif /* #ifndef tixStrDup */

#endif /* _TIXINT */
