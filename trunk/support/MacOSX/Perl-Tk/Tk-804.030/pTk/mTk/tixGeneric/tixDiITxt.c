
/*	$Id: tixDiITxt.c,v 1.2 2000/10/12 02:54:04 idiscovery Exp $	*/

/*
 * tixDiImgTxt.c --
 *
 *	This file implements one of the "Display Items" in the Tix library :
 *	Image-text display items.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include "tixPort.h"
#include "tk.h"
#include "tixInt.h"
#include "tixDef.h"

#define DEF_IMAGETEXTITEM_BITMAP	""
#define DEF_IMAGETEXTITEM_IMAGE		""
#define DEF_IMAGETEXTITEM_TYPE		"imagetext"
#define DEF_IMAGETEXTITEM_SHOWIMAGE	"1"
#define DEF_IMAGETEXTITEM_SHOWTEXT	"1"
#define DEF_IMAGETEXTITEM_STYLE		""
#define DEF_IMAGETEXTITEM_TEXT		""
#define DEF_IMAGETEXTITEM_UNDERLINE	"-1"

static Tk_ConfigSpec imageTextItemConfigSpecs[] = {

    {TK_CONFIG_BITMAP, "-bitmap", "bitmap", "Bitmap",
       DEF_IMAGETEXTITEM_BITMAP, Tk_Offset(TixImageTextItem, bitmap),
       TK_CONFIG_NULL_OK},

    {TK_CONFIG_OBJECT, "-image", "image", "Image",
       DEF_IMAGETEXTITEM_IMAGE, Tk_Offset(TixImageTextItem, imageString),
       TK_CONFIG_NULL_OK},

    {TK_CONFIG_CUSTOM, "-itemtype", "itemType", "ItemType",
       DEF_IMAGETEXTITEM_TYPE, Tk_Offset(TixImageTextItem, diTypePtr),
       0, &tixConfigItemType},

    {TK_CONFIG_INT, "-showimage", "showImage", "ShowImage",
	DEF_IMAGETEXTITEM_SHOWIMAGE, Tk_Offset(TixImageTextItem, showImage), 0},

    {TK_CONFIG_INT, "-showtext", "showText", "ShowText",
	DEF_IMAGETEXTITEM_SHOWTEXT, Tk_Offset(TixImageTextItem, showText), 0},

    {TK_CONFIG_CUSTOM, "-style", "imageTextStyle", "ImageTextStyle",
       DEF_IMAGETEXTITEM_STYLE, Tk_Offset(TixImageTextItem, stylePtr),
       TK_CONFIG_NULL_OK, &tixConfigItemStyle},

    {TK_CONFIG_LANGARG, "-text", "text", "Text",
       DEF_IMAGETEXTITEM_TEXT, Tk_Offset(TixImageTextItem, text),
       TK_CONFIG_NULL_OK},

    {TK_CONFIG_INT, "-underline", "underline", "Underline",
       DEF_IMAGETEXTITEM_UNDERLINE, Tk_Offset(TixImageTextItem, underline), 0},

    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
       (char *) NULL, 0, 0}
};

/*----------------------------------------------------------------------
 *
 *		Configuration options for Text Styles
 *
 *----------------------------------------------------------------------
 */


#define SELECTED_BG SELECT_BG
#define DISABLED_BG DISABLED

#define DEF_IMAGETEXTSTYLE_NORMAL_FG_COLOR	BLACK
#define DEF_IMAGETEXTSTYLE_NORMAL_FG_MONO	BLACK
#define DEF_IMAGETEXTSTYLE_NORMAL_BG_COLOR	NORMAL_BG
#define DEF_IMAGETEXTSTYLE_NORMAL_BG_MONO	WHITE

#define DEF_IMAGETEXTSTYLE_ACTIVE_FG_COLOR	BLACK
#define DEF_IMAGETEXTSTYLE_ACTIVE_FG_MONO	WHITE
#define DEF_IMAGETEXTSTYLE_ACTIVE_BG_COLOR	ACTIVE_BG
#define DEF_IMAGETEXTSTYLE_ACTIVE_BG_MONO	BLACK

#define DEF_IMAGETEXTSTYLE_SELECTED_FG_COLOR	BLACK
#define DEF_IMAGETEXTSTYLE_SELECTED_FG_MONO	WHITE
#define DEF_IMAGETEXTSTYLE_SELECTED_BG_COLOR	SELECTED_BG
#define DEF_IMAGETEXTSTYLE_SELECTED_BG_MONO	BLACK

#define DEF_IMAGETEXTSTYLE_DISABLED_FG_COLOR	BLACK
#define DEF_IMAGETEXTSTYLE_DISABLED_FG_MONO	BLACK
#define DEF_IMAGETEXTSTYLE_DISABLED_BG_COLOR	DISABLED_BG
#define DEF_IMAGETEXTSTYLE_DISABLED_BG_MONO	WHITE

#define DEF_IMAGETEXTSTYLE_FONT		CTL_FONT
#define DEF_IMAGETEXTSTYLE_GAP		"4"
#define DEF_IMAGETEXTSTYLE_PADX		"2"
#define DEF_IMAGETEXTSTYLE_PADY		"2"
#define DEF_IMAGETEXTSTYLE_JUSTIFY	"left"
#define DEF_IMAGETEXTSTYLE_WLENGTH	"0"
#define DEF_IMAGETEXTSTYLE_ANCHOR	"w"
#define DEF_IMAGETEXTSTYLE_TEXTANCHOR	"e"


static Tk_ConfigSpec imageTextStyleConfigSpecs[] = {
    {TK_CONFIG_ANCHOR, "-anchor", "anchor", "Anchor",
       DEF_IMAGETEXTSTYLE_ANCHOR, Tk_Offset(TixImageTextStyle, anchor), 0},

    {TK_CONFIG_SYNONYM, "-bg", "background", (char *) NULL,
       (char *) NULL, 0, 0},
    {TK_CONFIG_SYNONYM, "-fg", "foreground", (char *) NULL,
       (char *) NULL, 0, 0},

    {TK_CONFIG_FONT, "-font", "font", "Font",
       DEF_IMAGETEXTSTYLE_FONT, Tk_Offset(TixImageTextStyle, font), 0},

    {TK_CONFIG_PIXELS, "-gap", "gap", "Gap",
       DEF_IMAGETEXTSTYLE_GAP, Tk_Offset(TixImageTextStyle, gap), 0},

    {TK_CONFIG_JUSTIFY, "-justify", "justify", "Justyfy",
       DEF_IMAGETEXTSTYLE_JUSTIFY, Tk_Offset(TixImageTextStyle, justify),
       TK_CONFIG_NULL_OK},

    {TK_CONFIG_PIXELS, "-padx", "padX", "Pad",
       DEF_IMAGETEXTSTYLE_PADX, Tk_Offset(TixImageTextStyle, pad[0]), 0},

    {TK_CONFIG_PIXELS, "-pady", "padY", "Pad",
       DEF_IMAGETEXTSTYLE_PADY, Tk_Offset(TixImageTextStyle, pad[1]), 0},

    {TK_CONFIG_ANCHOR, "-textanchor", "textAnchor", "TextAnchor",
       DEF_IMAGETEXTSTYLE_TEXTANCHOR, Tk_Offset(TixImageTextStyle, textanchor), 0},

    {TK_CONFIG_PIXELS, "-wraplength", "wrapLength", "WrapLength",
       DEF_IMAGETEXTSTYLE_WLENGTH, Tk_Offset(TixImageTextStyle, wrapLength),
       0},

/* The following is automatically generated */
	{TK_CONFIG_COLOR,"-background","background","Background",
	DEF_IMAGETEXTSTYLE_NORMAL_BG_COLOR,
	Tk_Offset(TixImageTextStyle,colors[TIX_DITEM_NORMAL].bg),
	TK_CONFIG_COLOR_ONLY},
	{TK_CONFIG_COLOR,"-background","background","Background",
	DEF_IMAGETEXTSTYLE_NORMAL_BG_MONO,
	Tk_Offset(TixImageTextStyle,colors[TIX_DITEM_NORMAL].bg),
	TK_CONFIG_MONO_ONLY},
	{TK_CONFIG_COLOR,"-foreground","foreground","Foreground",
	DEF_IMAGETEXTSTYLE_NORMAL_FG_COLOR,
	Tk_Offset(TixImageTextStyle,colors[TIX_DITEM_NORMAL].fg),
	TK_CONFIG_COLOR_ONLY},
	{TK_CONFIG_COLOR,"-foreground","foreground","Foreground",
	DEF_IMAGETEXTSTYLE_NORMAL_FG_MONO,
	Tk_Offset(TixImageTextStyle,colors[TIX_DITEM_NORMAL].fg),
	TK_CONFIG_MONO_ONLY},
	{TK_CONFIG_COLOR,"-activebackground","activeBackground","ActiveBackground",
	DEF_IMAGETEXTSTYLE_ACTIVE_BG_COLOR,
	Tk_Offset(TixImageTextStyle,colors[TIX_DITEM_ACTIVE].bg),
	TK_CONFIG_COLOR_ONLY},
	{TK_CONFIG_COLOR,"-activebackground","activeBackground","ActiveBackground",
	DEF_IMAGETEXTSTYLE_ACTIVE_BG_MONO,
	Tk_Offset(TixImageTextStyle,colors[TIX_DITEM_ACTIVE].bg),
	TK_CONFIG_MONO_ONLY},
	{TK_CONFIG_COLOR,"-activeforeground","activeForeground","ActiveForeground",
	DEF_IMAGETEXTSTYLE_ACTIVE_FG_COLOR,
	Tk_Offset(TixImageTextStyle,colors[TIX_DITEM_ACTIVE].fg),
	TK_CONFIG_COLOR_ONLY},
	{TK_CONFIG_COLOR,"-activeforeground","activeForeground","ActiveForeground",
	DEF_IMAGETEXTSTYLE_ACTIVE_FG_MONO,
	Tk_Offset(TixImageTextStyle,colors[TIX_DITEM_ACTIVE].fg),
	TK_CONFIG_MONO_ONLY},
	{TK_CONFIG_COLOR,"-selectbackground","selectBackground","SelectBackground",
	DEF_IMAGETEXTSTYLE_SELECTED_BG_COLOR,
	Tk_Offset(TixImageTextStyle,colors[TIX_DITEM_SELECTED].bg),
	TK_CONFIG_COLOR_ONLY},
	{TK_CONFIG_COLOR,"-selectbackground","selectBackground","SelectBackground",
	DEF_IMAGETEXTSTYLE_SELECTED_BG_MONO,
	Tk_Offset(TixImageTextStyle,colors[TIX_DITEM_SELECTED].bg),
	TK_CONFIG_MONO_ONLY},
	{TK_CONFIG_COLOR,"-selectforeground","selectForeground","SelectForeground",
	DEF_IMAGETEXTSTYLE_SELECTED_FG_COLOR,
	Tk_Offset(TixImageTextStyle,colors[TIX_DITEM_SELECTED].fg),
	TK_CONFIG_COLOR_ONLY},
	{TK_CONFIG_COLOR,"-selectforeground","selectForeground","SelectForeground",
	DEF_IMAGETEXTSTYLE_SELECTED_FG_MONO,
	Tk_Offset(TixImageTextStyle,colors[TIX_DITEM_SELECTED].fg),
	TK_CONFIG_MONO_ONLY},
	{TK_CONFIG_COLOR,"-disabledbackground","disabledBackground","DisabledBackground",
	DEF_IMAGETEXTSTYLE_DISABLED_BG_COLOR,
	Tk_Offset(TixImageTextStyle,colors[TIX_DITEM_DISABLED].bg),
	TK_CONFIG_COLOR_ONLY},
	{TK_CONFIG_COLOR,"-disabledbackground","disabledBackground","DisabledBackground",
	DEF_IMAGETEXTSTYLE_DISABLED_BG_MONO,
	Tk_Offset(TixImageTextStyle,colors[TIX_DITEM_DISABLED].bg),
	TK_CONFIG_MONO_ONLY},
	{TK_CONFIG_COLOR,"-disabledforeground","disabledForeground","DisabledForeground",
	DEF_IMAGETEXTSTYLE_DISABLED_FG_COLOR,
	Tk_Offset(TixImageTextStyle,colors[TIX_DITEM_DISABLED].fg),
	TK_CONFIG_COLOR_ONLY},
	{TK_CONFIG_COLOR,"-disabledforeground","disabledForeground","DisabledForeground",
	DEF_IMAGETEXTSTYLE_DISABLED_FG_MONO,
	Tk_Offset(TixImageTextStyle,colors[TIX_DITEM_DISABLED].fg),
	TK_CONFIG_MONO_ONLY},

    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
       (char *) NULL, 0, 0}
};

/*----------------------------------------------------------------------
 * Forward declarations for procedures defined later in this file:
 *----------------------------------------------------------------------
 */
static void		ImageProc _ANSI_ARGS_((ClientData clientData,
			    int x, int y, int width, int height,
			    int imgWidth, int imgHeight));
static void		Tix_ImageTextItemCalculateSize	_ANSI_ARGS_((
			    Tix_DItem * iPtr));
static char *		Tix_ImageTextItemComponent  _ANSI_ARGS_((
			    Tix_DItem * iPtr, int x, int y));
static int		Tix_ImageTextItemConfigure _ANSI_ARGS_((
			    Tix_DItem * iPtr, int argc, char ** argv,
			    int flags));
static Tix_DItem *	Tix_ImageTextItemCreate _ANSI_ARGS_((
			    Tix_DispData * ddPtr, Tix_DItemInfo * diTypePtr));
static void		Tix_ImageTextItemDisplay  _ANSI_ARGS_((
			    Pixmap pixmap, GC gc, Tix_DItem * iPtr,
			    int x, int y, int width, int height, int flag));
static void		Tix_ImageTextItemFree  _ANSI_ARGS_((
			    Tix_DItem * iPtr));
static void		Tix_ImageTextItemLostStyle  _ANSI_ARGS_((
			    Tix_DItem * iPtr));
static void		Tix_ImageTextItemStyleChanged  _ANSI_ARGS_((
			    Tix_DItem * iPtr));
static int		Tix_ImageTextStyleConfigure _ANSI_ARGS_((
			    Tix_DItemStyle* style, int argc, char ** argv,
			    int flags));
static Tix_DItemStyle *	Tix_ImageTextStyleCreate _ANSI_ARGS_((
			    Tcl_Interp *interp, Tk_Window tkwin,
			    Tix_DItemInfo * diTypePtr, char * name));
static void		Tix_ImageTextStyleFree _ANSI_ARGS_((
			    Tix_DItemStyle* style));
static void		Tix_ImageTextStyleSetTemplate _ANSI_ARGS_((
			    Tix_DItemStyle* style,
			    Tix_StyleTemplate * tmplPtr));

Tix_DItemInfo tix_ImageTextItemType = {
    "imagetext",			/* type */
    TIX_DITEM_IMAGETEXT,
    Tix_ImageTextItemCreate,		/* createProc */
    Tix_ImageTextItemConfigure,
    Tix_ImageTextItemCalculateSize,
    Tix_ImageTextItemComponent,
    Tix_ImageTextItemDisplay,
    Tix_ImageTextItemFree,
    Tix_ImageTextItemStyleChanged,
    Tix_ImageTextItemLostStyle,

    Tix_ImageTextStyleCreate,
    Tix_ImageTextStyleConfigure,
    Tix_ImageTextStyleFree,
    Tix_ImageTextStyleSetTemplate,

    imageTextItemConfigSpecs,
    imageTextStyleConfigSpecs,
    NULL,				/*next */
};


/*----------------------------------------------------------------------
 * Tix_ImageText --
 *
 *
 *----------------------------------------------------------------------
 */
static Tix_DItem * Tix_ImageTextItemCreate(ddPtr, diTypePtr)
    Tix_DispData * ddPtr;
    Tix_DItemInfo * diTypePtr;
{
    TixImageTextItem * itPtr;

    itPtr = (TixImageTextItem*) ckalloc(sizeof(TixImageTextItem));

    itPtr->diTypePtr	= diTypePtr;
    itPtr->ddPtr	= ddPtr;
    itPtr->stylePtr	= NULL;
    itPtr->clientData	= 0;
    itPtr->size[0]	= 0;
    itPtr->size[1]	= 0;

    itPtr->bitmap	= None;
    itPtr->bitmapW	= 0;
    itPtr->bitmapH	= 0;

    itPtr->imageString	= NULL;
    itPtr->image	= NULL;
    itPtr->imageW	= 0;
    itPtr->imageH	= 0;

    itPtr->numChars	= 0;
    itPtr->text		= NULL;
    itPtr->textW	= 0;
    itPtr->textH	= 0;
    itPtr->underline	= -1;

    itPtr->showImage	= 1;
    itPtr->showText	= 1;

    return (Tix_DItem *)itPtr;
}

static void Tix_ImageTextItemFree(iPtr)
    Tix_DItem * iPtr;
{
    TixImageTextItem * itPtr = (TixImageTextItem *) iPtr;

    if (itPtr->image) {
	Tk_FreeImage(itPtr->image);
    }
    if (itPtr->stylePtr) {
	TixDItemStyleFree(iPtr, (Tix_DItemStyle*)itPtr->stylePtr);
    }

    Tk_FreeOptions(imageTextItemConfigSpecs, (char *)itPtr,
	itPtr->ddPtr->display, 0);
    ckfree((char*)itPtr);
}

static int Tix_ImageTextItemConfigure(iPtr, argc, argv, flags)
    Tix_DItem * iPtr;
    int argc;
    char ** argv;
    int flags;
{
    TixImageTextItem * itPtr = (TixImageTextItem *) iPtr;
    TixImageTextStyle * oldStyle = itPtr->stylePtr;

    if (Tk_ConfigureWidget(itPtr->ddPtr->interp, itPtr->ddPtr->tkwin,
	imageTextItemConfigSpecs,
	argc, argv, (char *)itPtr, flags) != TCL_OK) {
	return TCL_ERROR;
    }
    if (itPtr->stylePtr == NULL) {
	itPtr->stylePtr = (TixImageTextStyle*)TixGetDefaultDItemStyle(
	    itPtr->ddPtr, &tix_ImageTextItemType, iPtr, NULL);
    }

    /*
     * Free the old images for the widget, if there were any.
     */
    if (itPtr->image != NULL) {
	Tk_FreeImage(itPtr->image);
	itPtr->image = NULL;
    }

    if (itPtr->imageString != NULL) {
	itPtr->image = Tk_GetImage(itPtr->ddPtr->interp, itPtr->ddPtr->tkwin,
	    itPtr->imageString, ImageProc, (ClientData) itPtr);
	if (itPtr->image == NULL) {
	    return TCL_ERROR;
	}
    }

    if (oldStyle != NULL && itPtr->stylePtr != oldStyle) {
	Tix_ImageTextItemStyleChanged(iPtr);
    }
    else {
	Tix_ImageTextItemCalculateSize((Tix_DItem*)itPtr);
    }

    return TCL_OK;
}

static void Tix_ImageTextItemDisplay(pixmap, gc, iPtr, x, y,
	width, height, flags)
    Pixmap pixmap;
    GC gc;
    Tix_DItem * iPtr;
    int x;
    int y;
    int width;
    int height;
    int flags;
{
    TixImageTextItem *itPtr = (TixImageTextItem *)iPtr;
    GC foreGC, backGC;
    TixpSubRegion subReg;
    int bitY;
    int bitX;
    int textY;
    int textX;
    int imageH = 0;
    int imageW = 0;

    if ((width <= 0) || (height <= 0)) {
	return;
    }

    TixGetColorDItemGC(iPtr, &backGC, &foreGC, flags);
    TixpStartSubRegionDraw(itPtr->ddPtr, pixmap, foreGC,
	    &subReg, 0, 0, x, y, width, height,
	    itPtr->size[0], itPtr->size[1]);

    if (backGC != None) {
	TixpSubRegFillRectangle(itPtr->ddPtr->display, pixmap,
		backGC, &subReg, x, y, width, height);
    }

    TixDItemGetAnchor(itPtr->stylePtr->anchor, x, y, width, height,
	itPtr->size[0], itPtr->size[1], &x, &y);


    if (itPtr->image != NULL)
     {
      imageH = itPtr->imageH;
      imageW = itPtr->imageW;
     }
    else if (itPtr->bitmap != None)
     {
      imageH = itPtr->bitmapH;
      imageW = itPtr->bitmapW;
     }

    bitX = textX = x + itPtr->stylePtr->pad[0];
    bitY = textY = y + itPtr->stylePtr->pad[1];
    /* Adjust X position according to textanchor */
    switch(itPtr->stylePtr->textanchor) {
      case TK_ANCHOR_NW:
      case TK_ANCHOR_SW:
      case TK_ANCHOR_W:
	bitX += itPtr->textW + itPtr->stylePtr->gap;
        break;
      case TK_ANCHOR_NE:
      case TK_ANCHOR_SE:
      case TK_ANCHOR_E:
	textX += imageW + itPtr->stylePtr->gap;
        break;
      default:
	bitX = itPtr->size[0] - imageW - 2*itPtr->stylePtr->pad[0];

	if (bitX > 0) {
	    bitX = bitX / 2 + (bitX %2);
	} else {
	    bitX = 0;
	}
	bitX += x;
	textX = itPtr->size[0] - itPtr->textW - 2*itPtr->stylePtr->pad[0];
	if (textX > 0) {
	    textX = textX / 2 + (textX %2);
	} else {
	    textX = 0;
	}
	textX += x;
        break;
    }
    /* Adjust Y position according to textanchor */
    switch(itPtr->stylePtr->textanchor) {
      case TK_ANCHOR_NW:
      case TK_ANCHOR_NE:
      case TK_ANCHOR_N:
	bitY += itPtr->textH + itPtr->stylePtr->gap;
        break;
      case TK_ANCHOR_SE:
      case TK_ANCHOR_SW:
      case TK_ANCHOR_S:
	textY += imageH + itPtr->stylePtr->gap;
        break;
      default:
	bitY = itPtr->size[1] - imageH - 2*itPtr->stylePtr->pad[1];

	if (bitY > 0) {
	    bitY = bitY / 2 + (bitY %2);
	} else {
	    bitY = 0;
	}
	bitY += y;
	textY = itPtr->size[1] - itPtr->textH - 2*itPtr->stylePtr->pad[1];
	if (textY > 0) {
	    textY = textY / 2 + (textY %2);
	} else {
	    textY = 0;
	}
	textY += y;
        break;
    }

    if (itPtr->image != NULL) {
	if (itPtr->showImage) {
	    TixpSubRegDrawImage(&subReg, itPtr->image, 0, 0,
		    itPtr->imageW, itPtr->imageH, pixmap, bitX, bitY);
	}
    }
    else if (itPtr->bitmap != None && foreGC != None) {
	if (itPtr->showImage) {
	    TixpSubRegDrawBitmap(itPtr->ddPtr->display, pixmap, foreGC,
		    &subReg, itPtr->bitmap, 0, 0,
		    itPtr->bitmapW, itPtr->bitmapH,
		    bitX, bitY, 1);
	}
    }

    if (itPtr->text && itPtr->showText && foreGC != None) {
	TixpSubRegDisplayText(itPtr->ddPtr->display, pixmap,  foreGC, &subReg,
		itPtr->stylePtr->font, Tcl_GetString(itPtr->text), itPtr->numChars,
		textX, textY,
		itPtr->textW,
		itPtr->stylePtr->justify,
		itPtr->underline);
    }

    TixpEndSubRegionDraw(itPtr->ddPtr->display, pixmap, foreGC,
	    &subReg);
}

static void Tix_ImageTextItemCalculateSize(iPtr)
    Tix_DItem * iPtr;
{
    TixImageTextItem *itPtr = (TixImageTextItem *)iPtr;

    itPtr->size[0] = 0;
    itPtr->size[1] = 0;

    /* Note: the size of the image or the text are used even when
     * the showImage or showText options are off. These two options are
     * used to "blank" the respective components temporarily without
     * affecting the geometry of the ditem. The main is to indicate
     * transfer during drag+drop.
     *
     * If you want the image or text to completely disappear, config them
     * to NULL
     */
    if (itPtr->image != NULL) {
	Tk_SizeOfImage(itPtr->image, &itPtr->imageW, &itPtr->imageH);

	itPtr->size[0] = itPtr->imageW;
	itPtr->size[1] = itPtr->imageH;
    }
    else if (itPtr->bitmap != None) {
	Tk_SizeOfBitmap(itPtr->ddPtr->display, itPtr->bitmap, &itPtr->bitmapW,
		&itPtr->bitmapH);

	itPtr->size[0] = itPtr->bitmapW;
	itPtr->size[1] = itPtr->bitmapH;
    }

    if (itPtr->text) {
	itPtr->numChars = strlen(Tcl_GetString(itPtr->text));
	TixComputeTextGeometry(itPtr->stylePtr->font, Tcl_GetString(itPtr->text),
		itPtr->numChars, itPtr->stylePtr->wrapLength,
		&itPtr->textW, &itPtr->textH);
	/* Adjust size[0] i.e. X for EW-ness */
	switch(itPtr->stylePtr->textanchor) {
	  case TK_ANCHOR_SE: case TK_ANCHOR_SW:
	  case TK_ANCHOR_NE: case TK_ANCHOR_NW:
	  case TK_ANCHOR_E: case TK_ANCHOR_W:
	    itPtr->size[0] += itPtr->stylePtr->gap;
	    itPtr->size[0] += itPtr->textW;
	    break;
	  case TK_ANCHOR_CENTER:
	  case TK_ANCHOR_N: case TK_ANCHOR_S:
	    if (itPtr->textW > itPtr->size[0]) {
		itPtr->size[0] = itPtr->textW;
	    }
	    break;
	}
	/* Adjust size[1] i.e. Y for NS-ness */
	switch(itPtr->stylePtr->textanchor) {
	  case TK_ANCHOR_NW: case TK_ANCHOR_NE:
	  case TK_ANCHOR_SW: case TK_ANCHOR_SE:
	  case TK_ANCHOR_N: case TK_ANCHOR_S:
	    itPtr->size[1] += itPtr->stylePtr->gap;
	    itPtr->size[1] += itPtr->textH;
	    break;
	  case TK_ANCHOR_CENTER:
	  case TK_ANCHOR_E: case TK_ANCHOR_W:
	    if (itPtr->textH > itPtr->size[1]) {
		itPtr->size[1] = itPtr->textH;
	    }
	    break;
	}
    }

    itPtr->size[0] += 2*itPtr->stylePtr->pad[0];
    itPtr->size[1] += 2*itPtr->stylePtr->pad[1];
}

static char * Tix_ImageTextItemComponent(iPtr, x, y)
    Tix_DItem * iPtr;
    int x;
    int y;
{
    /* Unimplemented */
#if 0
    TixImageTextItem *itPtr = (TixImageTextItem *)iPtr;
#endif

    static char * body = "body";

    return body;
}


static void Tix_ImageTextItemStyleChanged(iPtr)
    Tix_DItem * iPtr;
{
    TixImageTextItem *itPtr = (TixImageTextItem *)iPtr;

    if (itPtr->stylePtr == NULL) {
	/* Maybe we haven't set the style to default style yet */
	return;
    }
    Tix_ImageTextItemCalculateSize(iPtr);
    if (itPtr->ddPtr->sizeChangedProc != NULL) {
	itPtr->ddPtr->sizeChangedProc(iPtr);
    }
}
static void Tix_ImageTextItemLostStyle(iPtr)
    Tix_DItem * iPtr;
{
    TixImageTextItem *itPtr = (TixImageTextItem *)iPtr;

    itPtr->stylePtr = (TixImageTextStyle*)TixGetDefaultDItemStyle(
	itPtr->ddPtr, &tix_ImageTextItemType, iPtr, NULL);

    Tix_ImageTextItemStyleChanged(iPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * ImageProc --
 *
 *	This procedure is invoked by the image code whenever the manager
 *	for an image does something that affects the size of contents
 *	of an image displayed in this widget.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Arranges for the HList to get redisplayed.
 *
 *----------------------------------------------------------------------
 */
static void
ImageProc(clientData, x, y, width, height, imgWidth, imgHeight)
    ClientData clientData;		/* Pointer to widget record. */
    int x, y;				/* Upper left pixel (within image)
					 * that must be redisplayed. */
    int width, height;			/* Dimensions of area to redisplay
					 * (may be <= 0). */
    int imgWidth, imgHeight;		/* New dimensions of image. */
{
    TixImageTextItem *itPtr = (TixImageTextItem *)clientData;

    Tix_ImageTextItemCalculateSize((Tix_DItem *)itPtr);
    if (itPtr->ddPtr->sizeChangedProc != NULL) {
	itPtr->ddPtr->sizeChangedProc((Tix_DItem *)itPtr);
    }
}

/*----------------------------------------------------------------------
 *
 *
 *			Display styles
 *
 *
 *----------------------------------------------------------------------
 */
static Tix_DItemStyle *
Tix_ImageTextStyleCreate(interp, tkwin, diTypePtr, name)
    Tcl_Interp * interp;
    Tk_Window tkwin;
    char * name;
    Tix_DItemInfo * diTypePtr;
{
    int i;
    TixImageTextStyle * stylePtr =
      (TixImageTextStyle *)ckalloc(sizeof(TixImageTextStyle));

    stylePtr->font	 = NULL;
    stylePtr->gap	 = 0;
    stylePtr->textanchor = TK_ANCHOR_E;
    stylePtr->justify	 = TK_JUSTIFY_LEFT;
    stylePtr->wrapLength = 0;
    stylePtr->pad[0]	 = 0;
    stylePtr->pad[1]	 = 0;
    stylePtr->anchor	 = TK_ANCHOR_CENTER;

    for (i=0; i<4; i++) {
	stylePtr->colors[i].bg = NULL;
	stylePtr->colors[i].fg = NULL;
	stylePtr->colors[i].backGC = None;
	stylePtr->colors[i].foreGC = NULL;
    }

    return (Tix_DItemStyle *)stylePtr;
}

static int
Tix_ImageTextStyleConfigure(style, argc, argv, flags)
    Tix_DItemStyle *style;
    int argc;
    char ** argv;
    int flags;
{
    TixImageTextStyle * stylePtr = (TixImageTextStyle *)style;
    XGCValues gcValues;
    GC newGC;
    int i, isNew;

    if (stylePtr->font == NULL) {
	isNew = 1;
    } else {
	isNew = 0;
    }

    if (!(flags &TIX_DONT_CALL_CONFIG)) {
	if (Tk_ConfigureWidget(stylePtr->interp, stylePtr->tkwin,
	    imageTextStyleConfigSpecs,
	    argc, argv, (char *)stylePtr, flags) != TCL_OK) {
	    return TCL_ERROR;
	}
    }

    gcValues.font = TixFontId(stylePtr->font);
    gcValues.graphics_exposures = False;

    for (i=0; i<4; i++) {
	/* Foreground */
	gcValues.background = stylePtr->colors[i].bg->pixel;
	gcValues.foreground = stylePtr->colors[i].fg->pixel;
	newGC = Tk_GetGC(stylePtr->tkwin,
	    GCFont|GCForeground|GCBackground|GCGraphicsExposures, &gcValues);

	if (stylePtr->colors[i].foreGC != None) {
	    Tk_FreeGC(Tk_Display(stylePtr->tkwin),
		stylePtr->colors[i].foreGC);
	}
	stylePtr->colors[i].foreGC = newGC;

	/* Background */
	gcValues.foreground = stylePtr->colors[i].bg->pixel;
	newGC = Tk_GetGC(stylePtr->tkwin,
	    GCFont|GCForeground|GCGraphicsExposures, &gcValues);

	if (stylePtr->colors[i].backGC != None) {
	    Tk_FreeGC(Tk_Display(stylePtr->tkwin),
		stylePtr->colors[i].backGC);
	}
	stylePtr->colors[i].backGC = newGC;
    }

    if (!isNew) {
	TixDItemStyleChanged(stylePtr->diTypePtr, (Tix_DItemStyle *)stylePtr);
    }

    return TCL_OK;
}

static void Tix_ImageTextStyleFree(style)
    Tix_DItemStyle *style;
{
    TixImageTextStyle * stylePtr = (TixImageTextStyle *)style;
    int i;

    for (i=0; i<4; i++) {
	if (stylePtr->colors[i].backGC != None) {
	    Tk_FreeGC(Tk_Display(stylePtr->tkwin), stylePtr->colors[i].backGC);
	}
	if (stylePtr->colors[i].foreGC != None) {
	    Tk_FreeGC(Tk_Display(stylePtr->tkwin), stylePtr->colors[i].foreGC);
	}
    }

    Tk_FreeOptions(imageTextStyleConfigSpecs, (char *)stylePtr,
	Tk_Display(stylePtr->tkwin), 0);
    ckfree((char *)stylePtr);
}

static int bg_flags [4] = {
    TIX_DITEM_NORMAL_BG,
    TIX_DITEM_ACTIVE_BG,
    TIX_DITEM_SELECTED_BG,
    TIX_DITEM_DISABLED_BG
};
static int fg_flags [4] = {
    TIX_DITEM_NORMAL_FG,
    TIX_DITEM_ACTIVE_FG,
    TIX_DITEM_SELECTED_FG,
    TIX_DITEM_DISABLED_FG
};

static void
Tix_ImageTextStyleSetTemplate(style, tmplPtr)
    Tix_DItemStyle* style;
    Tix_StyleTemplate * tmplPtr;
{
    TixImageTextStyle * stylePtr = (TixImageTextStyle *)style;
    int i;

    if (tmplPtr->flags & TIX_DITEM_FONT) {
	if (stylePtr->font != NULL) {
	    TixFreeFont(stylePtr->font);
	}
	stylePtr->font = TixGetFont(
	    stylePtr->interp, stylePtr->tkwin,
	    TixNameOfFont(tmplPtr->font));
    }
    if (tmplPtr->flags & TIX_DITEM_PADX) {
	stylePtr->pad[0] = tmplPtr->pad[0];
    }
    if (tmplPtr->flags & TIX_DITEM_PADY) {
	stylePtr->pad[1] = tmplPtr->pad[1];
    }

    for (i=0; i<4; i++) {
	if (tmplPtr->flags & bg_flags[i]) {
	    if (stylePtr->colors[i].bg != NULL) {
		Tk_FreeColor(stylePtr->colors[i].bg);
	    }
	    stylePtr->colors[i].bg = Tk_GetColor(
		stylePtr->interp, stylePtr->tkwin,
		Tk_NameOfColor(tmplPtr->colors[i].bg));
	}
    }
    for (i=0; i<4; i++) {
	if (tmplPtr->flags & fg_flags[i]) {
	    if (stylePtr->colors[i].fg != NULL) {
		Tk_FreeColor(stylePtr->colors[i].fg);
	    }
	    stylePtr->colors[i].fg = Tk_GetColor(
		stylePtr->interp, stylePtr->tkwin,
		Tk_NameOfColor(tmplPtr->colors[i].fg));
	}
    }

    Tix_ImageTextStyleConfigure(style, 0, 0, TIX_DONT_CALL_CONFIG);
}
