/*
  Copyright (c) 1995-2003 Nick Ing-Simmons. All rights reserved.
  This program is free software; you can redistribute it and/or
  modify it under the same terms as Perl itself.
*/
#define PERL_NO_GET_CONTEXT
#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>

#include <tkGlue.def>

#include <pTk/tkPort.h>
#include <pTk/tkInt.h>
#include <pTk/tkVMacro.h>
#include <tkGlue.h>
#include <tkGlue.m>

DECLARE_VTABLES;

static void Tk_ImageBackground _((Tk_Window win, char *imageString));
static void ButtonImageProc _((ClientData clientData,
			    int x, int y, int width, int height,
			    int imgWidth, int imgHeight));

static void
BackgroundImageProc(clientData, x, y, width, height, imgWidth, imgHeight)
    ClientData clientData;		/* Pointer to widget record. */
    int x, y;				/* Upper left pixel (within image)
					 * that must be redisplayed. */
    int width, height;			/* Dimensions of area to redisplay
					 * (may be <= 0). */
    int imgWidth, imgHeight;		/* New dimensions of image. */
{
}

static void
Tk_ImageBackground(tkwin, imageString)
Tk_Window tkwin;
char *imageString;
{
 Display *dpy   = Tk_Display(tkwin);
 Tcl_Interp *interp = ((TkWindow*) tkwin)->mainPtr->interp;
 Tk_Image image = Tk_GetImage(interp, tkwin,
		  imageString, BackgroundImageProc, (ClientData) tkwin);
 int width, height;
 Pixmap pixmap;
 Tk_SizeOfImage(image, &width, &height);
 Tk_MakeWindowExist(tkwin);
 pixmap  = Tk_GetPixmap(dpy, Tk_WindowId(tkwin), width, height, Tk_Depth(tkwin));
 Tk_RedrawImage(image, 0, 0, width, height, pixmap, 0, 0);
 Tk_SetWindowBackgroundPixmap(tkwin, pixmap);
 Tk_FreePixmap(dpy, pixmap);
 Tk_FreeImage(image);
}


MODULE = Tk::ImageBack	PACKAGE = Tk	PREFIX = Tk_

PROTOTYPES: DISABLE

void
Tk_ImageBackground(tkwin, imageString)
Tk_Window	tkwin
char *		imageString


BOOT:
 {
  IMPORT_VTABLES;
 }

