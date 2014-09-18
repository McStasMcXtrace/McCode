/*
  Copyright (c) 1995-2004 Nick Ing-Simmons. All rights reserved.
  This program is free software; you can redistribute it and/or
  modify it under the same terms as Perl itself.
*/
#define PERL_NO_GET_CONTEXT
#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>

#include "tkGlue.def"

#include "pTk/tkPort.h"
#include "pTk/tkInt.h"
#include "pTk/tixPort.h"
#include "pTk/tixInt.h"
#include "pTk/tixImgXpm.h"
#include "tkGlue.h"
#include "tkGlue.m"
#include "pTk/tkVMacro.h"

DECLARE_VTABLES;
DECLARE_TIX;
DECLARE_TIXXPM;

extern Tk_ImageType tixPixmapImageType;

static void  Install _((char *, TkWindow *win));

static void
Install(class,winPtr)
char *class;
TkWindow *winPtr;
{
 TkMainInfo *mainInfo = winPtr->mainPtr;
 if (mainInfo)
  {
   Tcl_Interp *Et_Interp = mainInfo->interp;
   if (Et_Interp)
    {
#if defined(WIN32) && !defined(__GNUC__)
#pragma warning(disable: 4305)
#endif
#define UNSIGNED_CHAR unsigned char
#include "pTk/tixBitmaps.h"
    }
  }
}


MODULE = Tk::Pixmap	PACKAGE = Tk::Pixmap

PROTOTYPES: DISABLE

void
Install(class,win)
char *		class
TkWindow *	win

BOOT:
 {
  IMPORT_VTABLES;
  IMPORT_TIX;
  IMPORT_TIXXPM;
  Tk_CreateImageType(&tixPixmapImageType);
 }
