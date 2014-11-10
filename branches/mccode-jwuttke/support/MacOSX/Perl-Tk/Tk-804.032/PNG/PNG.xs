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
#include "tkGlue.h"
#include "tkGlue.m"
#include "pTk/tkImgPhoto.h"
#include "pTk/tkImgPhoto.m"
#include "pTk/imgInt.h"
#include "pTk/imgInt.m"
#include "pTk/tkVMacro.h"

extern Tk_PhotoImageFormat	imgFmtPNG;

DECLARE_VTABLES;
DECLARE_PHOTO;

MODULE = Tk::PNG	PACKAGE = Tk::PNG

PROTOTYPES: DISABLE

BOOT:
 {
  IMPORT_VTABLES;
  IMPORT_PHOTO;
  Tk_CreatePhotoImageFormat(&imgFmtPNG);
 }
