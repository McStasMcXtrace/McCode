/*
  Copyright (c) 1995-2003 Nick Ing-Simmons. All rights reserved.
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
#include "pTk/tkVMacro.h"

DECLARE_VTABLES;

extern Tk_ItemType ptkCanvGridType;
extern Tk_ItemType ptkCanvGroupType;


MODULE = Tk::Canvas	PACKAGE = Tk

void
canvas(...)
CODE:
 {
  TKXSRETURN(XSTkCommand(cv,1,Tk_CanvasObjCmd,items,&ST(0)));
 }


PROTOTYPES: DISABLE

BOOT:
 {
  IMPORT_VTABLES;
  Tk_CreateItemType(&ptkCanvGridType);
  Tk_CreateItemType(&ptkCanvGroupType);
 }
