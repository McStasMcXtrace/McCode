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
#include "pTk/tix.h"
#include "pTk/tixInt.h"
#include "pTk/tkVMacro.h"
#include "tkGlue.h"
#include "tkGlue.m"


DECLARE_VTABLES;
DECLARE_TIX;

MODULE = Tk::InputO	PACKAGE = Tk

PROTOTYPES: DISABLE

void
inputo(...)
CODE:
 {
  TKXSRETURN(XSTkCommand(cv,1,Tix_InputOnlyCmd,items,&ST(0)));
 }

BOOT:
 {
  IMPORT_VTABLES;
  IMPORT_TIX;
 }
