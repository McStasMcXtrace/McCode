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

MODULE = Tk::Text	PACKAGE = Tk

PROTOTYPES: DISABLE

void
text(...)
CODE:
 {
  TKXSRETURN(XSTkCommand(cv,1,Tk_TextCmd,items,&ST(0)));
 }

BOOT:
 {
  IMPORT_VTABLES;
 }
