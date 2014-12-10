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

MODULE = Tk::Entry	PACKAGE = Tk

void
entry(...)
CODE:
 {
  TKXSRETURN(XSTkCommand(cv,0,Tk_EntryObjCmd,items,&ST(0)));
 }

void
spinbox(...)
CODE:
 {
  TKXSRETURN(XSTkCommand(cv,0,Tk_SpinboxObjCmd,items,&ST(0)));
 }

PROTOTYPES: DISABLE

BOOT:
 {
  IMPORT_VTABLES;
  /* Lang_TkCommand("entry", Tk_EntryCmd); */
 }
