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
#if defined(WIN32) || (defined(__WIN32__) && defined(__CYGWIN__))
#include "pTk/tkWin.h"
#include "pTk/tkWinInt.h"
#endif
#include "pTk/tkVMacro.h"
#include "tkGlue.h"
#include "tkGlue.m"

DECLARE_VTABLES;

MODULE = Tk::Scrollbar	PACKAGE = Tk

PROTOTYPES: DISABLE

void
scrollbar(...)
CODE:
 {
  TKXSRETURN(XSTkCommand(cv,1,Tk_ScrollbarCmd,items,&ST(0)));
 }

BOOT:
 {
  IMPORT_VTABLES;
 }
