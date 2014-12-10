/*
  Copyright (c) 1995-2003 Nick Ing-Simmons. All rights reserved.
  This program is free software; you can redistribute it and/or
  modify it under the same terms as Perl itself.
*/
#define PERL_NO_GET_CONTEXT
#include <windows.h>
#include <shellapi.h>

#ifdef __CYGWIN__
# undef WIN32
#endif
#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>

#include "tkGlue.def"

#include "pTk/tkPort.h"
#include "pTk/tkInt.h"
#include "pTk/tixPort.h"
#include "pTk/tixInt.h"
#include "tkGlue.h"
#include "tkGlue.m"
#include "pTk/tkVMacro.h"

DECLARE_VTABLES;

MODULE = Tk::DragDrop::Win32Site	PACKAGE = Tk::DragDrop::Win32Site

PROTOTYPES: Enable

void
DragAcceptFiles(win,flag)
HWND	win
BOOL 	flag
CODE:
 {
  DragAcceptFiles(win,flag);
 }

void
DropInfo(drop)
HANDLE	drop
PPCODE:
 {
  UINT count = DragQueryFile(drop, -1, NULL, 0);
  UINT i;
  POINT point;
  SV *x = sv_newmortal();
  SV *y = sv_newmortal();
  int n = 0;
  if (DragQueryPoint(drop,&point))
   {
    sv_setiv(x,point.x);
    sv_setiv(y,point.y);
   }
  XPUSHs(x);
  XPUSHs(y);
  for (i=0; i < count; i++)
   {
    UINT len = DragQueryFile(drop, i, NULL, 0);
    SV *sv = newSVpv("",0);
    SvGROW(sv,len+1);
    len = DragQueryFile(drop,i,SvPVX(sv),SvLEN(sv));
    SvCUR(sv) = len;
    XPUSHs(sv_2mortal(sv));
   }
  DragFinish(drop);
  PUTBACK;
 }

BOOT:
 {
  IMPORT_VTABLES;
 }
