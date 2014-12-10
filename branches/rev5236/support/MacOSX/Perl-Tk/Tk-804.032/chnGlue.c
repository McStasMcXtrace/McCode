/*
  Copyright (c) 1997-2003 Nick Ing-Simmons. All rights reserved.
  This program is free software; you can redistribute it and/or
  modify it under the same terms as Perl itself.
*/

#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>

#include "tkGlue.def"

#include "pTk/tkPort.h"
#include "pTk/tkInt.h"
#include "tkGlue.h"

static char *Lang_Utf8ToBytes(CONST char *src);

Tcl_Channel
Tcl_OpenFileChannel(interp,fileName,modeString,permissions)
Tcl_Interp *interp;
CONST char *fileName;
CONST char *modeString;
int permissions;
{PerlIO *f = PerlIO_open(Lang_Utf8ToBytes(fileName),modeString);
 /* Hopefully every fileName here should be translated back to octets ... */
 if (!f)
  {
   /* FIXME - use strerr() or perl's equivalent */
   if (interp)
    Tcl_SprintfResult(interp,"Cannot open '%s' in mode '%s'",fileName, modeString);
  }
 return (Tcl_Channel) f;
}

Tcl_Channel
Tcl_FSOpenFileChannel(interp, pathPtr, modeString, permissions)
    Tcl_Interp *interp;                 /* Interpreter for error reporting;
                                         * can be NULL. */
    Tcl_Obj *pathPtr;                   /* Name of file to open. */
    CONST char *modeString;             /* A list of POSIX open modes or
                                         * a string such as "rw". */
    int permissions;                    /* If the open involves creating a
                                         * file, with what modes to create
                                         * it? */
{
 return Tcl_OpenFileChannel(interp, Tcl_GetString(pathPtr), modeString, permissions);
}



Tcl_Channel
Tcl_GetChannel (Tcl_Interp *interp,CONST char *chanName, int *modePtr)
{
 Tcl_SprintfResult(interp,"Tcl_GetChannel %s not implemeted",chanName);
 return NULL;
}


int
Tcl_Read(chan,bufPtr,toRead)
Tcl_Channel chan;
char *bufPtr;
int toRead;
{
 PerlIO *f = (PerlIO *) chan;
 return PerlIO_read(f,bufPtr,toRead);
}

int
Tcl_Write(chan, buf, count)
Tcl_Channel chan;
CONST char *buf;
int count;
{
 PerlIO *f = (PerlIO *) chan;
 if (count < 0)
  count = strlen(buf);
 return PerlIO_write(f,buf,count);
}

int
Tcl_WriteChars(Tcl_Channel chan, CONST char * src, int srcLen)
{
 return Tcl_Write(chan, (char *) src, srcLen);
}

Tcl_Channel
Tcl_GetStdChannel(int type)
{
 switch(type)
  {
   case TCL_STDIN:
    return (Tcl_Channel) PerlIO_stdin();
   case TCL_STDOUT:
    return (Tcl_Channel) PerlIO_stdout();
   case TCL_STDERR:
    return (Tcl_Channel) PerlIO_stderr();
  }
 return NULL;
}


int
Tcl_Close(interp,chan)
Tcl_Interp *interp;
Tcl_Channel chan;
{
 return PerlIO_close((PerlIO *) chan);
}

Tcl_WideInt
Tcl_Seek(Tcl_Channel chan, Tcl_WideInt offset, int mode)
{
 PerlIO_seek((PerlIO *) chan, offset, mode);
 return PerlIO_tell((PerlIO *) chan);
}

int
Tcl_Eof(Tcl_Channel chan)
{
 PerlIO *f = (PerlIO *) chan;
 return PerlIO_eof(f);
}

int
Tcl_SetChannelOption(Tcl_Interp *interp, Tcl_Channel chan,
                  CONST char *optionName, CONST char *newValue)
{
 PerlIO *f = (PerlIO *) chan;
 if (LangCmpOpt("-translation",optionName,-1) == 0 ||
     LangCmpOpt("-encoding",optionName,-1) == 0
    )
  {
   if (strcmp(newValue,"binary") == 0)
    {
     dTHX;
     PerlIO_binmode(aTHX_ f,'+',O_BINARY,Nullch);
     return TCL_OK;
    }
  }
 warn("Set option %s=%s on channel %d", optionName, newValue, PerlIO_fileno(f));
 return TCL_OK;
}

char *
Lang_Utf8ToBytes(CONST char *src)
{
 SV* sv = newSVpv(src,0);
 sv_2mortal(sv);
 sv_utf8_decode(sv);
 return SvPVbyte_nolen(sv);
}



