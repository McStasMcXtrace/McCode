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

/* #define DO_CHECK_TCL_ALLOC */


#define TCL_EVENT_IMPLEMENT
#include "pTk/Lang.h"
#include "pTk/tkEvent.h"
#include "pTk/tkEvent_f.h"
#include "pTk/tkEvent_f.c"

extern void TclInitSubsystems(CONST char *argv0);

static int parent_pid = 0;

static SV *
FindVarName(pTHX_ char *varName,int flags)
{
 STRLEN len;
 SV *name = newSVpv("Tk",2);
 SV *sv;
 sv_catpv(name,"::");
 sv_catpv(name,varName);
 sv = perl_get_sv(SvPV(name,len),flags);
 SvREFCNT_dec(name);
 return sv;
}

#ifndef INT2PTR
#define INT2PTR(any,d) (any)(d)
#endif
#ifndef PTR2IV
#define PTR2IV(p)	INT2PTR(IV,p)
#endif

void
LangDebug(CONST char *fmt,...)
{
 dTHX; /* FIXME? */
 va_list ap;
 va_start(ap,fmt);
 if (SvIV(FindVarName(aTHX_ "LangDebug",GV_ADD|GV_ADDWARN)))
  {
   PerlIO_vprintf(PerlIO_stderr(), fmt, ap);
   PerlIO_flush(PerlIO_stderr());
  }
 va_end(ap);
}

void
#ifdef STANDARD_C
Tcl_Panic(CONST char *fmt,...)
#else
/*VARARGS0 */
Tcl_Panic(fmt, va_alist)
CONST char *fmt;
va_dcl
#endif
{
 dTHX;
 va_list ap;
#ifdef I_STDARG
 va_start(ap, fmt);
#else
 va_start(ap);
#endif
 PerlIO_flush(PerlIO_stderr());
 PerlIO_vprintf(PerlIO_stderr(), fmt, ap);
 PerlIO_putc(PerlIO_stderr(),'\n');
 va_end(ap);
#if defined(WIN32) && defined(DEBUGGING)
 {
  int *p = 0;
  if (*p)
   abort();
 }
#endif
 abort();
 croak("Tcl_Panic");
}

#undef Tcl_Realloc
#undef Tcl_Alloc
#undef Tcl_Free

#undef Tcl_DumpActiveMemory
#undef Tcl_ValidateAllMemory
void
Tcl_ValidateAllMemory (CONST char *file, int line)
{
}


#ifdef DO_CHECK_TCL_ALLOC

typedef struct alloc_s
{
 struct alloc_s *self;
 unsigned int size;
 struct alloc_s *next;
 struct alloc_s *prev;
 CONST char *file;
 int line;
 char space[1];
} alloc_t, *alloc_ptr;

static alloc_ptr allocated;

long Tcl_AllocCount = 0;

static int
is_perl_arena(void *ptr)
{
    SV *sv = ptr;
    SV* sva;
    register SV* svend;

    for (sva = PL_sv_arenaroot; sva; sva = (SV*)SvANY(sva)) {
	svend = &sva[SvREFCNT(sva)];
	if (sva <= sv && sv < svend)
	    return 1;
    }
    return 0;
}

static void
check_lp(alloc_ptr lp,CONST char *s, CONST char *file, int line)
{
 if (is_perl_arena(lp))
  {
   warn("Attempt to '%s(%p)' perl data @ %s:%d",s,&lp->space, file, line);
   abort();
  }
 if (lp->self != lp || lp[lp->size-1].self != lp)
  {
   warn("Invalid '%s(%p)' a=%p s=%p e=%p @ %s:%d/%s:%d",
         s,&lp->space,
         lp,lp->self, lp[lp->size-1].self, file, line, lp->file, lp->line);
   abort();
  }
}

static void
delink(alloc_ptr lp)
{
 lp->prev->next = lp->next;
 lp->next->prev = lp->prev;
 if (allocated == lp)
  {
   allocated = lp->prev;
   if (allocated == lp)
    allocated = NULL;
  }
}

static char *
enlink(alloc_ptr lp, unsigned int size, CONST char *file, int line)
{
 lp->next = (allocated) ? allocated : lp;
 lp->prev = (allocated) ? allocated->prev : lp;
 lp->prev->next = lp;
 lp->next->prev = lp;
 allocated = lp;
 lp->self = lp;
 lp->size = size;
 lp[size-1].self = lp;
 lp->file = file;
 lp->line = line;
 check_lp(lp,__FUNCTION__,file,line);
 return lp->space;
}

void
Lang_NoteOwner (void *owner,void *packet, CONST char *file, int line)
{
 alloc_ptr op = (alloc_ptr) ((char *)owner  - offsetof(alloc_t,space));
 alloc_ptr lp = (alloc_ptr) ((char *)packet - offsetof(alloc_t,space));
 check_lp(lp,__FUNCTION__, file, line);
 check_lp(op,__FUNCTION__, file, line);
 lp->file = op->file;
 lp->line = op->line;
}

char *
Tcl_DbCkrealloc (char *ptr,unsigned int size,CONST char *file,int line)
{
 alloc_ptr lp = (alloc_ptr) (ptr - offsetof(alloc_t,space));
 check_lp(lp,__FUNCTION__, file, line);
 file = lp->file;
 line = lp->line;
 delink(lp);
 if ((int) size < 0)
  abort();
 size = (sizeof(alloc_t)+size+sizeof(alloc_t)+sizeof(alloc_t)-1)/sizeof(alloc_t);
 lp = PerlMemShared_realloc(lp,size*sizeof(alloc_t));
 return enlink(lp,size,file,line);
}

char *
Tcl_DbCkalloc (unsigned int usize,CONST char *file,int line)
{
 char *res;
 alloc_t *lp;
 size_t size = (sizeof(alloc_t)+usize+sizeof(alloc_t)+sizeof(alloc_t)-1)/sizeof(alloc_t);
 if ((int) usize < 0)
  abort();
 lp = PerlMemShared_calloc(size, sizeof(alloc_t));
 Tcl_AllocCount++;
 res = enlink(lp,size,file,line);
 if (res+usize > (char *)(&lp[size-1].self))
  {
   warn("s=%x lp=%p res=%p..%p e=%p\n",usize,lp,res,res+usize,&lp[size-1].self);
   assert(res+usize <= (char *)(&lp[size-1].self));
  }
 return res;
}

int
Tcl_DbCkfree (char *ptr,CONST char *file ,int line)
{
 if (ptr)
  {
   alloc_ptr lp = (alloc_ptr) (ptr - offsetof(alloc_t,space));
   check_lp(lp,__FUNCTION__, file, line);
   delink(lp);
   Tcl_AllocCount--;
   memset(lp,-1,lp->size*sizeof(alloc_t));
   PerlMemShared_free(lp);
  }
 else
  {
#ifndef WIN32
   warn("Attempt to 'free(%p) @ %s:%s' NULL",ptr,file,line);
#endif
  }
 return 0;
}

int
Tcl_DumpActiveMemory (CONST char *fileName)
{
 alloc_ptr p = allocated;
 if (Tcl_AllocCount)
  {
   long count = 0;
   PerlIO_printf(PerlIO_stderr(),"\n%ld Tcl_Alloc packets un-freed\n",Tcl_AllocCount);
   do
    {
     PerlIO_printf(PerlIO_stderr(),"%3ld @ %s:%d\n",p->size,p->file,p->line);
     count++;
     p = p->next;
    } while (p != allocated);
   if (count != Tcl_AllocCount)
    {
     PerlIO_printf(PerlIO_stderr(),"%ld un-freed, %ld on chain",Tcl_AllocCount,count);
    }
  }
 return 0;
}

char *
Tcl_Realloc(char *p, unsigned int size)
{
 return Tcl_DbCkrealloc(p, size, __FILE__, __LINE__);
}

char *
Tcl_Alloc(unsigned int size)
{
 return Tcl_DbCkalloc(size,__FILE__,__LINE__);
}

void
Tcl_Free(char *p)
{
 Tcl_DbCkfree(p,__FILE__,__LINE__);
}

#else

void
Lang_NoteOwner (void *owner,void *packet, CONST char *file, int line)
{
}

int
Tcl_DumpActiveMemory (CONST char *fileName)
{
 return 0;
}

char *
Tcl_Realloc(char *p, unsigned int size)
{
 dTHXs;
 if ((int) size < 0)
  abort();
 p = PerlMemShared_realloc(p,size*sizeof(char));
 return p;
}

char *
Tcl_Alloc(unsigned int size)
{
 dTHXs;
 char *p;
 if ((int) size < 0)
  abort();
 p = PerlMemShared_calloc(size, sizeof(char));
 return p;
}

void
Tcl_Free(char *p)
{
 dTHXs;
 PerlMemShared_free(p);
}

char *
Tcl_DbCkalloc (unsigned int size,CONST char *file,int line)
{
 return Tcl_Alloc(size);
}

int
Tcl_DbCkfree (char *ptr,CONST char *file ,int line)
{
 Tcl_Free(ptr);
 return 0;
}

char *
Tcl_DbCkrealloc (char *ptr,unsigned int size,CONST char *file,int line)
{
 return Tcl_Realloc(ptr,size);
}

#endif

char *
Tcl_AttemptDbCkalloc(unsigned int usize,CONST char *file,int line)
{
 return Tcl_DbCkalloc(usize,file,line);
}

void
Event_CleanupGlue(void)
{

}


long
Lang_OSHandle(fd)
int fd;
{
#if defined(WIN32) && !defined(__CYGWIN__)
 return win32_get_osfhandle(fd);
#else
 return fd;
#endif
}

static void
install_vtab(pTHX_ char *name, void *table, size_t size)
{
 if (table)
  {
   typedef int (*fptr)_((void));
   fptr *q = table;
   unsigned i;
   sv_setiv(FindVarName(aTHX_ name,GV_ADD|GV_ADDMULTI),PTR2IV(table));
   if (size % sizeof(fptr))
    {
     warn("%s is strange size %d",name,size);
    }
   size /= sizeof(void *);
   for (i=0; i < size; i++)
    {
     if (!q[i])
      warn("%s slot %d is NULL",name,i);
    }
  }
 else
  {
   croak("%s pointer is NULL",name);
  }
}

static void SetupProc _ANSI_ARGS_((ClientData clientData, int flags));
static void CheckProc _ANSI_ARGS_((ClientData clientData, int flags));
static int EventProc  _ANSI_ARGS_((Tcl_Event *evPtr, int flags));

typedef struct PerlIOHandler
 {
  struct PerlIOHandler *nextPtr;  /* Next in list of all files we care about. */
  SV *handle;                     /* Handle we are tied to */
  IO *io;                         /* Current IO within handle */
  GV *untied;                     /* Another handle to pass to methods
                                   * it is untied to avoid recusion and
                                   * has IoIFP/IoOFP of its IO dynamically set to those
                                   * of io.
                                   */
  LangCallback *readHandler;
  LangCallback *writeHandler;
  LangCallback *exceptionHandler;
  int mask;                       /* Mask of desired events: TCL_READABLE etc. */
  int readyMask;                  /* Mask of events that have been seen since the
                                     * last time file handlers were invoked for
                                     * this file. */
  int waitMask;                   /* Events on which we are doing blocking wait */
  int handlerMask;                /* Events for which we have callbacks */
  int callingMask;                /* Events for which we are in callbacks */
  int pending;
  SV *mysv;
  IV extraRefs;
  } PerlIOHandler;

typedef struct PerlIOEvent
 {
  Tcl_Event header;               /* Information that is standard for all events. */
  IO *io;                         /* PerlIO descriptor that is ready. */
 } PerlIOEvent;


static int initialized = 0;

static PerlIOHandler *firstPerlIOHandler;

static void PerlIOEventInit(void);

static void PerlIO_watch(PerlIOHandler *filePtr);

static volatile int stuck;

void
PerlIO_MaskCheck(PerlIOHandler *filePtr)
{
 if (filePtr->mask & ~(filePtr->waitMask|filePtr->handlerMask))
  {
   warn("Mask=%d wait=%d handler=%d",
         filePtr->mask, filePtr->waitMask, filePtr->handlerMask);
   PerlIO_watch(filePtr);
  }
}

static void
PerlIOFileProc(ClientData clientData, int mask)
{
 PerlIOHandler *filePtr = (PerlIOHandler *) clientData;
 PerlIO_MaskCheck(filePtr);
 filePtr->readyMask |= (mask & filePtr->mask);
}

SV *
PerlIO_handle(filePtr)
PerlIOHandler *filePtr;
{
 dTHX; /* FIXME */
 filePtr->io = sv_2io(filePtr->handle);
 if (filePtr->io)
  {
   /* io exists - copy current PerlIO * from io to our un-tied IO */
   IO *tmpio = GvIOp(filePtr->untied);
   IoIFP(tmpio) = IoIFP(filePtr->io);
   IoOFP(tmpio) = IoOFP(filePtr->io);
   IoFLAGS(tmpio) = IoFLAGS(filePtr->io);
   return newRV((SV *) filePtr->untied);
  }
 return &PL_sv_undef;
}

void
PerlIO_unwatch(PerlIOHandler *filePtr)
{
 filePtr->waitMask = filePtr->handlerMask = 0;
 PerlIO_watch(filePtr);
}

void
PerlIO_watch(PerlIOHandler *filePtr)
{
 dTHX; /* FIXME */
 PerlIO *ip = IoIFP(filePtr->io);
 PerlIO *op = IoOFP(filePtr->io);
 int fd     = (ip) ? PerlIO_fileno(ip) : ((op) ? PerlIO_fileno(op) : -1);
 int mask   = filePtr->waitMask|filePtr->handlerMask;

 if (mask & ~(TCL_READABLE|TCL_EXCEPTION|TCL_WRITABLE))
  {
   LangDebug("Invalid mask %x",mask);
   croak("Invalid mask %x",mask);
  }

 if (mask & (TCL_READABLE|TCL_EXCEPTION))
  {
   if (!ip)
    croak("Handle not opened for input");
  }

 if (mask & (TCL_WRITABLE))
  {
   if (!op)
    croak("Handle not opened for output");
  }

 if ((mask & TCL_READABLE) && (mask & TCL_WRITABLE))
  {
   /* Both read and write IO - make sure buffers not shared */
   if (op && (op == ip) && fd >= 0)
    {
     IoOFP(filePtr->io) = op = PerlIO_fdopen(fd, "w");
    }
   if (PerlIO_fileno(ip) != PerlIO_fileno(op))
    {
     croak("fileno not same for read %d  and write %d",
           PerlIO_fileno(ip) , PerlIO_fileno(op));
    }
  }

 if (filePtr->mask != mask)
  {
   if (fd >= 0)
    {
     Tcl_DeleteFileHandler(fd);
    }
   if (mask && fd >= 0)
    {
     Tcl_CreateFileHandler(fd, mask, PerlIOFileProc, (ClientData) filePtr );
    }
   filePtr->mask = mask;
  }

}

int
PerlIO_is_writable(PerlIOHandler *filePtr)
{
 if (!(filePtr->readyMask & TCL_WRITABLE))
  {
   dTHX; /* FIXME */
   PerlIO *op = IoOFP(filePtr->io);
   if (op)
    {
     if (PerlIO_has_cntptr(op) && PerlIO_get_cnt(op) > 0)
      {
       filePtr->readyMask |= TCL_WRITABLE;
      }
    }
  }
 return (filePtr->readyMask & TCL_WRITABLE);
}

int
PerlIO_is_readable(PerlIOHandler *filePtr)
{
 dTHX; /* FIXME */
 if (!(filePtr->readyMask & TCL_READABLE))
  {
   PerlIO *io = IoIFP(filePtr->io);
   if (io)
    {
#ifdef PERLIO_LAYERS
     if (PerlIO_has_cntptr(io) && PerlIO_get_cnt(io) > 0)
      {
       filePtr->readyMask |= TCL_READABLE;
      }
#else
     /* Turn this buffer stuff off for now */
     if (PerlIO_has_cntptr(io) && PerlIO_get_cnt(io) > 0)
      {
       filePtr->readyMask |= TCL_READABLE;
      }
#endif
    }
  }
 return (filePtr->readyMask & TCL_READABLE);
}

int
PerlIO_has_exception(PerlIOHandler *filePtr)
{
 return (filePtr->readyMask & TCL_EXCEPTION);
}

void
PerlIO_wait(filePtr,mask)
PerlIOHandler *	filePtr;
int		mask;
{
 /* Return at once if we are in the callback */
 if (!(filePtr->callingMask & mask))
  {
   int oldMask = filePtr->mask & mask;
   int oldWait = filePtr->waitMask & mask;
   int (*check)(PerlIOHandler *) = NULL;
   /* Prepare to poll */
   switch (mask)
    {
     case TCL_EXCEPTION:
      check = PerlIO_has_exception;
      break;
     case TCL_WRITABLE:
      check = PerlIO_is_writable;
      break;
     case TCL_READABLE:
      check = PerlIO_is_readable;
      break;
     default:
      croak("Invalid wait type %d",mask);
    }

   /* Inhibit callbacks */
   filePtr->waitMask |= mask;

   /* Watch handle if we are not already */
   if (!oldMask)
    PerlIO_watch(filePtr);

   while (!(*check)(filePtr))
    {
     Tcl_DoOneEvent(0);
    }

   /* Restore watch state */
   filePtr->waitMask = (filePtr->waitMask&~mask)|oldWait;
   PerlIO_watch(filePtr);

   /* Re-enable callbacks */
   /* Consume the readiness */
   filePtr->readyMask &= ~mask;
  }
}

void
TkPerlIO_debug(filePtr,s)
PerlIOHandler *filePtr;
char *s;
{
 dTHX; /* FIXME */
 PerlIO *ip = IoIFP(filePtr->io);
 PerlIO *op = IoOFP(filePtr->io);
 int ifd    = (ip) ? PerlIO_fileno(ip) : -1;
 int ofd    = (op) ? PerlIO_fileno(op) : -1;
 LangDebug("%s: ip=%p count=%d, op=%p count=%d\n",s,
           ip,PerlIO_get_cnt(ip),
           op,PerlIO_get_cnt(op));
}

static void
PerlIOSetupProc(ClientData data, int flags)
{
 static Tcl_Time blockTime = {0, 0};
 if (flags & TCL_FILE_EVENTS)
  {
   PerlIOHandler *filePtr = firstPerlIOHandler;
   while (filePtr != NULL)
    {
     /* file is ready do not block */
     if ((filePtr->mask & TCL_READABLE)
          && PerlIO_is_readable(filePtr))
      Tcl_SetMaxBlockTime(&blockTime);
     if ((filePtr->mask & TCL_WRITABLE)
          && PerlIO_is_writable(filePtr))
      Tcl_SetMaxBlockTime(&blockTime);
     if ((filePtr->mask & TCL_EXCEPTION)
          && PerlIO_has_exception(filePtr))
      Tcl_SetMaxBlockTime(&blockTime);
     filePtr = filePtr->nextPtr;
    }
  }
}

static int
PerlIOEventProc(Tcl_Event *evPtr, int flags)
{
 if (flags & TCL_FILE_EVENTS)
  {
   PerlIOEvent *fileEvPtr = (PerlIOEvent *) evPtr;
   PerlIOHandler *filePtr = firstPerlIOHandler;
   dTHX;
   /*
    * Search through the file handlers to find the one whose handle matches
    * the event.  We do this rather than keeping a pointer to the file
    * handler directly in the event, so that the handler can be deleted
    * while the event is queued without leaving a dangling pointer.
    */

   while (filePtr != NULL)
    {
     if (filePtr->io == fileEvPtr->io)
      {
       int doMask;
       /*
        * The code is tricky for two reasons:
        * 1. The file handler's desired events could have changed
        *    since the time when the event was queued, so AND the
        *    ready mask with the desired mask.
        * 2. The file could have been closed and re-opened since
        *    the time when the event was queued.  This is why the
        *    ready mask is stored in the file handler rather than
        *    the queued event:  it will be zeroed when a new
        *    file handler is created for the newly opened file.
        */
       PerlIO_MaskCheck(filePtr);

       /* clear bits nobody cares about */
       filePtr->readyMask &= filePtr->mask;

       /* Decide which callbacks will be called */
       doMask = (filePtr->readyMask) & (~(filePtr->waitMask)) & (filePtr->handlerMask);

       /* clear bits we are going to callback */
       filePtr->readyMask &= ~doMask;
       filePtr->pending = 0;

       if ((doMask & TCL_READABLE) && filePtr->readHandler)
        {
         SV *sv = filePtr->readHandler;
         ENTER;
         SAVETMPS;
         SvREFCNT_inc(filePtr->mysv);
         filePtr->extraRefs++;
         filePtr->callingMask |= TCL_READABLE;
         LangPushCallbackArgs(&sv);
         LangCallCallback(sv,G_DISCARD);
         filePtr->callingMask &= ~TCL_READABLE;
         filePtr->extraRefs--;
         SvREFCNT_dec(filePtr->mysv);
         FREETMPS;
         LEAVE;
        }
       if ((doMask & TCL_WRITABLE) && filePtr->writeHandler)
        {
         SV *sv = filePtr->writeHandler;
         ENTER;
         SAVETMPS;
         SvREFCNT_inc(filePtr->mysv);
         filePtr->extraRefs++;
         filePtr->callingMask |= TCL_WRITABLE;
         LangPushCallbackArgs(&sv);
         LangCallCallback(sv,G_DISCARD);
         filePtr->callingMask &= ~TCL_WRITABLE;
         filePtr->extraRefs--;
         SvREFCNT_dec(filePtr->mysv);
         FREETMPS;
         LEAVE;
        }
       if ((doMask & TCL_EXCEPTION) && filePtr->exceptionHandler)
        {
         SV *sv = filePtr->exceptionHandler;
         ENTER;
         SAVETMPS;
         SvREFCNT_inc(filePtr->mysv);
         filePtr->extraRefs++;
         filePtr->callingMask |= TCL_EXCEPTION;
         LangPushCallbackArgs(&sv);
         LangCallCallback(sv,G_DISCARD);
         filePtr->callingMask &= ~TCL_EXCEPTION;
         filePtr->extraRefs--;
         SvREFCNT_dec(filePtr->mysv);
         FREETMPS;
         LEAVE;
        }
       break;
      }
     filePtr = filePtr->nextPtr;
    }
   return 1;    /* Say we have handled event */
  }
 return 0;      /* Event is deferred */
}

static void
PerlIOCheckProc(ClientData data, int flags)
{
 if (flags & TCL_FILE_EVENTS)
  {
   PerlIOEvent *fileEvPtr;
   PerlIOHandler *filePtr = firstPerlIOHandler;
   while (filePtr)
    {
     PerlIO_MaskCheck(filePtr);
     if ((filePtr->readyMask & ~filePtr->waitMask & filePtr->handlerMask)
         && !filePtr->pending)
      {
       fileEvPtr = (PerlIOEvent *) ckalloc(sizeof(PerlIOEvent));
       fileEvPtr->io = filePtr->io;
       Tcl_QueueProcEvent(PerlIOEventProc, (Tcl_Event *) fileEvPtr, TCL_QUEUE_TAIL);
       filePtr->pending = 1;
      }
     filePtr = filePtr->nextPtr;
    }
  }
}

static void
PerlIOExitHandler(ClientData clientData)
{
 Tcl_DeleteEventSource(PerlIOSetupProc, PerlIOCheckProc, NULL);
 initialized = 0;
}

static void
PerlIOEventInit(void)
{
 initialized = 1;
 firstPerlIOHandler = NULL;
 Tcl_CreateEventSource(PerlIOSetupProc, PerlIOCheckProc, NULL);
 Tcl_CreateExitHandler(PerlIOExitHandler, NULL);
}

PerlIOHandler *
SVtoPerlIOHandler(sv)
SV *sv;
{
 dTHX; /* FIXME */
 if (sv_isa(sv,"Tk::Event::IO"))
  return (PerlIOHandler *) SvPVX(SvRV(sv));
 croak("Not an Tk::Event::IO");
 return NULL;
}


SV *
PerlIO_TIEHANDLE(class, fh, mask)
char *class;
SV *fh;
int mask;                         /* OR'ed TCL_READABLE, TCL_WRITABLE, and TCL_EXCEPTION */
{
 dTHX; /* FIXME */
 HV *stash = gv_stashpv(class, TRUE);
 GV *tmpgv = (GV *) newSV(0);
 IO *tmpio = newIO();
 IO *io = sv_2io(fh);
 SV *obj = newSV(sizeof(PerlIOHandler));
 PerlIOHandler *filePtr = (PerlIOHandler *)SvPVX(obj);
 gv_init(tmpgv,stash,"Foo",3,0);
 GvIOp(tmpgv) = tmpio;
 if (!initialized)
  PerlIOEventInit();
 Zero(filePtr,1,PerlIOHandler);
 filePtr->io          = io;
 filePtr->handle      = SvREFCNT_inc(fh);
 filePtr->untied      = tmpgv;
 filePtr->readyMask   = 0;
 filePtr->handlerMask = 0;
 filePtr->mask        = 0;
 filePtr->waitMask    = mask;
 filePtr->pending     = 0;
 filePtr->nextPtr     = firstPerlIOHandler;
 filePtr->mysv        = obj;
 filePtr->extraRefs   = 0;
 firstPerlIOHandler   = filePtr;
 PerlIO_watch(filePtr);
 obj = newRV_noinc(obj);
 sv_bless(obj, stash);
 return obj;
}

void
PerlIO_DESTROY(thisPtr)
PerlIOHandler *thisPtr;
{
 dTHX; /* FIXME */
 if (initialized)
  {
   PerlIOHandler **link = &firstPerlIOHandler;
   PerlIOHandler *filePtr;
   while ((filePtr = *link))
    {
     if (!thisPtr || filePtr == thisPtr)
      {
       IO *tmpio;
       *link = filePtr->nextPtr;
       PerlIO_unwatch(filePtr);
       if (filePtr->readHandler)
        {
         LangFreeCallback(filePtr->readHandler);
         filePtr->readHandler = NULL;
        }
       if (filePtr->writeHandler)
        {
         LangFreeCallback(filePtr->writeHandler);
         filePtr->writeHandler = NULL;
        }
       if (filePtr->exceptionHandler)
        {
         LangFreeCallback(filePtr->exceptionHandler);
         filePtr->exceptionHandler = NULL;
        }
       tmpio = GvIOp(filePtr->untied);
       IoIFP(tmpio) = NULL;
       IoOFP(tmpio) = NULL;
       SvREFCNT_dec(filePtr->untied);
       SvREFCNT_dec(filePtr->handle);
      }
     else
      {
       link = &filePtr->nextPtr;
      }
    }
  }
}




SV *
PerlIO_handler(filePtr, mask, cb)
PerlIOHandler *filePtr;
int mask;
LangCallback *cb;
{
 dTHX; /* FIXME */
 STRLEN len;
 if (cb)
  {
   if (!SvROK(cb))
    cb = NULL;
   if (mask & TCL_READABLE)
    {
     if (filePtr->readHandler)
      {
       LangFreeCallback(filePtr->readHandler);
       filePtr->readHandler = NULL;
      }
     if (cb)
      {
       filePtr->readHandler = LangCopyCallback(cb);
      }
    }
   if (mask & TCL_WRITABLE)
    {
     if (filePtr->writeHandler)
      {
       LangFreeCallback(filePtr->writeHandler);
       filePtr->writeHandler = NULL;
      }
     if (cb)
      {
       filePtr->writeHandler = LangCopyCallback(cb);
      }
    }
   if (mask & TCL_EXCEPTION)
    {
     if (filePtr->exceptionHandler)
      {
       LangFreeCallback(filePtr->exceptionHandler);
       filePtr->exceptionHandler = NULL;
      }
     if (cb)
      {
       filePtr->exceptionHandler = LangCopyCallback(cb);
      }
    }
   if (cb)
    {
     filePtr->handlerMask |= mask;
    }
   else
    {
     filePtr->handlerMask &= ~mask;
    }
   PerlIO_watch(filePtr);
   /* What we return will be made mortal and free'd by caller
      which will balance the effective REFCNT_inc
      in LangMakeCallback()
    */
   return (cb) ? cb : &PL_sv_undef;
  }
 else
  {
   switch (mask)
    {
     case TCL_EXCEPTION:
      cb = filePtr->exceptionHandler;
      break;
     case TCL_WRITABLE:
      cb = filePtr->writeHandler;
      break;
     case TCL_READABLE:
      cb = filePtr->readHandler;
      break;
     default:
      croak("Invalid handler type %d",mask);
    }
   /* We need the REFCNT_inc from LangCallbackObj() to
      counter the mortal-ize of return result
    */
   return (cb) ? LangCallbackObj(cb) : &PL_sv_undef;
  }
}

void
PerlIO_Cleanup(PerlIOHandler *filePtr)
{
 PerlIO_unwatch(filePtr);
 if (filePtr->readHandler)
  {
   LangFreeCallback(filePtr->readHandler);
   filePtr->readHandler = NULL;
  }
 if (filePtr->writeHandler)
  {
   LangFreeCallback(filePtr->writeHandler);
   filePtr->writeHandler = NULL;
  }
 if (filePtr->exceptionHandler)
  {
   LangFreeCallback(filePtr->exceptionHandler);
   filePtr->exceptionHandler = NULL;
  }
}

void
PerlIO_UNTIE(SV *sv,IV count)
{
 PerlIOHandler *thisPtr = SVtoPerlIOHandler(sv);
 if ((count-thisPtr->extraRefs) > 0) {
  warn("untie called with %ld references",count);
 }
}

void
PerlIO_END(void)
{
 PerlIO_DESTROY(NULL);
}

static void
SetupProc(clientData,flags)
ClientData clientData;
int flags;
{
 dTHX; /* FIXME */
 dSP;
 ENTER;
 SAVETMPS;
 PUSHMARK(sp);
 XPUSHs(sv_2mortal(newRV((SV *)clientData)));
 XPUSHs(sv_2mortal(newSViv(flags)));
 PUTBACK;
 perl_call_method("setup",G_VOID);
 FREETMPS;
 LEAVE;
}

static void
CheckProc(clientData,flags)
ClientData clientData;
int flags;
{
 dTHX; /* FIXME */
 dSP;
 ENTER;
 SAVETMPS;
 PUSHMARK(sp);
 XPUSHs(sv_2mortal(newRV((SV *)clientData)));
 XPUSHs(sv_2mortal(newSViv(flags)));
 PUTBACK;
 perl_call_method("check",G_VOID);
 FREETMPS;
 LEAVE;
}

typedef struct
{
 Tcl_Event sv;
 SV *obj;
} PerlEvent;

static int
EventProc(evPtr, flags)
Tcl_Event *evPtr;
int flags;
{PerlEvent *pe = (PerlEvent *) evPtr;
 int code = 1;
 int count;
 dTHX; /* FIXME */
 dSP;
 ENTER;
 SAVETMPS;
 PUSHMARK(sp);
 XPUSHs(pe->obj);
 XPUSHs(sv_2mortal(newSViv(flags)));
 PUTBACK;
 count = perl_call_method("event",G_SCALAR);
 SPAGAIN;
 if (count)
  {
   SV *result = POPs;
   code = SvIV(result);
  }
 PUTBACK;
 FREETMPS;
 LEAVE;
 return code;
}

#ifndef NSIG
#define NSIG 64
#endif

static Signal_t handle_signal _((int sig));
static Signal_t (*old_handler) _((int sig)) = NULL;
static char seen[NSIG];

Tcl_AsyncHandler async[NSIG];

static Signal_t
handle_signal(sig)
int sig;
{
 if (sig >= 0 && sig < NSIG)
  {
   if (async[sig])
    Tcl_AsyncMark(async[sig]);
  }
}

void
HandleSignals(pTHX)
{
#if defined(PATCHLEVEL) && (PATCHLEVEL < 5)
 croak("Cannot HandleSignals with before perl5.005");
#else
 if (PL_sighandlerp != handle_signal)
  {
   old_handler    = PL_sighandlerp;
   PL_sighandlerp = handle_signal;
  }
#endif
}

XS(XS_Tk__Callback_Call)
{
 dXSARGS;
 STRLEN na;
 int i;
 int count;
 SV *cb = ST(0);
 SV *err;
 int wantarray = GIMME;
 if (!items)
  {
   croak("No arguments");
  }
 LangPushCallbackArgs(&cb);
 SPAGAIN;
 for (i=1; i < items; i++)
  {
   if (SvTAINTED(ST(i)))
    {
     croak("Tcl_Obj * %d to callback %"SVf" is tainted",i,ST(i));
    }
   XPUSHs(ST(i));
  }
 PUTBACK;

 count = LangCallCallback(cb,GIMME|G_EVAL);
 SPAGAIN;

 err = ERRSV;
 if (SvTRUE(err))
  {
   SV *save = sv_2mortal(newSVsv(err));
   STRLEN len;
   char *s = SvPV(save, len);
   if (len >= 11 && !strncmp("_TK_EXIT_(",s,10))
    {
     char *e = strchr(s+=10,')');
     sv_setpvn(save,s,e-s);
     TclpExit(SvIV(save));
    }
   else
    {
     LangDebug("%s error:%.*s\n",__FUNCTION__,len,s);
     croak("%s",s);
    }
  }
 if (count)
  {
   for (i=1; i <= count; i++)
    {
     ST(i-1) = sp[i-count];
    }
  }
 else
  {
   if (!(wantarray & G_ARRAY))
    {
     ST(0) = &PL_sv_undef;
     count++;
    }
  }
 PUTBACK;
 XSRETURN(count);
}

static void
Callback_DESTROY(SV *sv)
{
}

#define Tcl_setup(obj,flags)
#define Tcl_check(obj,flags)

#define Const_READABLE() TCL_READABLE
#define Const_WRITABLE() TCL_WRITABLE
#define Const_EXCEPTION() TCL_EXCEPTION

#define Const_DONT_WAIT()     (TCL_DONT_WAIT)
#define Const_WINDOW_EVENTS() (TCL_WINDOW_EVENTS)
#define Const_FILE_EVENTS()   (TCL_FILE_EVENTS)
#define Const_TIMER_EVENTS()  (TCL_TIMER_EVENTS)
#define Const_IDLE_EVENTS()   (TCL_IDLE_EVENTS)
#define Const_ALL_EVENTS()    (TCL_ALL_EVENTS)

#define Event_INIT()

extern XSdec(XS_Tk__Event_INIT);
XS(XS_Tk__Event_INIT)
{
 dXSARGS;
 install_vtab(aTHX_ "TkeventVtab",TkeventVGet(),sizeof(TkeventVtab));
 XSRETURN_EMPTY;
}

#define pTk_exit(status) TclpExit(status)

#define IsParentProcess() (PerlProc_getpid() == parent_pid)

void pTk_END()
{
 dTHX;
 if (IsParentProcess())
  {
   Tcl_Finalize();
  }
}

MODULE = Tk	PACKAGE = Tk	PREFIX = pTk_

PROTOTYPES: ENABLE

void
pTk_IsParentProcess(...)
CODE:
 {
  ST(0) = (IsParentProcess()) ? &PL_sv_yes : &PL_sv_no;
  XSRETURN(1);
 }

void
pTk_END()

void
pTk_exit(status = 0)
int	status

MODULE = Tk::Event	PACKAGE = Tk::Callback	PREFIX = Callback_
PROTOTYPES: DISABLE

void
Callback_DESTROY(object)
SV *	object

MODULE = Tk::Event	PACKAGE = Tk::Event::IO PREFIX = Const_

PROTOTYPES: ENABLE

int
Const_READABLE()

int
Const_WRITABLE()

int
Const_EXCEPTION()

MODULE = Tk::Event	PACKAGE = Tk::Event PREFIX = Const_

PROTOTYPES: ENABLE

IV
Const_DONT_WAIT()

IV
Const_WINDOW_EVENTS()

IV
Const_FILE_EVENTS()

IV
Const_TIMER_EVENTS()

IV
Const_IDLE_EVENTS()

IV
Const_ALL_EVENTS()

MODULE = Tk::Event	PACKAGE = Tk::Event::IO	PREFIX = TkPerlIO_

PROTOTYPES: DISABLE

void
TkPerlIO_debug(filePtr,s)
PerlIOHandler *	filePtr
char *	s

MODULE = Tk::Event	PACKAGE = Tk::Event::IO	PREFIX = PerlIO_

PROTOTYPES: DISABLE

SV *
PerlIO_TIEHANDLE(class,fh,mask = 0)
char *	class
SV *	fh
int	mask

SV *
PerlIO_handle(filePtr)
PerlIOHandler *	filePtr

void
PerlIO_unwatch(filePtr)
PerlIOHandler *	filePtr

void
PerlIO_wait(filePtr,mode)
PerlIOHandler *	filePtr
int		mode

int
PerlIO_is_readable(filePtr)
PerlIOHandler *	filePtr

int
PerlIO_has_exception(filePtr)
PerlIOHandler *	filePtr

int
PerlIO_is_writable(filePtr)
PerlIOHandler *	filePtr

SV *
PerlIO_handler(filePtr, mask = TCL_READABLE, cb = NULL)
PerlIOHandler *	filePtr
int		mask
LangCallback *	cb

void
PerlIO_DESTROY(filePtr)
PerlIOHandler *	filePtr

void
PerlIO_UNTIE(filePtr,count)
SV *		filePtr
IV		count


void
PerlIO_END()

MODULE = Tk::Event	PACKAGE = Tk::Event::Source	PREFIX = Tcl_

void
Tcl_setup(obj,flags)
SV *	obj
int	flags

void
Tcl_check(obj,flags)
SV *	obj
int	flags

void
new(class,sv)
char *	class
SV *	sv
CODE:
{
 HV *stash = gv_stashpv(class, TRUE);
 if (SvROK(sv))
  {
   sv = newSVsv(sv);
  }
 else
  {
   sv = newRV(sv);
  }
 sv_bless(sv, stash);
 Tcl_CreateEventSource(SetupProc,CheckProc,(ClientData)SvRV(sv));
 ST(0) = sv;
}

void
delete(sv)
SV *	sv
CODE:
{
 SV *obj = SvRV(sv);
 Tcl_DeleteEventSource(SetupProc,CheckProc,(ClientData)obj);
 SvREFCNT_dec(obj);
}

MODULE = Tk::Event	PACKAGE = Tk::Event	PREFIX = Tcl_

double
dGetTime()
CODE:
 {Tcl_Time time;
  Tcl_GetTime(&time);
  RETVAL = (double) time.sec + time.usec * 1e-6;
 }
OUTPUT:
 RETVAL

void
Tcl_Exit(status)
int	status

int
Tcl_DoOneEvent(flags)
int	flags

void
Tcl_QueueEvent(evPtr, position = TCL_QUEUE_TAIL)
Tcl_Event *		evPtr
Tcl_QueuePosition	position

void
Tcl_QueueProcEvent(proc, evPtr, position  = TCL_QUEUE_TAIL)
Tcl_EventProc *		proc
Tcl_Event *		evPtr
Tcl_QueuePosition	position

int
Tcl_ServiceEvent(flags)
int	flags

Tcl_TimerToken
Tcl_CreateTimerHandler(milliseconds, proc, clientData = NULL)
int		milliseconds
Tcl_TimerProc *	proc
ClientData	clientData

void
Tcl_DeleteTimerHandler(token)
Tcl_TimerToken	token

void
Tcl_SetMaxBlockTime(sec, usec = 0)
double	sec
IV	usec
CODE:
 {
  Tcl_Time ttime;
  ttime.sec  = sec;
  ttime.usec = (sec - ttime.sec) * 1e6 + usec;
  Tcl_SetMaxBlockTime(&ttime);
 }

void
Tcl_DoWhenIdle(proc,clientData = NULL)
Tcl_IdleProc *	proc
ClientData	clientData

void
Tcl_CancelIdleCall(proc,clientData = NULL)
Tcl_IdleProc *	proc
ClientData	clientData

void
Tcl_CreateExitHandler(proc,clientData = NULL)
Tcl_ExitProc *	proc
ClientData	clientData

void
Tcl_CreateFileHandler(fd, mask, proc, clientData = NULL)
int		fd
int		mask
Tcl_FileProc *	proc
ClientData	clientData

void
Tcl_DeleteFileHandler(fd)
int	fd

void
Tcl_Sleep(ms)
int	ms

int
Tcl_GetServiceMode()

int
Tcl_SetServiceMode(mode)
int	mode

int
Tcl_ServiceAll()

void
HandleSignals()
CODE:
 {
  HandleSignals(aTHX);
 }

MODULE = Tk::Event	PACKAGE = Tk::Event	PREFIX = Event_

void
Event_CleanupGlue()

MODULE = Tk::Event	PACKAGE = Tk::Event

PROTOTYPES: DISABLE

BOOT:
 {
#ifdef pWARN_NONE
  SV *old_warn = PL_curcop->cop_warnings;
  PL_curcop->cop_warnings = pWARN_NONE;
#endif
  newXS("Tk::Event::INIT", XS_Tk__Event_INIT, file);
#ifdef pWARN_NONE
 PL_curcop->cop_warnings = old_warn;
#endif
  newXS("Tk::Callback::Call", XS_Tk__Callback_Call, __FILE__);
  install_vtab(aTHX_ "TkeventVtab",TkeventVGet(),sizeof(TkeventVtab));
  sv_setiv(FindVarName(aTHX_ "LangDebug",GV_ADD|GV_ADDMULTI),1);
  TclInitSubsystems(SvPV_nolen(get_sv("0",FALSE)));
  parent_pid = PerlProc_getpid();
 }



