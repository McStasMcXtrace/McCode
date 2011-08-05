/*
  Copyright (c) 1995-2004 Nick Ing-Simmons. All rights reserved.
  This program is free software; you can redistribute it and/or
  modify it under the same terms as Perl itself.
*/
#define PERL_NO_GET_CONTEXT
#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>
#include <patchlevel.h>


#include "tkGlue.def"
#define TCL_EVENT_IMPLEMENT

#include "pTk/tkInt.h"
#include "pTk/Lang.h"
#include "pTk/tkEvent.h"
#include "tkGlue.h"
/*
   For perl a "callback" is an SV
   - Simple case of ref to CV
   - A ref to an AV, 1st element is "method" rest are
   args to be passed on EACH call (before/after any Tk args ?)
   Akin to fact that TCL/TK evals an arbitary string
   (Perl code could pre-scan args and convert Malcolm's
   -method/-slave into this form.)
   - Special case of a "window" reference, treat 1st arg
   as a method. (e.g. for TCL/TK's .menu post x y )

 */


LangCallback *
LangMakeCallback(sv)
SV *sv;
{
 dTHX; /* FIXME */
 if (sv)
  {
   dTHX;
   AV *av;
   int old_taint = PL_tainted;
   if (SvTAINTED(sv))
    croak("Attempt to make callback from tainted %"SVf, sv);
   PL_tainted = 0;
   /* Case of a Tcl_Merge which returns an AV * */
   if (SvTYPE(sv) == SVt_PVAV)
    {
     sv = newRV(sv);
     warn("Making callback from array not reference");
    }
   else if (!SvOK(sv) || (SvPOK(sv) && SvCUR(sv) == 0))
    return sv;
   else if (SvREADONLY(sv) || SvROK(sv) || SvPOK(sv))
    sv = newSVsv(sv);  /* FIXME: Always do this ??? */
   else
    {
     SvREFCNT_inc(sv);
    }
   if (!SvROK(sv))
    {
     sv = newRV_noinc(sv);
    }
   else
    {
     if (SvTYPE(SvRV(sv)) == SVt_PVCV)
      {
       AV *av = newAV();
#if 0
       /* This leaks */
       av_push(av,SvREFCNT_inc(sv));  /* Increment REFCNT ! */
#else
       av_push(av,sv);  /* changed by SRT: do not increment REFCNT ! */
#endif
       sv = newRV_noinc((SV *) av);
      }
    }
   if (SvTYPE(SvRV(sv)) == SVt_PVAV)
    {
     if (av_len((AV *) SvRV(sv)) < 0)
      {
       croak("Empty list is not a valid callback");
      }
    }
   if (!sv_isa(sv,"Tk::Callback"))
    {
     HV *stash = gv_stashpv("Tk::Callback", TRUE);
     sv = sv_bless(sv, stash);
    }
   PL_tainted = old_taint;
  }
 if (sv && SvTAINTED(sv))
  croak("Making callback tainted %"SVf, sv);
 return sv;
}

LangCallback *
LangCopyCallback(sv)
SV *sv;
{
 if (sv)
  {
#if !defined(__GNUC__) || defined(__STRICT_ANSI__) || defined(PERL_GCC_PEDANTIC)
 /* Unless using GCC extensions we need PL_Sv */
   dTHX;
#endif
   SvREFCNT_inc(sv);
  }
 return sv;
}

void
LangFreeCallback(sv)
SV *sv;
{
 dTHX; /* FIXME */
 if (!sv_isa(sv,"Tk::Callback"))
  {
   warn("Free non-Callback %p RV=%p",sv,SvRV(sv));
   /*//   abort();*/
  }
 SvREFCNT_dec(sv);
}

Tcl_Obj *
LangCallbackObj(sv)
SV *sv;
{
 dTHX; /* FIXME */
 if (sv && !sv_isa(sv,"Tk::Callback"))
  {
   warn("non-Callback arg");
   sv_dump(sv);
  }
 return SvREFCNT_inc(sv);
}

Tcl_Obj *
LangOldCallbackArg(sv,file,line)
SV *sv;
char *file;
int line;
{
 dTHX; /* FIXME */
 LangDebug("%s:%d: LangCallbackArg is deprecated\n",file,line);
 sv = LangCallbackObj(sv);
 SvREFCNT_dec(sv);
 return sv;
}

int
LangCallCallback(sv, flags)
SV *sv;
int flags;
{
 dTHX; /* FIXME */
 dSP;
 STRLEN na;
 I32 myframe = TOPMARK;
 I32 count;
 ENTER;
 if (SvGMAGICAL(sv))
  mg_get(sv);
 if (SvTAINTED(sv))
  {
   croak("Call of tainted value %"SVf,sv);
  }
 if (!SvOK(sv))
  {
   char *s = "Call of undefined value";
   sv_setpvn(ERRSV,s,strlen(s));
   abort();
   return 0;
  }
 if (flags & G_EVAL)
  {
   CV *cv  = perl_get_cv("Tk::__DIE__", FALSE);
   if (cv)
    {
     HV *sig  = perl_get_hv("SIG",TRUE);
     SV **old = hv_fetch(sig, "__DIE__", 7, TRUE);
     save_svref(old);
     hv_store(sig,"__DIE__",7,newRV((SV *) cv),0);
    }
  }

 /* Belt-and-braces fix to callback destruction issues */
 /* Increment refcount of thing while we call it */
 SvREFCNT_inc(sv);
 /* Arrange to have it decremented on scope exit */
 save_freesv(sv);

 if (SvTYPE(sv) == SVt_PVCV)
  {
   count = perl_call_sv(sv, flags);
  }
 else if (SvROK(sv) && SvTYPE(SvRV(sv)) == SVt_PVCV)
  {
   count = perl_call_sv(SvRV(sv), flags);
  }
 else
  {
   SV **top = PL_stack_base + myframe + 1;
   SV *obj = *top;
   if (SvGMAGICAL(obj))
    mg_get(obj);
   if (SvPOK(sv) && SvROK(obj) && SvOBJECT(SvRV(obj)))
    {
     count = perl_call_method(SvPV_nolen(sv), flags);
    }
   else if (SvPOK(obj) && SvROK(sv) && SvOBJECT(SvRV(sv)))
    {
     *top = sv;
     count = perl_call_method(SvPV_nolen(obj), flags);
    }
   else
    {
     count = perl_call_sv(sv, flags);
    }
  }
 LEAVE;
 return count;
}

void
LangPushCallbackArgs(SV **svp)
{
 dTHX; /* FIXME */
 SV *sv = *svp;
 dSP;
 STRLEN na;
 if (SvTAINTED(sv))
  {
   croak("Tainted callback %"SVf,sv);
  }
 if (SvROK(sv) && SvTYPE(SvRV(sv)) != SVt_PVCV)
  sv = SvRV(sv);
 PUSHMARK(sp);
 if (SvTYPE(sv) == SVt_PVAV)
  {
   AV *av = (AV *) sv;
   int n = av_len(av) + 1;
   SV **x = av_fetch(av, 0, 0);
   if (x)
    {
     int i = 1;
     sv = *x;
     if (SvTAINTED(sv))
      {
       croak("Callback slot 0 tainted %"SVf,sv);
      }
     for (i = 1; i < n; i++)
      {
       x = av_fetch(av, i, 0);
       if (x)
        {SV *arg = *x;
         if (SvTAINTED(arg))
          {
           croak("Callback slot %d tainted %"SVf,i,arg);
          }
         XPUSHs(sv_mortalcopy(arg));
        }
       else
        XPUSHs(&PL_sv_undef);
      }
    }
   else
    {
     sv = &PL_sv_undef;
    }
  }
 *svp = sv;
 PUTBACK;
}

int
LangCmpCallback(a, b)
SV *a;
SV *b;
{
 dTHX; /* FIXME */
 if (a == b)
  return 1;
 if (!a || !b)
  return 0;
 if (SvTYPE(a) != SvTYPE(b))
  return 0;
 switch(SvTYPE(a))
  {
   case SVt_PVAV:
    {
     AV *aa = (AV *) a;
     AV *ba = (AV *) a;
     if (av_len(aa) != av_len(ba))
      return 0;
     else
      {
       IV i;
       for (i=0; i <= av_len(aa); i++)
        {
         SV **ap = av_fetch(aa,i,0);
         SV **bp = av_fetch(ba,i,0);
         if (ap && !bp)
          return 0;
         if (bp && !ap)
          return 0;
         if (ap && bp && !LangCmpCallback(*ap,*bp))
          return 0;
        }
       return 1;
      }
    }
   default:
   case SVt_PVGV:
   case SVt_PVCV:
    return 0;
#ifdef HAS_REAL_SVT_RV
   case SVt_RV:
#endif
   case SVt_IV:
   case SVt_NV:
   case SVt_PV:
   case SVt_PVIV:
   case SVt_PVNV:
    if (SvROK(a) && SvROK(b))
     {
      return LangCmpCallback(SvRV(a),SvRV(b));
     }
    else
     {STRLEN asz;
      char *as = SvPV(a,asz);
      STRLEN bsz;
      char *bs = SvPV(b,bsz);
      if (bsz != asz)
       return 0;
      return !memcmp(as,bs,asz);
     }
  }
}

VOID *
Tcl_GetThreadData(keyPtr, size)
    Tcl_ThreadDataKey *keyPtr;	/* Identifier for the data chunk */
    int size;			/* Size of storage block */
{
    VOID *result;
    if (*keyPtr == NULL) {
	result = (VOID *)ckalloc((size_t)size);
	memset((char *)result, 0, (size_t)size);
	*keyPtr = (Tcl_ThreadDataKey)result;
	/* TclRememberDataKey(keyPtr); */
    }
    result = *(VOID **)keyPtr;
    return result;
}

VOID *
TclThreadDataKeyGet(keyPtr)
    Tcl_ThreadDataKey *keyPtr;	/* Identifier for the data chunk,
				 * really (pthread_key_t **) */
{
    char *result = *(char **)keyPtr;
    return (VOID *)result;
}


Tcl_ThreadId
Tcl_GetCurrentThread(void)
{
#if 0
 warn("%s not implemented",__FUNCTION__);
 abort();
#endif
 return 0;
}

void
TclpAsyncMark(async)
Tcl_AsyncHandler async;		/* Token for handler. */
{
#ifdef WIN32
 static DWORD mainThreadId;
 if (!mainThreadId)
   mainThreadId = GetCurrentThreadId();


    /*
     * Need a way to kick the Windows event loop and tell it to go look at
     * asynchronous events.
     */

    PostThreadMessage(mainThreadId, WM_USER, 0, 0);
#endif
}

void
TclpInitLock(void)
{
}

void
TclpExit(int status)
{
/*
 * Tk::exit comes here - via Tcl_Exit()
 * Once upon a time and we just called my_exit()
 * but that causes perl to longjmp() out of tkEvent.c and tkBind.c
 * which have stored stack addresses in Tk structures.
 * The die scheme works round this but imposes cost on normal execution.
 */
 dTHX; /* FIXME */
 if (PL_in_eval)
  croak("_TK_EXIT_(%d)\n",status);
 else
  my_exit(status);
}

void
TclpInitUnlock(void)
{
}

void
TclpInitPlatform(void)
{
}

void
TclInitIOSubsystem(void)
{
}

void
TclInitObjSubsystem(void)
{
}

void
TclFinalizeIOSubsystem(void)
{
}

void
TclFinalizeThreadData(void)
{
}

void
TclFinalizeObjSubsystem(void)
{
}

void
LangAsyncCheck(void)
{
#ifdef PERL_ASYNC_CHECK
 dTHX;
 PERL_ASYNC_CHECK();
#endif
}





