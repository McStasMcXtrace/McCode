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
#ifdef __CYGWIN__
#  undef XS
#  define XS(name) void name(pTHXo_ CV* cv)
#endif

#define Tkgv_fullname(x,y,z) gv_fullname3(x,y,z)

#include "tkGlue.def"
#include "pTk/tkPort.h"
#include "pTk/tkInt.h"
#include "pTk/tix.h"  /* for form */
#include "pTk/tkImgPhoto.h"
#include "pTk/tkImgPhoto.m"
#include "pTk/imgInt.h"
#include "pTk/imgInt.m"
#include "pTk/tkOption.h"
#include "pTk/tkOption_f.h"
#include "pTk/Lang_f.h"
#include "pTk/Xlib.h"
#include "pTk/tk_f.h"
#include "pTk/tkInt_f.h"
#include "pTk/Xlib_f.h"
#include "pTk/tclDecls_f.h"
#include "pTk/tkDecls_f.h"
#include "pTk/tkIntDecls_f.h"
#include "pTk/tkEvent.h"
#include "pTk/tkEvent.m"
#if defined(WIN32) || (defined(__WIN32__) && defined(__CYGWIN__))
#include "pTk/tkWin.h"
#include "pTk/tkWinInt.h"
#include "pTk/tkIntXlibDecls_f.h"
#include "pTk/tkIntPlatDecls_f.h"
#include "pTk/tkPlatDecls_f.h"
#else
#  ifdef OS2
#    include "pTk/tkOS2Int.h"
#  else
#    include "pTk/tkUnixInt.h"
#  endif
#endif
#include "tkGlue.h"
#include "tkGlue_f.h"

DECLARE_EVENT;

/* #define DEBUG_REFCNT /* */

#ifdef WIN32
long DCcount = 0;
void
LangNoteDC(HDC dc,int inc)
{
#ifdef DEBUGGING
 DCcount += inc;
#endif
}

void
LangCheckDC(const char *file,int line)
{
#ifdef DEBUGGING
 if (DCcount)
    LangDebug("%s:%d DCcount %ld\n",file,line,DCcount);
#endif
}
#else
void
LangCheckDC(const char *file,int line)
{
}
#endif


extern Tk_PhotoImageFormat	imgFmtBMP;
#if 0
extern Tk_PhotoImageFormat	imgFmtGIF;
#else
extern Tk_PhotoImageFormat	tkImgFmtGIF;
#endif
extern Tk_PhotoImageFormat	imgFmtXBM;
extern Tk_PhotoImageFormat	imgFmtXPM;

typedef struct
{
 Lang_VarTraceProc *proc;
 ClientData         clientData;
 Tcl_Interp        *interp;
 char              *part2;
 SV                *sv;
} Tk_TraceInfo;

typedef struct
{
 Tcl_Interp *interp;
 SV *cb;
} GenericInfo;

typedef struct Assoc_s
{
 Tcl_InterpDeleteProc *proc;
 ClientData clientData;
} Assoc_t;

static int initialized = 0;


static I32 ec = 0;
static SV *my_watch;

static char XEVENT_KEY[]   = "_XEvent_";
static char GEOMETRY_KEY[] = "_ManageGeometry_";
static char CM_KEY[]       = "_ClientMessage_";
static char ASSOC_KEY[]    = "_AssocData_";
static char FONTS_KEY[]    = "_Fonts_";
static char CMD_KEY[]      = "_CmdInfo_";

#ifndef BASEEXT
#define BASEEXT "Tk"
#endif

typedef XSdec((*XSptr));

static XSdec(XStoSubCmd);
static XSdec(XStoDisplayof);
static XSdec(XStoTk);
static XSdec(XStoBind);
static XSdec(XStoEvent);

extern XSdec(XS_Tk__Widget_SelectionGet);
extern XSdec(XS_Tk__Widget_ManageGeometry);
extern XSdec(XS_Tk__MainWindow_Create);
extern XSdec(XS_Tk__Interp_DESTROY);
extern XSdec(XS_Tk__Widget_BindClientMessage);
extern XSdec(XS_Tk__Widget_PassEvent);
extern XSdec(XS_Tk_INIT);
extern XSdec(XS_Tk_DoWhenIdle);
extern XSdec(XS_Tk_CreateGenericHandler);

#ifdef PERL_MG_UFUNC
#define DECL_MG_UFUNC(name,a,b) PERL_MG_UFUNC(name,a,b)
#else
#define DECL_MG_UFUNC(name,a,b) I32 name(IV a, SV *b)
#endif

extern void  LangPrint _((SV *sv));

static void handle_idle _((ClientData clientData));
static AV *CopyAv _((AV * dst, AV * src));
static void LangCatArg _((SV * out, SV * sv, int refs));
static SV *NameFromCv _((CV * cv));
static AV *FindAv _((pTHX_ Tcl_Interp *interp, char *who, int create, char *key));
static HV *FindHv _((pTHX_ HV *interp, char *who, int create, char *key));
static SV *Blessed _((char *package, SV * sv));
static int PushObjCallbackArgs _((Tcl_Interp *interp, SV **svp,EventAndKeySym *obj));
static int Check_Eval _((Tcl_Interp *interp));
static int handle_generic _((ClientData clientData, XEvent * eventPtr));
static void HandleBgErrors _((ClientData clientData));
static void SetTclResult _((Tcl_Interp *interp,int count));
static int InfoFromArgs _((Lang_CmdInfo *info,Tcl_ObjCmdProc *proc,int mwcd, int items, SV **args));
static I32 InsertArg _((SV **mark,I32 posn,SV *sv));
extern Tk_Window TkToMainWindow _((Tk_Window tkwin));
static int isSwitch _((char *arg));
static void Lang_ClearErrorInfo _((Tcl_Interp *interp));
static void Lang_MaybeError _((Tcl_Interp *interp,int code,char *why));
static int  all_printable _((char *s,int n));
static void Set_widget _((SV *widget));
static SV *tilde_magic _((SV *hv, SV *sv));
static SV *struct_sv   _((void *ptr, STRLEN sz));
static int SelGetProc _((ClientData clientData,
			Tcl_Interp *interp,
			long *portion,
			int numItems,
			int format,
			Atom type,
			Tk_Window tkwin));
static void Perl_GeomRequest _((ClientData clientData,Tk_Window tkwin));
static void Perl_GeomLostSlave _((ClientData clientData, Tk_Window tkwin));

Tcl_ObjCmdProc *LangOptionCommand = (Tcl_ObjCmdProc *)Tk_OptionObjCmd;

static GV *current_widget;
static GV *current_event;

static int
Expire(int code)
{
 return code;
}

#define EXPIRE(args) \
  ( Tcl_SprintfResult args, Expire(TCL_ERROR) )

#ifdef DEBUG_TAINT
#define do_watch() do { if (PL_tainting) taint_proper("tainted", __FUNCTION__); } while (0)
#else
extern void do_watch _((void));
void
do_watch()
{

}
#endif

static void
LangCatAv(pTHX_ SV *out, AV *av, int refs, char *bra)
{
 int n = av_len(av) + 1;
 int i = 0;
 sv_catpvn(out, bra, 1);
 while (i < n)
  {
   SV **x = av_fetch(av, i, 0);
   LangCatArg(out, (x) ? (*x) : &PL_sv_undef, refs);
   if (++i < n)
    sv_catpv(out, ",");
  }
 sv_catpvn(out, bra+1, 1);
}

static void
LangCatArg(out, sv, refs)
SV *out;
SV *sv;
int refs;
{
 dTHX;
 char buf[80];
 if (sv)
  {
   STRLEN na;
   switch(SvTYPE(sv))
    {
     case SVt_PVAV:
      LangCatAv(aTHX_ out, (AV *) sv, refs,"()");
      break;
     case SVt_PVGV:
      {SV *tmp = newSVpv("", 0);
       Tkgv_fullname(tmp,(GV *) sv, Nullch);
       sv_catpv(out,"*");
       sv_catpv(out,SvPV(tmp,na));
       SvREFCNT_dec(tmp);
      }
      break;
     case SVt_PVCV:
      if (CvGV(sv))
       {
        SV *tmp = newSVpv("", 0);
        Tkgv_fullname(tmp, CvGV(sv), Nullch);
        sv_catpv(out,"&");
        sv_catpv(out,SvPV(tmp,na));
        SvREFCNT_dec(tmp);
        break;
       }
     default:
      if (SvOK(sv))
       {
        char *s = "";
        if (SvROK(sv))
         {
          if (SvTYPE(SvRV(sv)) == SVt_PVAV)
           LangCatAv(aTHX_ out, (AV *) SvRV(sv), refs,"[]");
          else if (SvTYPE(SvRV(sv)) == SVt_PVHV)
           {
            SV *hv = SvRV(sv);
            sv_catpv(out,"{}");
            if (refs)
             {
              sprintf(buf, "(%ld%s", (long) SvREFCNT(hv), SvTEMP(hv) ? "t)" : ")");
              sv_catpv(out, buf);
             }
           }
          else
           {
            sv_catpv(out,"\\");
            LangCatArg(out, SvRV(sv), refs);
           }
         }
        else
         {
          if (refs && !SvPOK(sv))
           {
            sprintf(buf, "f=%08lX ", (unsigned long) SvFLAGS(sv));
            sv_catpv(out, buf);
           }
          s = SvPV(sv, na);
         }
        sv_catpv(out, s);
       }
      else
       {
        sv_catpv(out, "undef");
       }
     break;
    }
  }
 if (refs)
  {
   sprintf(buf, "(%ld%s", (long) SvREFCNT(sv), SvTEMP(sv) ? "t)" : ")");
   sv_catpv(out, buf);
  }
}

int
LangNull(sv)
Tcl_Obj * sv;
{
 STRLEN len = 0;
 if (!sv || !SvOK(sv) || (SvPOK(sv) && !SvCUR(sv)))
  return 1;
 return 0;
}

char *
LangMergeString(argc, args)
int argc;
SV **args;
{
 dTHX;
 SV *sv = newSVpv("", 0);
 STRLEN i = 0;
 STRLEN na;
 char *s;
 while (i < (STRLEN) argc)
  {
   LangCatArg(sv, args[i++], 0);
   if (i < (STRLEN) argc)
    sv_catpvn(sv, " ", 1);
  }
 SvPV(sv, i);
 s = strncpy(ckalloc(i + 1), SvPV(sv, na), i);
 s[i] = '\0';
 SvREFCNT_dec(sv);
 return s;
}

void
LangPrint(sv)
SV *sv;
{
 dTHX;
 static char *type_name[] =
 {
  "NULL",
  "IV",
  "NV",
  "RV",
  "PV",
  "PVIV",
  "PVNV",
  "PVMG",
  "PVBM",
  "PVLV",
  "PVAV",
  "PVHV",
  "PVCV",
  "PVGV",
  "PVFM",
  "PVIO"
 };
 if (sv)
  {
   SV *tmp = newSVpv("", 0);
   int type = SvTYPE(sv);
   STRLEN na;
   LangCatArg(tmp, sv, 1);
   PerlIO_printf(PerlIO_stderr(), "0x%p %4s f=%08lx %s\n",
                 sv, (type < 16) ? type_name[type] : "?",
                 (unsigned long) SvFLAGS(sv), SvPV(tmp, na));
   SvREFCNT_dec(tmp);
  }
 else
  {
   PerlIO_printf(PerlIO_stderr(), "0x%p <<!!!\n",sv);
  }
}


#ifdef DEBUG_REFCNT
static Tcl_Interp *IncInterp(Tcl_Interp *interp,char *why);
static Tcl_Interp *DecInterp(Tcl_Interp *interp,char *why);

static Tcl_Interp *
IncInterp(interp,why)
Tcl_Interp *interp;
char *why;
{
 dTHX;
 SvREFCNT_inc((SV *) interp);
 PerlIO_printf(PerlIO_stdout(),"%s %p %ld\n",why,interp,SvREFCNT((SV *) interp));
 return interp;
}

static Tcl_Interp *
DecInterp(interp,why)
Tcl_Interp *interp;
char *why;
{
 dTHX;
 SvREFCNT_dec((SV *) interp);
 PerlIO_printf(PerlIO_stdout(),"%s %p %ld\n",why,interp,SvREFCNT((SV *) interp));
 return interp;
}
#else
#define DecInterp(interp,why) SvREFCNT_dec(interp)
#define IncInterp(interp,why) SvREFCNT_inc(interp)
#endif

#ifdef DEBUG_REFCNT

static SV *
Decrement(SV * sv, char *who)
{
 do_watch();
 if (sv)
  {
   fprintf(stderr, "DEC %s ", who);
   LangPrint(sv);
   SvREFCNT_dec(sv);
   do_watch();
  }
 else
  Tcl_Panic("No sv");
 return sv;
}

static SV *
Increment(SV * sv, char *who)
{
 do_watch();
 if (sv)
  {
   fprintf(stderr, "INC %s ", who);
   LangPrint(sv);
   SvREFCNT_inc(sv);
  }
 else
  Tcl_Panic("No sv");
 return sv;
}
#else
#define Decrement(x,y) SvREFCNT_dec(x)
#define Increment(x,y) SvREFCNT_inc(x)
#endif

SV *
MakeReference(sv)
SV *sv;
{
 dTHX;
 SV *rv = newRV(sv);              /* REFCNT of sv now 2 */
 SvREFCNT_dec(sv);
 return rv;
}

static SV *
Blessed(package, sv)
char *package;
SV *sv;
{
 dTHX;
 HV *stash = gv_stashpv(package, TRUE);
 return sv_bless(sv, stash);
}

#if 0
SV *
TagIt(SV *sv, char *type)
{
 char buffer[1024];
 sprintf(buffer,"Tk::%s_Type",type);
 if (SvROK(sv))
  Blessed(buffer,sv);
 else
  {
   SV *rv = newRV(sv);
   Blessed(buffer,rv);
   SvREFCNT_dec(rv);
  }
 return sv;
}
#else
#define TagIt(sv,type) (sv)
#endif

Tcl_Interp *
Tcl_CreateInterp  _((void))
{
 dTHX;
 HV *hv = newHV();
 SvREFCNT_dec(Blessed("Tk::Interp",newRV((SV *) hv)));
 return hv;
}

HV *
InterpHv(interp,fatal)
Tcl_Interp *interp;
int fatal;
{
 if (interp && SvTYPE((SV *) interp) == SVt_PVHV)
  {
   return interp;
  }
 else if (fatal)
  {
   dTHX;
   STRLEN na;
   warn("%p is not a hash", interp);
   abort();
  }
 return NULL;
}

typedef SV *(*createProc_t)(pTHX);

static SV *
FindXv(pTHX_ Tcl_Interp *interp, char *who, int create,
       char *key, U32 type , createProc_t createProc)
{
 STRLEN len = strlen(key);
 HV *hv = InterpHv(interp,create != 0);
 if (hv)
  {
   if (hv_exists(hv, key, len))
    {
     SV **x = hv_fetch(hv, key, len, 0);
     if (x)
      {
       SV *sv = *x;
       if (type >= SVt_PVAV)
        {
         if (!SvROK(sv) || SvTYPE(SvRV(sv)) != type)
          {
           Tcl_Panic("%s not a %u reference %s", key, type, SvPV_nolen(sv));
          }
         else
          {
           sv = SvRV(sv);
          }
        }
       if (create < 0)
        {
         SvREFCNT_inc((SV *) sv);
         hv_delete(hv, key, len, G_DISCARD);
        }
       return sv;
      }
     else
      Tcl_Panic("%s exists but can't be fetched", key);
    }
   else if (create > 0)
    {
     SV *sv = (*createProc)(aTHX);
     if (sv)
      {
       TagIt(sv,key);
       if (type >= SVt_PVAV)
        {
         hv_store(hv, key, len, MakeReference(sv), 0);
        }
       else
        hv_store(hv, key, len, sv, 0);
      }
     return sv;
    }
  }
 return NULL;
}

static SV *
createHV(pTHX)
{
 return (SV *) newHV();
}

static HV *
FindHv(pTHX_ HV *hv, char *who, int create, char *key)
{
 return (HV *) FindXv(aTHX_ hv, who, create, key, SVt_PVHV, createHV);
}

static SV *
createAV(pTHX)
{
 return (SV *) newAV();
}

static AV *
FindAv(pTHX_ HV *hv, char *who, int create, char *key)
{
 return (AV *) FindXv(aTHX_ hv, who, create, key, SVt_PVAV, createAV);
}

static SV *
createSV(pTHX)
{
 return newSVsv(&PL_sv_undef);
}

static SV *
FindSv(pTHX_ HV *hv, char *who, int create, char *key)
{
 return FindXv(aTHX_ hv, who, create, key, SVt_NULL, createSV);
}


/* Result return handling
   Use the FindXv scheme to create an SV in the interp.
*/
Tcl_Obj *
Tcl_GetObjResult(interp)
Tcl_Interp *interp;
{
 dTHX;
 return FindSv(aTHX_ interp, "Tcl_GetObjResult", 1, "_TK_RESULT_");
}

void
Tcl_ResetResult(interp)
Tcl_Interp *interp;
{
 dTHX;
 if (InterpHv(interp,0))
  {
   /* We delete the entry in the interp.
      This means we are forever create/delete.
      Leaving an SV in the interp might be better, as might
      having Tcl_SetObjResult() which everything now uses
      just store the SV.
    */
   SV *sv = FindSv(aTHX_ interp, "Tcl_ResetResult", -1, "_TK_RESULT_");
   if (sv)
    {
     SvREFCNT_dec(sv);
    }
  }
}

void
Tcl_SetObjResult(interp, sv)
Tcl_Interp *interp;
SV *sv;
{
 dTHX;
 if (InterpHv(interp,0))
  {
   SV *result = Tcl_GetObjResult(interp);
   if (result == sv)
    {
     /* Recent Tk does
         save = Tcl_GetObjResult();
         Tcl_IncrRefCount(save);
         ...
         Tcl_SetObjResult(save);
         Tcl_DecrRefCount(save);
         So nothing more to do here.
     */
     return;
    }
   else
    {
     Tcl_ResetResult(interp);
     SvSetMagicSV(Tcl_GetObjResult(interp), sv);
    }
  }
 /* normal coding in Tk is equivalent to
    Tcl_SetObjResult(interp,Tcl_NewXxxObj());
    and then forget about the Tcl_Obj - i.e. ownership
    is handed to the interp.
    As we have taken a _copy_ we no longer need the
    original.
  */
 Tcl_DecrRefCount(sv);
}

void
Lang_SetBinaryResult(interp, string, len, freeProc)
Tcl_Interp *interp;
char *string;
int len;
Tcl_FreeProc *freeProc;
{
 dTHX;
 do_watch();
 if (string)
  {
   SV *sv = newSVpv(string, len);
   Tcl_SetObjResult(interp, sv);
   if (freeProc != TCL_STATIC && freeProc != TCL_VOLATILE)
    (*freeProc) (string);
  }
 else
  Tcl_ResetResult(interp);
 do_watch();
}

void
Tcl_SetResult(interp, string, freeProc)
Tcl_Interp *interp;
char *string;
Tcl_FreeProc *freeProc;
{
 STRLEN len = (string) ? strlen(string) : 0;
 Lang_SetBinaryResult(interp, string, len, freeProc);
}

void
Tcl_CallWhenDeleted(interp, proc, clientData)
Tcl_Interp *interp;
Tcl_InterpDeleteProc *proc;
ClientData clientData;
{
 dTHX;
 HV *hv = InterpHv(interp,1);
 AV *av = FindAv(aTHX_ interp, "Tcl_CallWhenDeleted", 1, "_When_Deleted_");
 av_push(av, newSViv(PTR2IV(proc)));
 av_push(av, newSViv(PTR2IV(clientData)));
}

XS(XS_Tk__Interp_DESTROY)
{
 dXSARGS;
 Tcl_Interp *interp = (Tcl_Interp *) SvRV(ST(0));
#if 0
 fprintf(stderr,"InterpDestroy %ld\n",SvREFCNT((SV *) interp));
#endif
 /* Tk_CheckHash((SV *)interp,NULL); */
 hv_undef(interp);
}

static void
DeleteInterp(char *cd)
{
 Tcl_Interp *interp = (Tcl_Interp *) cd;
 dTHX;
 SV *exiting = FindSv(aTHX_ interp, "DeleteInterp", -1, "_TK_EXIT_");
 AV *av = FindAv(aTHX_ interp, "DeleteInterp", -1, "_When_Deleted_");
 HV *hv = FindHv(aTHX_ interp, "DeleteInterp", -1, ASSOC_KEY);
 if (av)
  {
   while (av_len(av) > 0)
    {
     SV *cd = av_pop(av);
     SV *pr = av_pop(av);
     Tcl_InterpDeleteProc *proc = INT2PTR(Tcl_InterpDeleteProc *, SvIV(pr));
     ClientData clientData = INT2PTR(ClientData, SvIV(cd));
     (*proc) (clientData, interp);
     SvREFCNT_dec(cd);
     SvREFCNT_dec(pr);
    }
   SvREFCNT_dec((SV *) av);
  }
 if (hv)
  {HE *he;
   /* Tk_CheckHash((SV *)hv,NULL); */
   hv_iterinit(hv);
   while ((he = hv_iternext(hv)))
    {
     STRLEN sz;
     SV *val = hv_iterval(hv,he);
     Assoc_t *info = (Assoc_t *) SvPV(val,sz);
     if (sz != sizeof(*info))
      croak("%s corrupted",ASSOC_KEY);
     if (info->proc)
      (*info->proc)(info->clientData, interp);
    }
   hv_undef(hv);
  }
 DecInterp(interp, "DeleteInterp");
 if (exiting)
  {
   sv_2mortal(exiting);
   my_exit(SvIV(exiting));
  }
}

int
Tcl_InterpDeleted(Tcl_Interp *interp)
{
 dTHX;
 SV *sv = FindSv(aTHX_ interp, "Tcl_InterpDeleted", 0, "_DELETED_");
 if (sv)
  {
   return SvTRUE(sv);
  }
 return 0;
}

void
Tcl_DeleteInterp(interp)
Tcl_Interp *interp;
{
 dTHX;
 SV *del = FindSv(aTHX_ interp, "Tcl_DeleteInterp", 1, "_DELETED_");
 sv_setiv(del,1);
 Tcl_EventuallyFree((ClientData) interp, DeleteInterp);
}

/*
 * We just deleted the last window in the application.  Delete
 * the TkMainInfo structure too and replace all of Tk's commands
 * with dummy commands that return errors (except don't replace
 * the "exit" command, since it may be needed for the application
 * to exit).
 */

void
Lang_DeadMainWindow(interp,tkwin)
Tcl_Interp *interp;
Tk_Window tkwin;
{
 dTHX;
 HV *hv    = InterpHv(interp,1);
 HV *fonts = FindHv(aTHX_ interp, "Lang_DeadMainWindow", 0, FONTS_KEY);
 Display *dpy = Tk_Display(tkwin);
 STRLEN na;
 if (dpy)
  XSync(dpy,FALSE);
 if (0 && fonts)
  {HE *he;
   hv_iterinit(fonts);
   while ((he = hv_iternext(fonts)))
    {
     SV *val = hv_iterval(fonts,he);
     Lang_CmdInfo *info = WindowCommand(val,NULL,0);
     if (info && info->tkfont)
      {
       Tk_FreeFont(info->tkfont);
       info->tkfont = NULL;
      }
    }
   /* Tk_CheckHash((SV *)fonts,NULL); */
   hv_undef(fonts);
  }
 sv_unmagic((SV *) hv, PERL_MAGIC_ext);
 Tcl_DeleteInterp(interp);
}

static SV *
struct_sv(ptr,sz)
void *ptr;
STRLEN sz;
{
 dTHX;
 SV *sv = (ptr) ? newSVpv((char *) ptr, sz) : newSV(sz);
 if (ptr)
  {
   SvREADONLY_on(sv);
  }
 else
  {
   Zero(SvPVX(sv),sz+1,char);
   SvCUR_set(sv,sz);
   SvPOK_only(sv);
  }
 return sv;
}

static int
TkGlue_mgFree(pTHX_ SV *sv, MAGIC *mg)
{
 STRLEN na;
 return 0;
}

MGVTBL TkGlue_vtab = {
 NULL,
 NULL,
 NULL,
 NULL,
 TkGlue_mgFree
};

static SV *
tilde_magic(hv,sv)
SV *hv;
SV *sv;
{
 dTHX;
 MAGIC *mg;
 sv_magic(hv, sv, PERL_MAGIC_ext, NULL, 0);
 SvRMAGICAL_off(hv);
 mg = mg_find(hv, PERL_MAGIC_ext);
 if (mg->mg_obj != sv)
  abort();
 mg->mg_virtual = &TkGlue_vtab;
 mg_magical(hv);
 return sv;
}

#define mSVPV(sv,na) (SvOK(sv) ? SvPV(sv,na) : "undef")

void
LangDumpVec(CONST char *who, int count, SV **data)
{
 dTHX;
 int i;
 PerlIO_printf(PerlIO_stderr(), "%s (%d):\n", who, count);
 for (i = 0; i < count; i++)
  {
   SV *sv = data[i];
   if (sv)
    {
     PerlIO_printf(PerlIO_stderr(), "%2d ", i);
     LangPrint(sv);
     sv_dump(sv);
    }
  }
 if (SvTRUE(get_sv("Tk::_AbortOnLangDump",0)))
  {
   abort();
  }
}

void
DumpStack(CONST char *who)
{
 dTHX;
 do_watch();
 LangDumpVec(who, PL_stack_sp - PL_stack_base, PL_stack_base + 1);
}

void
LangSetString(sp, s)
SV **sp;
CONST char *s;
{
 dTHX;
 SV *sv = *sp;
 do_watch();
 if (!s)
  {
   /* tkOldConfig uses LangSetString when TK_CONFIG_NULL_OK is _NOT_ set
      we must set something.
    */
   s = "";
  }
 if (sv)
  {
   sv_setpv(sv, s);
   SvSETMAGIC(sv_maybe_utf8(sv));
   return;
  }
 *sp = Tcl_NewStringObj(s, -1);
}

void
LangSetDefault(sp, s)
SV **sp;
CONST char *s;
{
 dTHX;
 SV *sv = *sp;
 do_watch();
 if (sv)
  {
   if (!s || !*s || SvREADONLY(sv))
    {
     Decrement(sv, "LangSetDefault");
    }
   else
    {
     if (s && *s)
      {
       sv_setpv(sv, s);
       SvSETMAGIC(sv);
       return;
      }
    }
  }
 *sp = sv = (s && *s) ? TagIt(newSVpv(s, strlen(s)),"LangSetDefault") : &PL_sv_undef;
}

void
LangSetObj(sp, arg)
SV **sp;
SV *arg;
{
 dTHX;
 SV *sv = *sp;
 do_watch();
 if (!arg)
  arg = &PL_sv_undef;
 if (SvTYPE(arg) == SVt_PVAV)
  arg = newRV_noinc(arg);
 if (sv && SvMAGICAL(sv))
  {
   SvSetMagicSV(sv, arg);
   SvREFCNT_dec(arg);
  }
 else
  {
   *sp = arg;
   if (sv)
    SvREFCNT_dec(sv);
  }
}

static void
Deprecated(char *what, char *file, int line)
{
 LangDebug("%s:%d: %s is deprecated\n",file,line,what);
}

void
LangOldSetArg(sp, arg, file, line)
SV **sp;
SV *arg;
char *file;
int line;
{
 dTHX;
 Deprecated("LangSetArg",file,line);
 LangSetObj(sp,(arg) ? SvREFCNT_inc(arg) : arg);
}

/* This replaces LangSetArg(sp,LangVarArg(var)) which leaked RVs */
void
LangSetVar(sp,sv)
SV **sp;
Var sv;
{
 dTHX;
 if (sv)
  {
   SV *rv = newRV(sv);
   LangSetObj(sp,rv);
  }
 else
  LangSetObj(sp,NULL);
}

void
LangSetInt(sp, v)
SV **sp;
int v;
{
 dTHX;
 SV *sv = *sp;
 do_watch();
 if (sv && sv != &PL_sv_undef)
  {
   sv_setiv(sv, v);
   SvSETMAGIC(sv);
  }
 else
  *sp = sv = newSViv(v);
}

void
LangSetDouble(sp, v)
SV **sp;
double v;
{
 dTHX;
 SV *sv = *sp;
 do_watch();
 if (sv && sv != &PL_sv_undef)
  {
   sv_setnv(sv, v);
   SvSETMAGIC(sv);
  }
 else
  *sp = sv = newSVnv(v);
}

static void
die_with_trace(SV *sv,char *msg)
{
 dTHX;
 dSP;
 if (!sv)
  {
   sv = newSVpv("Tk",2);
  }
 ENTER;
 SAVETMPS;
 PUSHMARK(sp);
 XPUSHs(sv);
 XPUSHs(sv_2mortal(newSVpv(msg,0)));
 PUTBACK;
 perl_call_method("die_with_trace",G_VOID);
 FREETMPS;
 LEAVE;
}

Lang_CmdInfo *
WindowCommand(sv, hv_ptr, need)
SV *sv;
HV **hv_ptr;
int need;
{
 dTHX;
 STRLEN na;
 char *msg = "not a Tk object";
 if (SvROK(sv))
  {
   HV *hash = (HV *) SvRV(sv);
   MAGIC *mg = mg_find((SV *) hash,PERL_MAGIC_ext);
   if (hv_ptr)
    *hv_ptr = hash;
   if (mg)
    {
     Lang_CmdInfo *info = (Lang_CmdInfo *) SvPV(mg->mg_obj,na);
     if (info)
      {
       if ((need & 1) && !info->interp)
        croak("%s is not a Tk object",SvPV(sv,na));
       if ((need & 2) && !info->tkwin)
        croak("WindowCommand:%s is not a Tk Window",SvPV(sv,na));
       if ((need & 4) && !info->image)
        croak("%s is not a Tk Image",SvPV(sv,na));
       if ((need & 8) && !info->tkfont)
        croak("%s is not a Tk Font",SvPV(sv,na));
       return info;
      }
    }
  }
 else
  msg = "not a reference";
 if (need)  /* Cannot always do this - after() does this a lot ! */
  {
   die_with_trace(sv,msg);
  }
 return NULL;
}



Tk_Window
SVtoWindow(sv)
SV *sv;
{
 Lang_CmdInfo *info = WindowCommand(sv, NULL, 2);
 if (info && info->tkwin)
  return info->tkwin;
 return NULL;
}

HWND
SVtoHWND(sv)
SV *sv;
{
 Tk_Window tkwin = SVtoWindow(sv);
 if (tkwin)
  {
#ifdef WIN32
   Tk_MakeWindowExist(tkwin);
   return Tk_GetHWND(Tk_WindowId(tkwin));
#endif
  }
 return NULL;
}

void
#ifdef STANDARD_C
Tcl_SprintfResult(Tcl_Interp * interp, char *fmt,...)
#else
Tcl_SprintfResult(interp, fmt, va_alist)
Tcl_Interp *interp;
char *fmt;
va_dcl
#endif
{
 dTHX;
 SV *sv = newSVpv("",0);
 va_list ap;
#ifdef I_STDARG
 va_start(ap, fmt);
#else
 va_start(ap);
#endif
 sv_vsetpvfn(sv, fmt, strlen(fmt), &ap, Null(SV**), 0, NULL);
 Tcl_SetObjResult(interp, sv);
 va_end(ap);
}

#ifdef STANDARD_C
void
Tcl_IntResults
_ANSI_ARGS_((Tcl_Interp * interp, int count, int append,...))
#else
/*VARARGS0 */
void
Tcl_IntResults(interp, count, append, va_alist)
Tcl_Interp *interp;
int count;
int append;
va_dcl
#endif
{
 dTHX;
 va_list ap;
 Tcl_Obj *result;
#ifdef I_STDARG
 va_start(ap, append);
#else
 va_start(ap);
#endif
 if (!append)
  {
   Tcl_ResetResult(interp);
  }
 result = Tcl_GetObjResult(interp);
 if (count == 1 && !append)
  abort();
 while (count--)
  {
   int value = va_arg(ap, int);
   Tcl_Obj *vObj = Tcl_NewIntObj(value);
   Tcl_ListObjAppendElement(interp,result,vObj);
  }
 va_end(ap);
}

#ifdef STANDARD_C
void
Tcl_DoubleResults
_ANSI_ARGS_((Tcl_Interp * interp, int count, int append,...))
#else
void
Tcl_DoubleResults(interp, count, append, va_alist)
Tcl_Interp *interp;
int count;
int append;
va_dcl
#endif
{
 dTHX;
 va_list ap;
 Tcl_Obj *result;
#ifdef I_STDARG
 va_start(ap, append);
#else
 va_start(ap);
#endif
 if (!append)
  Tcl_ResetResult(interp);
 result = Tcl_GetObjResult(interp);
 if (!count)
  {
   LangDebug("%s - No Results\n",__FUNCTION__);
   abort();
   Tcl_Panic("No results");
  }
 while (count--)
  {
   double value = va_arg(ap, double);
   Tcl_ListObjAppendElement(interp,result,Tcl_NewDoubleObj(value));
  }
 va_end(ap);
}


#ifdef STANDARD_C
void
Tcl_AppendResult
_ANSI_ARGS_((Tcl_Interp * interp,...))
#else
void
Tcl_AppendResult(interp, va_alist)
Tcl_Interp *interp;
va_dcl
#endif
{
 SV *result = Tcl_GetObjResult(interp);
 va_list ap;
 char *s;
#ifdef I_STDARG
 va_start(ap, interp);
#else
 va_start(ap);
#endif
 while ((s = va_arg(ap, char *)))
  {
   Tcl_AppendStringsToObj(result,s, NULL);
  }
 va_end(ap);
}

SV *
ObjectRef(interp, path)
Tcl_Interp *interp;
char *path;
{
 dTHX;
 if (path)
  {
   HV *hv = InterpHv(interp,1);
   SV **x = hv_fetch(hv, path, strlen(path), 0);
   if (x)
    return *x;
  }
 return &PL_sv_undef;
}

SV *
WidgetRef(interp, path)
Tcl_Interp *interp;
char *path;
{
 dTHX;
 HV *hv = InterpHv(interp,1);
 SV **x = hv_fetch(hv, path, strlen(path), 0);
 if (x)
  {
   SV *w = *x;
   if (SvROK(w) && SvTYPE(SvRV(w)) == SVt_PVHV)
    return w;
   LangDumpVec(path,1,&w);
   abort();
  }
 return &PL_sv_undef;
}

SV *
TkToWidget(tkwin,pinterp)
Tk_Window tkwin;
Tcl_Interp **pinterp;
{
 dTHX;
 Tcl_Interp *junk;
 if (!pinterp)
  pinterp = &junk;
 *pinterp = NULL;
 if (tkwin)
  {
   TkWindow *winPtr = (TkWindow *) tkwin;
   TkMainInfo *mainInfo = winPtr->mainPtr;
   if (mainInfo)
    {
     Tcl_Interp *interp = mainInfo->interp;
     if (interp)
      {
       *pinterp = interp;
       if (Tk_PathName(tkwin))
         return WidgetRef(interp, Tk_PathName(tkwin));
      }
    }
  }
 return &PL_sv_undef;
}


Tk_Window
TkToMainWindow(tkwin)
Tk_Window tkwin;
{
 if (tkwin)
  {
   TkWindow *winPtr = (TkWindow *) tkwin;
   TkMainInfo *mainInfo = winPtr->mainPtr;
   if (mainInfo)
    {
     return (Tk_Window) mainInfo->winPtr;
    }
  }
 return NULL;
}

Tcl_Obj *
LangWidgetObj(interp, tkwin)
Tcl_Interp *interp;
Tk_Window tkwin;
{
 dTHX;
 return SvREFCNT_inc(TkToWidget(tkwin,NULL));
}

Tcl_Obj *
LangObjectObj(interp, name)
Tcl_Interp *interp;
char *name;
{
 dTHX;
 return SvREFCNT_inc(ObjectRef(interp, name));
}

Tk_Font
SVtoFont(SV *sv)
{
 dTHX;
 if (sv_isobject(sv) && SvPOK(SvRV(sv)))
  {
   Lang_CmdInfo *info = WindowCommand(sv, (HV **) &sv, 0);
   if (info)
    {
     if (!info->tkfont && info->interp)
      {
       Tk_Window tkwin = Tk_MainWindow(info->interp);
       if (tkwin)
        info->tkfont = Tk_GetFontFromObj(tkwin, sv);
      }
     if (info->tkfont)
      {
       STRLEN len;
       CONST char *s = Tk_NameOfFont(info->tkfont);
       if (strcmp(s,SvPV(sv,len)) != 0)
        {
         croak("Font %p name '%s' string '%s'",info->tkfont,s,SvPV(sv,len));
        }
      }
     return info->tkfont;
    }
  }
 return NULL;
}

Tcl_Obj *
LangFontObj(interp, tkfont, name)
Tcl_Interp *interp;
Tk_Font tkfont;
char *name;
{
 dTHX;
 HV *fonts = FindHv(aTHX_ interp, "LangFontArg", 1, FONTS_KEY);
 STRLEN na;
 SV *sv;
 SV **x;
 if (!name)
  name = (char *) Tk_NameOfFont(tkfont);
 x = hv_fetch(fonts, name, strlen(name), 0);
 if (x)
  {
   sv = *x;
  }
 else
  {
   Tk_Window tkwin = Tk_MainWindow(interp);
   Lang_CmdInfo info;
   SV *isv;
   sv = newSVpv(name,0);
   memset(&info,0,sizeof(info));
   info.interp = interp;
   info.tkfont = tkfont;
   IncInterp(interp,name);
   isv = struct_sv(&info,sizeof(info));
   tilde_magic(sv, isv);
   sv = Blessed("Tk::Font", MakeReference(sv));
   hv_store(fonts, name, strlen(name), sv, 0);
  }
 return SvREFCNT_inc(sv);
}

void
Font_DESTROY(SV *arg)
{
 dTHX;
 STRLEN na;
 SV *sv;
 Lang_CmdInfo *info = WindowCommand(arg,(HV **) &sv,0);
 if (info)
  {
   if (info->interp)
    DecInterp(info->interp,SvPV(sv,na));
   sv_unmagic(sv,PERL_MAGIC_ext);
  }
}

static AV *
CopyAv(dst, src)
AV *dst;
AV *src;
{
 dTHX;
 int n = av_len(src) + 1;
 int i;
 av_clear(dst);
 for (i = 0; i < n; i++)
  {
   SV **x = av_fetch(src, i, 0);
   if (x)
    {
     Increment(*x, "CopyAv");
     av_store(dst, i, *x);
    }
  }
 return dst;
}

static void
Lang_ClearErrorInfo(interp)
Tcl_Interp *interp;
{
 dTHX;
 AV *av = FindAv(aTHX_ interp, "Lang_ClearErrorInfo", -1, "_ErrorInfo_");
 if (av)
  {
   SvREFCNT_dec((SV *) av);
  }
}

void
Tcl_AddErrorInfo(interp, message)
Tcl_Interp *interp;
CONST char *message;
{
 dTHX;
 if (InterpHv(interp,0))
  {
   AV *av = FindAv(aTHX_ interp, "Tcl_AddErrorInfo", 1, "_ErrorInfo_");
   SV *sv;
   while (isspace(UCHAR(*message)))
    message++;
   if (*message)
    av_push(av,newSVpv((char *)message,0));
  }
}

static int
Check_Eval(interp)
Tcl_Interp *interp;
{
 dTHX;
 SV *sv = ERRSV;
 if (FindSv(aTHX_ interp, "Check_Eval", 0, "_TK_EXIT_"))
  return TCL_BREAK;
 if (SvTRUE(sv))
  {
   STRLEN len;
   char *s = SvPV(sv, len);
   if (!strncmp("_TK_EXIT_(",s,10))
    {
     Tk_Window tkwin = Tk_MainWindow(interp);
     SV *sv = FindSv(aTHX_ interp, "Check_Eval", 1, "_TK_EXIT_");
     char *e = strchr(s+=10,')');
     sv_setpvn(sv,s,e-s);
     if (tkwin)
      Tk_DestroyWindow(tkwin);
     return TCL_BREAK;
    }
   else if (!strcmp("_TK_BREAK_\n",s))
    {
     sv_setpv(sv,"");
     return TCL_BREAK;
    }
   else
    {
     SV *save = sv_2mortal(newSVsv(sv));
     s = SvPV(save, len);
#if 0
     LangDebug("%s error:%.*s\n",__FUNCTION__,na,s);
#endif
     if (!interp)
      croak("%s",s);
     Tcl_SetResult(interp, s, TCL_VOLATILE);
     sv_setpv(sv,"");
     return TCL_ERROR;
    }
  }
 return TCL_OK;
}

static void
Restore_widget(pTHX_ void *arg)
{
 SV *widget = (SV *) arg;
 SV * sv = GvSV(current_widget);
 SvSetMagicSV(sv,widget);
 SvREFCNT_dec(widget);
#if 0
 LangDumpVec("Restore Tk::widget",1,&sv);
#endif
}

static void
Set_widget(widget)
SV *widget;
{
 dTHX;
 if (!current_widget)
  current_widget = gv_fetchpv("Tk::widget",GV_ADD|GV_ADDWARN, SVt_PV);
 if (widget && SvROK(widget))
  {
   SV * sv = GvSV(current_widget);
   /* We used to use save_item() here but that and other
      generic perl save routines make assumptions about REFCNT
      and magic which we don't obey.
      Our REFCNT may be high, and both old an new SVs may have
      '~' magic for Tcl_Obj internal stuff.
    */
#if 0
   LangDumpVec("save Tk::widget",1,&sv);
#endif
   save_destructor_x(Restore_widget,LangCopyArg(sv));
   SvSetMagicSV(sv,widget);
  }
}

static void
Set_event(SV *event)
{
 dTHX;
 if (!current_event)
  current_event = gv_fetchpv("Tk::event",GV_ADD|GV_ADDWARN, SVt_PV);
 if (event && SvROK(event))
  {
   SV * sv = GvSV(current_event);
   save_item(sv);
   SvSetMagicSV(sv,event);
  }
}

static int
PushObjCallbackArgs(interp, svp ,obj)
Tcl_Interp *interp;
SV **svp;
EventAndKeySym *obj;
{
 SV *sv = *svp;
 dTHX;
 dSP;
 STRLEN na;
 if (SvTAINTED(sv))
  {
   croak("Tainted callback %"SVf,sv);
  }
 if (1 && interp && !sv_isa(sv,"Tk::Callback") && !sv_isa(sv,"Tk::Ev"))
  {
   return EXPIRE((interp,"Not a Callback '%s'",SvPV(sv,na)));
  }
 else
  {
   if (SvROK(sv) && SvTYPE(SvRV(sv)) != SVt_PVCV)
    sv = SvRV(sv);
  }

 PUSHMARK(sp);
 if (SvTYPE(sv) == SVt_PVAV)
  {
   AV *av = (AV *) sv;
   int n = av_len(av) + 1;
   SV **x = av_fetch(av, 0, 0);
   if (n && x)
    {
     int i = 1;
     sv = *x;
     if (SvTAINTED(sv))
      {
       croak("Callback slot 0 tainted %"SVf,sv);
      }
     /* FIXME:
        POE would like window passed to its callback objects
        Pending suggestion is:
         if ($object->can('_Tk_passWidget') &&
             $object->_Tk_passWidget($widget)
          {
           proceed_as_non_object();
          }
      */
     if (!sv_isobject(sv))
      {
       if (obj && obj->window) {
         XPUSHs(sv_mortalcopy(obj->window));
       }
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
         if (obj && sv_isa(arg,"Tk::Ev"))
          {
           SV *what = SvRV(arg);
           if (SvPOK(what))
            {STRLEN len;
             char *s = SvPV(what,len);
             if (len == 1)
              {
 	       PUTBACK;
               arg = XEvent_Info(obj, s);
	       SPAGAIN;
              }
             else
              {char *x;
               arg = sv_newmortal();
               sv_setpv(arg,"");
               while ((x = strchr(s,'%')))
                {
                 if (x > s)
                  sv_catpvn(arg,s,(unsigned) (x-s));
                 if (*++x)
                  {SV *f = XEvent_Info(obj, x++);
                   STRLEN len;
                   char *p = SvPV(f,len);
                   sv_catpvn(arg,p,len);
                  }
                 s = x;
                }
               sv_catpv(arg,s);
              }
            }
           else
            {
             switch(SvTYPE(what))
              {
               case SVt_NULL:
                arg = &PL_sv_undef;
                break;
               case SVt_PVAV:
                {
                 int code;
                 PUTBACK;
                 if ((code = PushObjCallbackArgs(interp,&arg,obj)) == TCL_OK)
                  {
                   int count = LangCallCallback(arg,G_ARRAY|G_EVAL);
                   if ((code = Check_Eval(interp)) != TCL_OK)
                    return code;
                   SPAGAIN;
                   arg = NULL;
                   break;
                  }
                 else
                  return code;
                }
               default:
                LangDumpVec("Ev",1,&arg);
                LangDumpVec("  ",1,&what);
                warn("Unexpected type %d %s",SvTYPE(what),SvPV(arg,na));
                arg = sv_mortalcopy(arg);
                break;
              }
            }
           if (arg) {
            XPUSHs(arg);
	   }
          }
         else
          XPUSHs(sv_mortalcopy(arg));
        }
       else
        XPUSHs(&PL_sv_undef);
      }
    }
   else
    {
     if (interp)
      {
       return EXPIRE((interp,"No 0th element of %s", SvPV(sv, na)));
      }
     else
      sv = &PL_sv_undef;
    }
  }
 else
  {
   if (obj && obj->window)
    XPUSHs(sv_mortalcopy(obj->window));
  }
 *svp = sv;
 PUTBACK;
 return TCL_OK;
}

static int
PushCallbackArgs(interp, svp)
Tcl_Interp *interp;
SV **svp;
{
 SV *sv = *svp;
 dTHX;
 dSP;
 if (0 && interp && !sv_isa(sv,"Tk::Callback") && !sv_isa(sv,"Tk::Ev"))
  {
   return EXPIRE((interp,"Not a Callback '%s'",SvPV_nolen(sv)));
  }
 LangPushCallbackArgs(svp);
 if (interp && (sv = *svp) == &PL_sv_undef)
  {
   return EXPIRE((interp,"No 0th element of %s", SvPV_nolen(sv)));
  }
 return TCL_OK;
}

static void
SetTclResult(interp,count)
Tcl_Interp *interp;
int count;
{
 dTHX;
 dSP;
 int offset = count;
 Tcl_ResetResult(interp);
 if (count)
  {
   Tcl_Obj *result = Tcl_GetObjResult(interp);
   SV **p = sp - count;
   if (count > 1)
    {
     while (count-- > 0)
      {
       Tcl_ListObjAppendElement(interp, result, newSVsv(*++p));
      }
    }
   else
    {
     SvSetMagicSV(result,p[1]);
    }
  }
 sp -= offset;
 PUTBACK;
}

static void
PushVarArgs(ap,argc)
va_list ap;
int argc;
{
 dTHX;
 dSP;
 int i;
 char *fmt = va_arg(ap, char *);
 char *s = fmt;
 unsigned char ch = '\0';
 int lng = 0;
 for (i = 0; i < argc; i++)
  {
   s = strchr(s, '%');
   if (s)
    {
     ch  = UCHAR(*++s);
     lng = 0;
     while (isdigit(ch) || ch == '.' || ch == '-' || ch == '+')
      ch = *++s;
     if (ch == 'l')
      {
       lng = 1;
       ch = *++s;
      }
     switch (ch)
      {
       case 'u':
       case 'i':
       case 'd':
        {IV val = (lng) ? va_arg(ap, long) : va_arg(ap, int);
         XPUSHs(sv_2mortal(newSViv(val)));
        }
        break;
       case 'g':
       case 'e':
       case 'f':
        XPUSHs(sv_2mortal(newSVnv(va_arg(ap, double))));
        break;
       case 's':
        {
         char *x = va_arg(ap, char *);
         if (x)
          XPUSHs(sv_2mortal(Tcl_NewStringObj(x, -1)));  /* for UTF-8-ness */
         else
          XPUSHs(&PL_sv_undef);
        }
        break;
       case '_':
        {
         SV *x = va_arg(ap, SV *);
         if (x)
          XPUSHs(sv_mortalcopy(x));
         else
          XPUSHs(&PL_sv_undef);
        }
        break;
       case 'L':
        {
         Tcl_Obj *x = va_arg(ap, Tcl_Obj *);
         Tcl_Obj **argv;
         int argc;
         if (Tcl_ListObjGetElements(NULL,x,&argc,&argv) == TCL_OK)
           {
            int i;
            for (i=0; i < argc; i++)
             {
	      XPUSHs(sv_mortalcopy((SV *) (argv[i])));
             }
           }
        }
        break;
       default:
        croak("Unimplemented format char '%c' in '%s'", ch, fmt);
        break;
      }
    }
   else
    croak("Not enough %%s (need %d) in '%s'", argc, fmt);
  }
 if (strchr(s,'%'))
  {
   croak("Too many %%s (need %d) in '%s'", argc, fmt);
  }
 PUTBACK;
}



#ifdef STANDARD_C
int
LangDoCallback
_ANSI_ARGS_((Tcl_Interp * interp, LangCallback * sv, int result, int argc,...))
#else
int
LangDoCallback(interp, sv, result, argc, va_alist)
Tcl_Interp *interp;
SV *sv;
int result;
int argc;
va_dcl
#endif
{
 STRLEN na;
 static int flags[3] = { G_DISCARD, G_SCALAR, G_ARRAY };
 int count = 0;
 int code;
 SV *cb    = sv;
 dTHX;
 ENTER;
 SAVETMPS;
 if (interp)
  {
   Tcl_ResetResult(interp);
   Lang_ClearErrorInfo(interp);
  }
 code = PushCallbackArgs(interp,&sv);
 if (code != TCL_OK)
  return code;
 if (argc)
  {
   va_list ap;
#ifdef I_STDARG
   va_start(ap, argc);
#else
   va_start(ap);
#endif
   PushVarArgs(ap,argc);
   va_end(ap);
  }
 count = LangCallCallback(sv, flags[result] | G_EVAL);
 if (interp && result)
  SetTclResult(interp,count);
 FREETMPS;
 LEAVE;
 count = Check_Eval(interp);
 if (count == TCL_ERROR && interp)
  {
   SV *tmp = newSVpv("", 0);
   LangCatArg(tmp,cb,0);
   Tcl_AddErrorInfo(interp,SvPV(tmp,na));
   SvREFCNT_dec(tmp);
  }
 return count;
}

static
void HandleBgErrors(clientData)
ClientData clientData;
{
 dTHX;
 Tcl_Interp *interp = (Tcl_Interp *) clientData;
 AV *pend   = FindAv(aTHX_ interp, "HandleBgErrors", 0, "_PendingErrors_");
 ENTER;
 SAVETMPS;
 TAINT_NOT;
 if (pend)
  {
   Set_widget( WidgetRef(interp,"."));
   while (av_len(pend) >= 0)
    {
     SV *sv = av_shift(pend);
     if (sv && SvOK(sv))
      {
       int result = PushCallbackArgs(interp,&sv);
       if (result == TCL_OK)
        {
         LangCallCallback(sv, G_DISCARD | G_EVAL);
         result = Check_Eval(interp);
        }
       if (result == TCL_BREAK)
        break;
       else if (result == TCL_ERROR)
        {
         warn("Background Error: %s",Tcl_GetStringResult(interp));
        }
      }
    }
   av_clear(pend);
  }
 FREETMPS;
 LEAVE;
 Tcl_ResetResult(interp);
 DecInterp(interp,"HandleBgErrors");
}

void
Tcl_BackgroundError(interp)
Tcl_Interp *interp;
{
 dTHX;
 int old_taint = PL_tainted;
 TAINT_NOT;
#if 0
 warn(__FUNCTION__);
#endif
 if (InterpHv(interp,0))
  {
   AV *pend = FindAv(aTHX_ interp, "Tcl_BackgroundError", 1, "_PendingErrors_");
   AV *av   = FindAv(aTHX_ interp, "Tcl_BackgroundError", -1, "_ErrorInfo_");
   SV *obj  = WidgetRef(interp,".");
   if (obj && SvROK(obj))
    obj = SvREFCNT_inc(obj);
   else
    obj = newSVpv(BASEEXT,0);
   if (!av)
    {
     av = newAV();
     TagIt((SV *) av, "Tcl_BackgroundError");
    }
   av_unshift(av,3);
   av_store(av, 0, newSVpv("Tk::Error",0));
   av_store(av, 1, obj);
   av_store(av, 2, newSVpv(Tcl_GetStringResult(interp),0));
   av_push( pend, LangMakeCallback(MakeReference((SV *) av)));
   if (av_len(pend) <= 0)
    {
     /* 1st one - setup callback */
     IncInterp(interp,"Tk_BackgroundError");
     Tcl_DoWhenIdle(HandleBgErrors, (ClientData) interp);
    }
   Tcl_ResetResult(interp);
  }
 TAINT_IF(old_taint);
}

static void
Lang_MaybeError(interp,code,why)
Tcl_Interp *interp;
int code;
char *why;
{
 if (code != TCL_OK)
  {
   Tcl_AddErrorInfo(interp,why);
   Tcl_BackgroundError(interp);
  }
 else
  Lang_ClearErrorInfo(interp);
}

void
ClearErrorInfo(win)
SV *win;
{Lang_CmdInfo *info = WindowCommand(win,NULL,1);
 Lang_ClearErrorInfo(info->interp);
}


static int
Return_Object(int items, int offset, Tcl_Obj *sv)
{
 dTHX;
 int gimme = GIMME_V;
 int count = 0;
 int i;
 SV **objv = NULL;
 SV **args = NULL;
 /* Get stack as it is now */
 dSP;
 switch(gimme)
  {
   case G_VOID :
    count = 0;
    objv  = NULL;
    break;
   case G_ARRAY:
    if (!SvOK(sv))
     {
      count = 0;
      break;
     }
    else if (SvROK(sv) && SvTYPE(SvRV(sv)) == SVt_PVAV &&
        !SvMAGICAL(sv) && !sv_isobject(sv))
     {
      Tcl_ListObjGetElements(NULL,sv,&count,&objv);
      break;
     }
    else
     {
      /* warn("Special obj in list context"); */
     }
   default:
    count = 1;
    objv  = &sv;
#if 0
    /* Breaks Canvas group members return */
    if (SvROK(sv) && SvTYPE(SvRV(sv)) == SVt_PVAV && !sv_isobject(sv))
     {
      if (av_len((AV *)SvRV(sv)) == 0)
       {
        die_with_trace(NULL,"One element array in scalar context");
        objv = av_fetch((AV *)SvRV(sv),0,0);
       }
     }
#endif
    break;
  }
 SPAGAIN;
 if (count > items)
  {
   EXTEND(sp, (count - items));
  }
 /* Now move 'args' to 0'th arg position in current stack */
 args = sp + offset;
 for (i = count-1; i >= 0; i--)
  {
   args[i] = sv_mortalcopy(objv[i]);
  }
 /* Copy stack pointer back to global */
 PUTBACK;
 return count;
}

static int
Return_Results(Tcl_Interp *interp,int items, int offset)
{
 Tcl_Obj *result = Tcl_GetObjResult(interp);
 int count = Return_Object(items,offset,result);
 Tcl_ResetResult(interp);
 return count;
}

static void
Lang_TaintCheck(char *s, int items, SV **args)
{
 dTHX;
 if (PL_tainting)
  {
   int i;
   for (i=0; i < items; i++)
    {
     if (SvTAINTED(args[i]))
      croak("Tcl_Obj * %d to `%s' (%"SVf") is tainted",i,s,args[i]);
    }
  }
}

struct pTkCheckChain
{
 struct pTkCheckChain *link;
 SV *sv;
};

void
Tk_CheckHash(SV *sv,struct pTkCheckChain *tail)
{
 dTHX;
 struct pTkCheckChain chain;
 HE *he;
 HV *hv;
 SV **svp;
 if (SvROK(sv))
  sv = SvRV(sv);
 chain.link = tail;
 chain.sv   = sv;
 if (SvTYPE(sv) != SVt_PVHV)
  return;
 hv = (HV *) sv;
 hv_iterinit(hv);
 while ((he = hv_iternext(hv)))
  {
   SV *val = hv_iterval(hv,he);
   if (val)
    {
     if (SvREFCNT(val) <= 0)
      {I32 len;
       char *key = hv_iterkey(he,&len);
       LangDebug("%.*s has 0 REFCNT\n",(int) len, key);
       sv_dump((SV *)hv);
       abort();
      }
     else
      {
       if (SvROK(val))
        val = SvRV(val);
       if (SvTYPE((SV *) val) == SVt_PVHV /*  && SvOBJECT(val) */)
        {
         struct pTkCheckChain *p = &chain;
         I32 len;
         while (p)
          {
           if (p->sv == val)
            {I32 len;
             char *key = hv_iterkey(he,&len);
             LangDebug("Check Loop %.*s %p - %p\n",(int) len, key, hv, val);
             goto skip;
            }
           p = p->link;
          }
         /* LangDebug("Check %p{%s}\n",hv,hv_iterkey(he,&len)); */
         Tk_CheckHash(val,&chain);
         skip:
          /* do nothing */;
        }
      }
    }
  }
}

int
Call_Tk(info, items, args)
Lang_CmdInfo *info;
int items;
SV **args;
{
 int count = 1;
 STRLEN na;
 if (info)
  {
   dTHX;
   dSP;
   SV *what = SvREFCNT_inc(args[0]);
   SV *exiting;
   Tcl_Interp *interp = info->interp;
   int old_taint = PL_tainted;
   IncInterp(interp,"Call_Tk");
   PL_tainted = 0;
   do_watch();

   Tcl_ResetResult(interp);
   if (info->Tk.proc || info->Tk.objProc)
    {
     int i;
     /* Must find offset of 0'th arg now in case
        stack moves as a result of the call
      */
     int offset = args - sp;
     int code;
     SV **our_sp = sp;

     Tcl_ObjCmdProc *proc = info->Tk.objProc;
     ClientData cd = info->Tk.objClientData;

     if (!proc)
      {
       proc = (Tcl_ObjCmdProc *) (info->Tk.proc);
       cd   = info->Tk.clientData;
      }
     if (PL_tainting)
      {
       Lang_TaintCheck(Tcl_GetString(args[0]),items, args);
      }
     for (i=0; i < items; i++)
      {
       if (SvPOK(args[i]))
        Tcl_GetString(args[i]);
      }

     Tcl_Preserve(interp);

     /* BEWARE if Tk code does a callback to perl and perl grows the
        stack then args that Tk code has will still point at old stack.
        Thus if Tk tests args[i] *after* the callback it will get junk.
        (Note it is only vector that is at risk, SVs themselves will stay put.)

        So we pre-emptively swap perl stack so any callbacks
        which grow their stack don't move our "args"
      */
     ENTER;
     SAVETMPS;
     SPAGAIN;
     PUSHSTACK;
     PUTBACK;

     code = (*proc) (cd, interp, items, args);

     POPSTACK;
     SPAGAIN;
     FREETMPS;
     LEAVE;

     if (sp != our_sp)
      abort();

     Tcl_Release(interp);

     /* info stucture may have been free'ed now ... */
#ifdef WIN32
     if (DCcount)
       {
        warn("DCcount %ld for %s",DCcount, Tcl_GetString(what));
        // LangDumpVec("DCcount",items,args);
        DCcount = 0;
       }
#endif
     if ((exiting = FindSv(aTHX_ interp, "Check_Eval", 0, "_TK_EXIT_")))
      {
       PL_tainted = old_taint;
       DecInterp(interp, "Call_Tk");
       SvREFCNT_dec(what);
       TclpExit(SvIV(exiting));
      }
     else if (code == TCL_OK)
      {
       count = Return_Results(interp,items,offset);
      }
     else if (code == TCL_BREAK)
      {
       PL_tainted = old_taint;
       DecInterp(interp, "Call_Tk");
       SvREFCNT_dec(what);
       croak("_TK_BREAK_\n");
      }
     else
      {
       SV *msg = sv_newmortal();
       sv_setpv(msg,"Tk callback for ");
       sv_catpv(msg,Tcl_GetString(what));
       Tcl_AddErrorInfo(interp, SvPV(msg,na));
       sv_setpv(msg,Tcl_GetStringResult(interp));

       PL_tainted = old_taint;
       DecInterp(interp, "Call_Tk");
       SvREFCNT_dec(what);
       croak("%s",SvPV(msg,na));
      }
    }
   else
    {
     /* call after DeleteWidget */
     if (info->tkwin)
      croak("%s has been deleted",Tk_PathName(info->tkwin));
    }
   PL_tainted = old_taint;
   DecInterp(interp, "Call_Tk");
   SvREFCNT_dec(what);
  }
 else
  {
   /* Could be an "after" when mainwindow has been destroyed */
  }
 do_watch();
 return count;
}

static void
InitVtabs(void)
{
 dTHX;
 /* Called by Boot_Glue below, re-called in 5.004_50+ at start of run phase.
  * If we have been "Compiled" then module this code is defined in
  * will have been re-linked, so the 'static' above will be 0 again
  * which will cause us to re-set vtables with addresses where
  * we happen to be loaded now, as opposed to where we were loaded
  * at compile time.
  */
 if (!initialized)
  {
   IMPORT_EVENT;
   install_vtab("LangVtab",LangVGet(),sizeof(LangVtab));
   install_vtab("TcldeclsVtab",TcldeclsVGet(),sizeof(TcldeclsVtab));
   install_vtab("TkVtab",TkVGet(),sizeof(TkVtab));
   install_vtab("TkdeclsVtab",TkdeclsVGet(),sizeof(TkdeclsVtab));
   install_vtab("TkglueVtab",TkglueVGet(),sizeof(TkglueVtab));
   install_vtab("TkintVtab",TkintVGet(),sizeof(TkintVtab));
   install_vtab("TkintdeclsVtab",TkintdeclsVGet(),sizeof(TkintdeclsVtab));
   install_vtab("TkoptionVtab",TkoptionVGet(),sizeof(TkoptionVtab));
   install_vtab("TkimgphotoVtab",TkimgphotoVGet(),sizeof(TkimgphotoVtab));
   install_vtab("ImgintVtab",ImgintVGet(),sizeof(ImgintVtab));
#ifdef WIN32
   install_vtab("TkintplatdeclsVtab",TkintplatdeclsVGet(),sizeof(TkintplatdeclsVtab));
   install_vtab("TkplatdeclsVtab",TkplatdeclsVGet(),sizeof(TkplatdeclsVtab));
   install_vtab("TkintxlibdeclsVtab",TkintxlibdeclsVGet(),sizeof(TkintxlibdeclsVtab));
#else
   install_vtab("XlibVtab",XlibVGet(),sizeof(XlibVtab));
#endif
   Boot_Tix(aTHX);
  }
 initialized++;
}

XS(XS_Tk__MainWindow_Create)
{
 dXSARGS;
 STRLEN na;
 Tcl_Interp *interp = Tcl_CreateInterp();
 SV **args = &ST(0);
 char *appName = SvPV(ST(1),na);
 int offset = args - sp;
 int code;
 if (!initialized)
  InitVtabs();
 code = TkCreateFrame(NULL, interp, items, &ST(0), 1, appName);
 if (code != TCL_OK)
  {
   Tcl_AddErrorInfo(interp, "Tk::MainWindow::Create");
   croak("%s",Tcl_GetStringResult(interp));
  }
#if !defined(WIN32) && !defined(__PM__) && !(defined(OS2) && defined(__WIN32__))
 TkCreateXEventSource();
#endif
 TKXSRETURN(Return_Results(interp,items,offset));
}


static int
all_printable(s,n)
char *s;
int n;
{
 while (n-- > 0)
  {
   unsigned ch = *s++;
   if (!isprint(ch) && ch != '\n' && ch != '\t')
    return 0;
  }
 return 1;
}

static int
SelGetProc(clientData,interp,portion,numItems,format,type,tkwin)
ClientData clientData;
Tcl_Interp *interp;
long *portion;
int numItems;
int format;
Atom type;
Tk_Window tkwin;
{
 dTHX;
 Tcl_Obj *result = (Tcl_Obj *) clientData;
 char *p = (char *) portion;
 SV *sv  = Nullsv;
 if (format == 8)
  {
   TkWindow *winPtr = (TkWindow *) tkwin;
   TkDisplay *dispPtr = winPtr->dispPtr;
   /* Whole can-of-worms here:
      Mozilla has various text/... targets with no charset
      data which are in some 16-bit Unicode UCS-2/utf-16 style
      for which this would be correct:
        format = 16;
        numItems /= 2;
      (It is a little-endian 16-bit on Linux-x86.)

      Note that the is_utf8_string test will _PASS_ for
      the 16-bit case with plain ASCII as '\0' is legitimate UTF-8

      KDE's Konsole has text/plain;charset=xxxx
      charsets are mostly really 8-bit but also has
      ISO-10646-UCS-2 which is 16-bit with a leading BOM
      For KDE there is no real gain in using one of these
      as UTF8_STRING returns same information.
      The mozilla targets might be interesting.

      Bare is_utf8_string() test may be wrong as well as
      we may get partial characters ?
   */
   if ((dispPtr->utf8Atom != None && type == dispPtr->utf8Atom) ||
        is_utf8_string((U8 *) p, numItems))
    {
     Tcl_AppendToObj(result, p, numItems);
    }
   else
    {
     const char *strType = Tk_GetAtomName(tkwin, type);
     /* Do NOT use NewStringObj on it that assumes UTF-8-ness
        and we have established it isn't
      */
     sv = newSVpvn(p, numItems);
#if 0
     LangDebug("%s %d '%.*s'\n",__FUNCTION__,numItems,numItems,p);
     LangDumpVec(strType,1,&sv);
     abort();
#endif
     Tcl_ListObjAppendElement(interp,result,sv);
    }
  }
 else
  {
   if (type == Tk_InternAtom(tkwin,"TARGETS"))
    type = XA_ATOM;
   while (numItems-- > 0)
    {
     IV value = 0;
     sv = Nullsv;
     if (8 * sizeof(unsigned char) == format)
      {
       value = *((unsigned char *) p);
      }
     else if (8 * sizeof(unsigned short) == format)
      {
       value = *((unsigned short *) p);
      }
     else if (8 * sizeof(unsigned int) == format)
      {
       value = *((unsigned int *) p);
      }
     else if (8 * sizeof(unsigned long) == format)
      {
       value = *((unsigned long *) p);
      }
     else
      {
       return EXPIRE((interp, "No C type for format %d", format));
      }
     p += (format / 8);
     if (type == XA_ATOM)
      {
       if (value)
        {
         sv = newSVpv(Tk_GetAtomName(tkwin,(Atom) value),0);
         sv_setiv(sv,value);
         SvPOK_on(sv);
        }
      }
     else
      sv = newSViv(value);
     if (sv)
      Tcl_ListObjAppendElement(interp,result,sv);
    }
  }
 return TCL_OK;
}

static int
isSwitch(s)
char *s;
{int ch;
 if (*s++ != '-')
  return 0;
 if (!isalpha(UCHAR(*s)))
  return 0;
 while ((ch = UCHAR(*++s)))
  {
   if (!isalnum(ch) && ch != '_')
    return 0;
  }
 return 1;
}

XS(XS_Tk__Widget_SelectionGet)
{
 dXSARGS;
 STRLEN na;
 int offset = &ST(0) - sp;
 Lang_CmdInfo *info = WindowCommand(ST(0), NULL, 3);
 TkWindow *winPtr = (TkWindow *) info->tkwin;
 TkDisplay *dispPtr = winPtr->dispPtr;
 Atom selection = XA_PRIMARY;
 Atom target    = None;
 int i = 1;
 Tcl_Obj *result = NULL;
 int retval = TCL_ERROR;
 while (i < items)
  {STRLEN len;
   char *s = SvPV(ST(i),len);
   if (len && !isSwitch(s))
    {
     target = Tk_InternAtom(info->tkwin,s);
     i += 1;
    }
   else if (len >= 2 && !strncmp(s,"-type",len))
    {
     if (i+1 < items)
      target = Tk_InternAtom(info->tkwin,SvPV(ST(i+1),na));
     i += 2;
    }
   else if (len >= 2 && !strncmp(s,"-selection",len))
    {
     if (i+1 < items)
      selection = Tk_InternAtom(info->tkwin,SvPV(ST(i+1),na));
     i += 2;
    }
   else
    croak("Bad option '%s'",s);
  }
 result = Tcl_NewObj();
 if (target == None)
  {
   /* Caller did not specify a target
       Try UTF8_STRING and if that fails try STRING

       But if they _ask_ for STRING then target will be set
       so we don't come here and just go for STRING below.

       We could get TARGETS list and then only ask for UTF8_STRING
       if owner supports it. But that would still be two requests
       and involves either a direct call to XConvertSelection()
       which is at best faked on Win32, or poking about in list of
       strings returned for the atoms.

       The more sophisticated TARGETS approach might start to win if we
       want to try TEXT COMPOUND_TEXT text/plain and other legacy
       ways of passing non-ASCII. But it seems like most applications
       are doing UTF8_STRING these days.

       We do UTF8_STRING first as owner may advertise STRING but
       fail to return it if selection contains high characters,
       or it may return STRING with a lot of '?' or '#' or other
       "marker" for non-converted chars. In contrast UTF-8 gives
       then no excuses ;-)

    */
   if (dispPtr->utf8Atom != None)
    {
     /* Try for UTF8_STRING */
     retval = Tk_GetXSelection(info->interp, info->tkwin, selection,
                              dispPtr->utf8Atom, SelGetProc,
                              (ClientData) result);
    }
   target = XA_STRING;
  }
 if (retval != TCL_OK)
  {
   retval = Tk_GetXSelection(info->interp, info->tkwin, selection, target,
                             SelGetProc, (ClientData) result);
  }
 if (retval != TCL_OK)
  {
   Tcl_DecrRefCount(result);
   croak("%s", Tcl_GetString(Tcl_GetObjResult(info->interp)));
  }
 retval = Return_Object(items,offset,result);
 Tcl_DecrRefCount(result);
 XSRETURN(retval);
}

static I32
InsertArg(mark,posn,sv)
SV **mark;
I32 posn;
SV *sv;
{
 dTHX;
 dSP;
 I32 items = sp - mark;
 MEXTEND(sp, 1);                     /* May not be room ? */
 while (sp > mark + posn)            /* Move all but one args up 1 */
  {
   sp[1] = sp[0];
   sp--;
  }
 mark[posn+1] = sv;
 sp = mark + (++items);
 PUTBACK;
 return items;
}

XS(XStoWidget)
{
 dXSARGS;
 Lang_CmdInfo *info = WindowCommand(ST(0), NULL, 1);
 do_watch();
 items = InsertArg(mark,1,XSANY.any_ptr);
 TKXSRETURN(Call_Tk(info, items, &ST(0)));
}

static SV *
NameFromCv(cv)
CV *cv;
{
 dTHX;
 SV *sv = NULL;
 if (cv)
  {
   GV *gv = CvGV(cv);
   char *s = GvNAME(gv);
   STRLEN l = GvNAMELEN(gv);
   sv = sv_newmortal();
   sv_setpvn(sv, s, l);
#ifdef DEBUG_GLUE
   fprintf(stderr, "Recovered name '%s'\n", Tcl_GetString(sv));
#endif
  }
 else
  croak("No CV passed");
 return sv;
}

Tk_Window
Tk_MainWindow(interp)
Tcl_Interp *interp;
{
 dTHX;
 HV *hv = InterpHv(interp,0);
 if (hv)
  {
   MAGIC *mg = mg_find((SV *) hv, PERL_MAGIC_ext);
   if (mg)
    {
     return INT2PTR(Tk_Window, SvIV(mg->mg_obj));
    }
  }
 return NULL;
}

static int
InfoFromArgs(info,proc,mwcd,items,args)
Lang_CmdInfo *info;
Tcl_ObjCmdProc *proc;
int mwcd;
int items;
SV **args;
{
 dTHX;
 SV *fallback = NULL;
 int i;
 memset(info,0,sizeof(*info));
 info->Tk.objProc = proc;
 for (i=0; i < items; i++)
  {
   SV *sv = args[i];
   if (SvROK(sv) && sv_isobject(sv))
    {
     Lang_CmdInfo *winfo = WindowCommand(sv,NULL,0);
     if (winfo && winfo->interp)
      {
       if (winfo->interp != info->interp)
        info->interp = winfo->interp;
       if (mwcd)
        {
         Tk_Window mw;
         if (winfo->tkwin)
          mw = TkToMainWindow(winfo->tkwin);
         else
          mw = Tk_MainWindow(winfo->interp);
         if (mw)
          {
           if ((ClientData) mw != info->Tk.objClientData)
            {
             if (info->Tk.objClientData)
              {
               PerlIO_printf(PerlIO_stderr(),"cmd %p/%p using %p/%p\n",
                       info->Tk.objClientData,info->interp,
                       mw, winfo->interp);
              }
             info->Tk.objClientData = (ClientData) mw;
            }
          }
        }
       return i;
      }
    }
  }
 fallback = perl_get_sv("Tk::_Interp",TRUE);
 if (!SvROK(fallback))
  {
   Tcl_Interp *interp = Tcl_CreateInterp();
   SV *sv = sv_2mortal(MakeReference((SV *) interp));
#if 0
   Tcl_CallWhenDeleted(interp, TkEventCleanupProc, (ClientData) NULL);
#endif
   SvSetMagicSV(fallback,sv);
  }
 info->interp = (Tcl_Interp *) SvRV(fallback);
 return -1;
}

static
XS(XStoSubCmd)
{
 dXSARGS;
 STRLEN na;
 Lang_CmdInfo info;
 SV *name = NameFromCv(cv);
 int posn = InfoFromArgs(&info,(Tcl_ObjCmdProc *) XSANY.any_ptr,1,items,&ST(0));
 if (posn < 0)
  {
#if 0
   LangDumpVec(Tcl_GetString(name),items,&ST(0));
#endif
   die_with_trace(ST(0),"XStoSubCmd: Not a Tk Window");
  }
 if (posn == 0)
  {
   /* Do arg re-ordering to covert grab/wm like calls from
      perl method call form to that expected by Tk
              0   1   2
      have [ win sub ?-opt? ....     ]
      need [ cv  sub ?-opt? win ...  ]

    */

   MEXTEND(sp, 1);                /* May not be room ? */
   while (sp > mark + 2)          /* Move all but two args up 1 */
    {
     if (SvPOK(*sp) && isSwitch(SvPV(*sp, na)))
      break;
     sp[1] = sp[0];
     sp--;
    }
   sp[1] = mark[1];               /* Move object = window arg */
   sp = mark + (++items);         /* move sp past the lot */
   PUTBACK;                       /* and reset the global */
  }
 ST(0) = name;          /* Fill in command name */
 TKXSRETURN(Call_Tk(&info, items, &ST(0)));
}

static
XS(XStoEvent)
{
 dXSARGS;
 STRLEN na;
 Lang_CmdInfo info;
 SV *name = NameFromCv(cv);
 int posn = InfoFromArgs(&info,(Tcl_ObjCmdProc *) XSANY.any_ptr,1,items,&ST(0));
 if (posn < 0)
  {
   croak("XStoEvent:%s is not a Tk Window",SvPV(ST(0),na));
  }
 if (posn == 0)
  {
   if (SvPOK(mark[2]) && strcmp(SvPV(mark[2], na), "generate") == 0)
    {
      /* Do arg re-ordering to convert calls from
	 perl method call form to that expected by Tk
	        0   1   2
	 have [ win sub ?-opt? ....     ]
	 need [ cv  sub win ?-opt? ...  ]

	 */
     MEXTEND(sp, 1);                /* May not be room ? */
     while (sp > mark + 2)          /* Move all but two args up 1 */
      {
       sp[1] = sp[0];
       sp--;
      }
     sp[1] = mark[1];               /* Move object = window arg */
     sp = mark + (++items);         /* move sp past the lot */
     PUTBACK;                       /* and reset the global */
    }
  }
 ST(0) = name;          /* Fill in command name */
 TKXSRETURN(Call_Tk(&info, items, &ST(0)));
}


static
XS(XStoAfterSub)
{
 dXSARGS;
 STRLEN na;
 Lang_CmdInfo info;
 SV *name = NameFromCv(cv);
 int posn = InfoFromArgs(&info,(Tcl_ObjCmdProc *) XSANY.any_ptr,1,items,&ST(0));
 if (posn != 0)
  {
   LangDumpVec(SvPV(name,na),items,&ST(0));
   croak("Usage $widget->%s(...)",SvPV(name,na));
  }
 /* Find a place for the widget arg after a possible subcommands */
 posn = 1;
 if (posn < items && SvPOK(ST(posn)) && !isSwitch(SvPV(ST(posn),na)))
  posn++;
 items = InsertArg(mark,posn,ST(0));
 ST(0) = name;          /* Fill in command name */
 Tcl_GetCommandInfo(info.interp,Tcl_GetString(name),&info.Tk);
 TKXSRETURN(Call_Tk(&info, items, &ST(0)));
}

static
XS(XStoGrid)
{
 dXSARGS;
 STRLEN na;
 Lang_CmdInfo info;
 SV *name = NameFromCv(cv);
 int posn = InfoFromArgs(&info,(Tcl_ObjCmdProc *) XSANY.any_ptr,1,items,&ST(0));
 if (posn == 0 && 0)
  {
   /* Find a place for the widget arg after a possible subcommands */
   posn = 1;
   if (posn < items && SvPOK(ST(posn)) && !isSwitch(SvPV(ST(posn),na)))
    posn++;
   items = InsertArg(mark,posn,ST(0));
   ST(0) = name;          /* Fill in command name */
  }
 items = InsertArg(mark,0, name);
#if 0
 LangDumpVec("grid", items, &ST(0));
#endif
 TKXSRETURN(Call_Tk(&info, items, &ST(0)));
}


static
XS(XStoDisplayof)
{
 dXSARGS;
 STRLEN na;
 Lang_CmdInfo info;
 SV *name = NameFromCv(cv);
 int posn = InfoFromArgs(&info,(Tcl_ObjCmdProc *) XSANY.any_ptr,1,items,&ST(0));
 if (posn != 0)
  {
   LangDumpVec(SvPV(name,na),items,&ST(0));
   croak("Usage $widget->%s(...)",SvPV(name,na));
  }
 posn = 1;
 if (posn < items && SvPOK(ST(posn)) && !isSwitch(SvPV(ST(posn),na)))
  posn++;
 items = InsertArg(mark,posn++,sv_2mortal(newSVpv("-displayof",0)));
 SPAGAIN;
 mark = sp-items;
 items = InsertArg(mark,posn,ST(0));
 ST(0) = name;          /* Fill in command name */
 TKXSRETURN(Call_Tk(&info, items, &ST(0)));
}

static
XS(XStoTk)
{
 dXSARGS;
 STRLEN na;
 SV *name = NameFromCv(cv);
 Lang_CmdInfo info;
 int posn =  InfoFromArgs(&info,(Tcl_ObjCmdProc *) XSANY.any_ptr,1,items,&ST(0));
 if (posn < 0)
  {
   LangDumpVec(SvPV(name,na),items,&ST(0));
   croak("Usage $widget->%s(...)",SvPV(name,na));
  }
 if (items == 0 || !SvPOK(ST(0)) || strcmp(SvPV(ST(0),na),BASEEXT) != 0)
  {
   items = InsertArg(mark,0,name);
  }
 ST(0) = name;                      /* Fill in command name */
 TKXSRETURN(Call_Tk(&info, items, &ST(0)));
}

static
XS(XStoOption)
{
 dXSARGS;
 STRLEN na;
 SV *name = NameFromCv(cv);
 Lang_CmdInfo info;
 int posn =  InfoFromArgs(&info, LangOptionCommand, 1, items, &ST(0));
 if (posn < 0)
  {
   LangDumpVec(SvPV(name,na),items,&ST(0));
   croak("Usage $widget->%s(...)",SvPV(name,na));
  }
 if (items > 1 && SvPOK(ST(1)) && !strcmp(SvPV(ST(1),na),"get"))
  {
   items = InsertArg(mark,2,ST(0));
  }
 ST(0) = name;                      /* Fill in command name */
 TKXSRETURN(Call_Tk(&info, items, &ST(0)));
}

static
XS(XStoImage)
{
 dXSARGS;
 STRLEN na;
 SV *name = NameFromCv(cv);
 Lang_CmdInfo info;
 int posn =  InfoFromArgs(&info,(Tcl_ObjCmdProc *) XSANY.any_ptr,1,items,&ST(0));
 if (posn < 0)
  {
   LangDumpVec(SvPV(name,na),items,&ST(0));
   croak("Usage $widget->%s(...)",SvPV(name,na));
  }
 if (items > 1 && SvPOK(ST(1)))
  {
   char *opt = SvPV(ST(1),na);
   if (strcmp(opt,"create") && strcmp(opt,"names") && strcmp(opt,"types"))
    {
    items = InsertArg(mark,2,ST(0));
    }
  }
 ST(0) = name;                      /* Fill in command name */
#if 0
 LangDumpVec("Image",items,&ST(0));
#endif
 TKXSRETURN(Call_Tk(&info, items, &ST(0)));
}

static
XS(XStoFont)
{
 dXSARGS;
 STRLEN na;
 SV *name = NameFromCv(cv);
 Lang_CmdInfo info;
 int posn =  InfoFromArgs(&info,(Tcl_ObjCmdProc *) XSANY.any_ptr,1,items,&ST(0));
 if (posn < 0)
  {
   LangDumpVec(SvPV(name,na),items,&ST(0));
   croak("Usage $widget->%s(...)",SvPV(name,na));
  }
 if (items > 1 && SvPOK(ST(1)))
  {
   char *opt = SvPV(ST(1),na);
   if (strcmp(opt,"create") && strcmp(opt,"names") && strcmp(opt,"families"))
    {
     /* FIXME: would be better to use hint from info rather than fact that
        object is not hash-based */
     if (SvROK(ST(0)) && SvTYPE(SvRV(ST(0))) != SVt_PVHV)
      {
       items = InsertArg(mark,2,ST(0));
      }
     else if (ST(2) == &PL_sv_undef)
      {
#if 0
       LangDumpVec("Font undef",items,&ST(0));
#endif
       croak("Cannot use undef as font object");
      }
    }
  }
 
 ST(0) = name;                      /* Fill in command name */
#if 0
 LangDumpVec("Font Post",items,&ST(0));
#endif
 TKXSRETURN(Call_Tk(&info, items, &ST(0)));
}

int
XSTkCommand (CV *cv, int mwcd, Tcl_ObjCmdProc *proc, int items, SV **args)
{
 dTHX;
 STRLEN na;
 Lang_CmdInfo info;
 SV *name = NameFromCv(cv);
 if (InfoFromArgs(&info,proc,mwcd,items,args) != 0)
  {
   croak("Usage $widget->%s(...)\n%s is not a Tk object",
         SvPV_nolen(name),SvPV_nolen(args[0]));
  }
 /* Having established a widget was passed in ST(0) overwrite
    with name of command Tk is expecting
  */
 args[0] = name;          /* Fill in command name */
 if (1 || !mwcd)
  {
   char *s = Tcl_GetString(name);
   Tcl_GetCommandInfo(info.interp,s,&info.Tk);
   if (!proc && info.Tk.objProc)
    {
     proc = info.Tk.objProc;
    }
   CvXSUBANY(cv).any_ptr = proc;
   if (!info.Tk.objProc && !info.Tk.proc)
    {
     info.Tk.objProc = proc;
     Tcl_SetCommandInfo(info.interp,s,&info.Tk);
    }
  }
 return Call_Tk(&info, items, args);
}

static
XS(XStoTclCmd)
{
 dXSARGS;
 TKXSRETURN(XSTkCommand(cv,1,(Tcl_ObjCmdProc *) XSANY.any_ptr, items, &ST(0)));
}

static
XS(XStoTclCmdNull)
{
 dXSARGS;
 TKXSRETURN(XSTkCommand(cv,0,(Tcl_ObjCmdProc *) XSANY.any_ptr, items, &ST(0)));
}

static
XS(XStoNoWindow)
{
 dXSARGS;
 STRLEN na;
 Lang_CmdInfo info;
 SV *name = NameFromCv(cv);
 HV *cm;
 STRLEN sz;
 char *cmdName = SvPV(name,sz);
 SV **x  ;
 InfoFromArgs(&info,(Tcl_ObjCmdProc *) XSANY.any_ptr,0,items,&ST(0));
 cm = FindHv(aTHX_ info.interp, "XStoNoWindow", 0, CMD_KEY);
 Tcl_GetCommandInfo(info.interp,cmdName,&info.Tk);
 if (items > 0 && (sv_isobject(ST(0)) || !strcmp(SvPV(ST(0),na),BASEEXT)))
  ST(0) = name;         /* Fill in command name */
 else
  items = InsertArg(mark,0,name);
 TKXSRETURN(Call_Tk(&info, items, &ST(0)));
}

static CV *
TkXSUB(const char *name,XSptr xs,Tcl_ObjCmdProc *proc)
{
 dTHX;
 STRLEN na;
 SV *sv = newSVpv(BASEEXT,0);
 CV *cv;
 sv_catpv(sv,"::");
 sv_catpv(sv,name);
 if (xs && proc)
  {
   cv = newXS(SvPV(sv,na),xs,__FILE__);
   CvXSUBANY(cv).any_ptr = (VOID *) proc;
  }
 else
  {
   cv = perl_get_cv(SvPV(sv,na),0);
  }
 SvREFCNT_dec(sv);
 return cv;
}

void
Lang_TkCommand(name,proc)
char *name;
Tcl_ObjCmdProc *proc;
{
 TkXSUB(name,XStoTclCmd,proc);
}

void
Lang_TkSubCommand(name,proc)
char *name;
Tcl_ObjCmdProc *proc;
{
 TkXSUB(name,XStoAfterSub,proc);
}


/*
  The bind command is handled specially, it must *always* be called
  with a widget object. And only the <> form of sequence is allowed
  so that the following forms of call can be spotted:

  $widget->bind();
  $widget->bind('tag');
  $widget->bind('<...>');
  $widget->bind('tag','<...>');
  $widget->bind('<...>',command);
  $widget->bind('tag','<...>',command);

*/

static
XS(XStoBind)
{
 dXSARGS;
 STRLEN na;
 Lang_CmdInfo info;
 SV *name = NameFromCv(cv);
 int posn = InfoFromArgs(&info,(Tcl_ObjCmdProc *) XSANY.any_ptr,1,items,&ST(0));
 STRLEN len;
 if (posn < 0)
  {
   LangDumpVec(SvPV(name,na),items,&ST(0));
   croak("Usage $widget->%s(...)",SvPV(name,na));
  }
 if (items < 2 || *SvPV(ST(1),len) == '<')
  {
   /* Looks like $widget->bind([<..>])
    * i.e. bind command to widget itself
    * Standard move up of all the args to make room for 'bind'
    * as argv[0]
    */
   items = InsertArg(mark,0,name);
  }
 else
  {
   /* Looks like $widget->bind('tag',...)
    * simply overwrite 0'th argument with 'bind'
    */
   ST(0) = name;          /* Fill in command name */
#if 0
   if (dowarn)
    {
     if (items == 4)
      {
       SV *sv = ST(3);
       if (SvROK(sv) && SvTYPE(SvRV(sv)) == SVt_PVCV)
        {
         LangDumpVec("bind",items,&ST(0));
         warn("Subreference for class binding");
        }
      }
    }
#endif
  }
 TKXSRETURN(Call_Tk(&info, items, &ST(0)));
}


void
LangDeadWindow(interp, tkwin)
Tcl_Interp *interp;
Tk_Window tkwin;
{
 dTHX;
 STRLEN na;
 HV *hv = InterpHv(interp,0);
 if (hv)
  {
   /* This is last hook before tkwin disapears
       - LangDeleteWidget has happened
       - <Destroy> bindings have happened
    */
   char *cmdName = Tk_PathName(tkwin);
   STRLEN cmdLen = strlen(cmdName);
   SV *obj = hv_delete(hv, cmdName, cmdLen, G_SCALAR);
   if (obj && SvROK(obj) && SvTYPE(SvRV(obj)) == SVt_PVHV)
    {
     HV *hash = (HV *) SvRV(obj);
     MAGIC *mg   = mg_find((SV *) hash,PERL_MAGIC_ext);

     /* Tk_CheckHash((SV *) hash, NULL); */
     if (SvREFCNT(hash) < 1)
      {
       LangDebug("%s %s has REFCNT=%d\n",__FUNCTION__,cmdName,(int) SvREFCNT(hash));
       sv_dump(obj);
      }

     if (mg)
      {
       Lang_CmdInfo *info = (Lang_CmdInfo *) SvPV_nolen(mg->mg_obj);
       if (info->interp != interp)
        Tcl_Panic("%s->interp=%p expected %p", cmdName, info->interp, interp);
       DecInterp(info->interp, cmdName);
       /* mg->mg_obj is SV holding the Lang_CmdInfo struct
          we have now finished with it
        */
       SvREFCNT_dec(mg->mg_obj);
       sv_unmagic((SV *) hash,PERL_MAGIC_ext);
      }
    }
  }
}


int
Tcl_DeleteCommandFromToken(interp, info)
Tcl_Interp *interp;
Tcl_Command info;
{
 if (info)
  {
   if (info->Tk.deleteProc)
    {
     (*info->Tk.deleteProc) (info->Tk.deleteData);
     info->Tk.deleteProc = NULL;
     info->Tk.deleteData = NULL;
    }
   info->Tk.clientData    = NULL;
   info->Tk.proc          = NULL;
   info->Tk.objClientData = NULL;
   info->Tk.objProc       = NULL;
  }
 return TCL_OK;
}

void
Lang_DeleteWidget(interp, info)
Tcl_Interp *interp;
Tcl_Command info;
{
 dTHX;
 Tk_Window tkwin = info->tkwin;
 char *cmdName = Tk_PathName(tkwin);
 SV *win = WidgetRef(interp, cmdName);
 /* This is first sign of disapearing widget, <Destroy> bindings
    are still to come.
  */
 LangMethodCall(interp,win,"_Destroyed",0,0);
 Tcl_DeleteCommandFromToken(interp,info);
 if (win && SvOK(win))
  {
   HV *hash = NULL;
   Lang_CmdInfo *info = WindowCommand(win,&hash,1);
   if (info->interp != interp)
    Tcl_Panic("%s->interp=%p expected %p", cmdName, info->interp, interp);
   if (hash)
    hv_delete(hash, XEVENT_KEY, strlen(XEVENT_KEY), G_DISCARD);
   /* Tk_CheckHash((SV *) hash, NULL); */
   if (SvREFCNT(hash) < 2)
    {
     LangDebug("%s %s has REFCNT=%d",__FUNCTION__,cmdName,(int) SvREFCNT(hash));
    }
   SvREFCNT_dec(hash);
  }
}

void
Lang_DeleteObject(interp, info)
Tcl_Interp *interp;
Tcl_Command info;
{
 dTHX;
 STRLEN na;
 char *cmdName = SvPV(info->image,na);
 if (info->interp != interp)
  Tcl_Panic("%s->interp=%p expected %p", cmdName, info->interp, interp);
 Tcl_DeleteCommandFromToken(interp, info);
 DecInterp(info->interp,cmdName);
}

void
Lang_NewMainWindow(interp,tkwin)
Tcl_Interp *interp;
Tk_Window tkwin;
{
 dTHX;
 tilde_magic((SV *) InterpHv(interp,1),newSViv(PTR2IV(tkwin)));
}

Tcl_Command
Lang_CreateWidget(interp, tkwin, proc, clientData, deleteProc)
Tcl_Interp *interp;
Tk_Window tkwin;
Tcl_ObjCmdProc *proc;
ClientData clientData;
Tcl_CmdDeleteProc *deleteProc;
{
 dTHX;
 STRLEN na;
 HV *hv = InterpHv(interp,1);
 char *cmdName = (tkwin) ? Tk_PathName(tkwin) : ".";
 STRLEN cmdLen = strlen(cmdName);
 HV *hash = newHV();
 SV *tmp;
 Lang_CmdInfo info;
 SV *sv;
 do_watch();
 memset(&info,0,sizeof(info));
 info.Tk.objProc = proc;
 info.Tk.deleteProc = deleteProc;
 info.Tk.objClientData = info.Tk.deleteData = clientData;
 info.interp = interp;
 info.tkwin = tkwin;
 info.image = NULL;
 sv = struct_sv(&info,sizeof(info));

 /* Record the object in the main hash */
 IncInterp(interp, cmdName);

 hv_store(hv, cmdName, cmdLen, newRV((SV *) hash), 0);
 /* At this point hash REFCNT should be 2, one for what is stored
    in interp and one representing Tk's use
  */
 tilde_magic((SV *) hash, sv);
 return (Lang_CmdInfo *) SvPV(sv,na);
}

Tcl_Command
Lang_CreateObject(interp, cmdName, proc, clientData, deleteProc)
Tcl_Interp *interp;
char *cmdName;
Tcl_ObjCmdProc *proc;
ClientData clientData;
Tcl_CmdDeleteProc *deleteProc;
{
 dTHX;
 STRLEN na;
 HV *hv = InterpHv(interp,1);
 STRLEN cmdLen = strlen(cmdName);
 HV *hash = newHV();
 SV *sv;
 Lang_CmdInfo info;
 do_watch();
 memset(&info,0,sizeof(info));
 info.Tk.objProc = proc;
 info.Tk.deleteProc = deleteProc;
 info.Tk.objClientData = info.Tk.deleteData = clientData;
 info.interp = interp;
 info.tkwin = NULL;
 info.image = newSVpv(cmdName,cmdLen);
 sv =  struct_sv(&info,sizeof(info));
 /* Record the object in the main hash */
 IncInterp(interp, cmdName);
 hv_store(hv, cmdName, cmdLen, MakeReference((SV *) hash), 0);
 tilde_magic((SV *)hash, sv);
 return (Lang_CmdInfo *) SvPV(sv,na);
}

Tcl_Command
Lang_CreateImage(interp, cmdName, proc, clientData, deleteProc, typePtr)
Tcl_Interp *interp;
char *cmdName;
Tcl_ObjCmdProc *proc;
ClientData clientData;
Tcl_CmdDeleteProc *deleteProc;
Tk_ImageType *typePtr;
{
 return Lang_CreateObject(interp, cmdName, proc, clientData, deleteProc);
}

Tcl_Command
Tcl_CreateObjCommand(interp, cmdName, proc, clientData, deleteProc)
Tcl_Interp *interp;
CONST char *cmdName;
Tcl_ObjCmdProc *proc;
ClientData clientData;
Tcl_CmdDeleteProc *deleteProc;
{
 Tk_Window mw = Tk_MainWindow(interp);
 if (cmdName[0] == '.')
  {
   Tk_Window tkwin;
   if (cmdName[1] == '\0')
    {
     tkwin = mw;
    }
   else
    {
     tkwin = Tk_NameToWindow(interp, (char *) cmdName, mw);
    }
   return Lang_CreateWidget(interp, tkwin, proc, clientData, deleteProc);
  }
 else
  {
   Tcl_CmdInfo info;
   CV *cv;
   char *kind = "NULL";
   if (clientData)
    {
     kind = (clientData == (ClientData) mw) ? "mw" : "custom";
    }
   memset(&info,0,sizeof(info));
   info.objProc = proc;
   info.objClientData = clientData;
   info.deleteProc = deleteProc;
   if (!strcmp(cmdName,"menu"))
    {
     cmdName = "_menu";
    }
   /* We cannot test sanity of clientData vs XStoXxxxx at this point
      as when 1st called XSs are still pointing a B::C friendly re-directors.
      Also CVs for "loaded" commands may not exist yet.
    */
#if 0
   if ((cv = TkXSUB(cmdName,NULL,NULL)))
    {
     if (clientData)
      {
       if (clientData == (ClientData) mw)
        {
         if (CvXSUB(cv) == XStoTclCmdNull)
          {
           warn("Wrong xsub %s cd=%p (mw)",cmdName, clientData);
           CvXSUB(cv) = XStoTclCmd;
          }
        }
       else
        {
         if (CvXSUB(cv) == XStoTclCmd)
          {
           warn("Wrong xsub %s cd=%p",cmdName, clientData);
           CvXSUB(cv) = XStoTclCmdNull;
         }
        }
      }
     else
      {
       if (CvXSUB(cv) == XStoTclCmd)
        {
         warn("Wrong xsub %s cd=%p",cmdName, clientData);
         CvXSUB(cv) = XStoTclCmdNull;
        }
      }
    }
   else
    {
     warn("No cv for %s",cmdName);
    }
#endif
   Tcl_SetCommandInfo(interp,cmdName,&info);
   if (deleteProc)
    {
     HV *hv = InterpHv(interp,1);
     Tcl_CallWhenDeleted(interp,(Tcl_InterpDeleteProc *)deleteProc,clientData);
    }
  }
 return NULL;
}

int
Tcl_IsSafe(interp)
Tcl_Interp *interp;
{
 return 0; /* Is this interp in a 'safe' compartment - not yet implemented */
}

int
Tcl_HideCommand (Tcl_Interp *interp, CONST char *cmdName, CONST char *hiddenCmdName)
{
 CV *cv = TkXSUB(cmdName,NULL,NULL);
 warn("Tcl_HideCommand %s => %s called",cmdName,hiddenCmdName);
 if (!cv)
  {
   return EXPIRE((interp,"Cannot find %s", cmdName));
  }
 return TCL_OK;
}

int
Tcl_SetCommandInfo(interp,cmdName,infoPtr)
Tcl_Interp *interp;
CONST char *cmdName;
CONST Tcl_CmdInfo *infoPtr;
{
 dTHX;
 HV *cm = FindHv(aTHX_ interp, "Tcl_SetCommandInfo", 1, CMD_KEY);
 hv_store(cm,cmdName,strlen(cmdName),
          struct_sv((char *) infoPtr,sizeof(*infoPtr)),0);
 return TCL_OK;
}

int
Tcl_GetCommandInfo (Tcl_Interp *interp, CONST char *cmdName, Tcl_CmdInfo *infoPtr)
{
 dTHX;
 HV *hv = InterpHv(interp,1);
 SV **svp = hv_fetch(hv,cmdName,strlen(cmdName),0);
 /* Widgets, images and named fonts get put in main hash */
 if (svp && *svp)
  {
   Lang_CmdInfo *info = WindowCommand(*svp,NULL,0);
   *infoPtr = info->Tk;
   return 1;
  }
 /* widgets are special cased elsewhere */
 else if (*cmdName != '.')
  {
   HV *cm = FindHv(aTHX_ interp, "Tcl_GetCommandInfo", 1, CMD_KEY);
   SV **svp = hv_fetch(cm,cmdName,strlen(cmdName),0);
   if (svp && *svp)
    {
     memcpy(infoPtr,SvPVX(*svp),sizeof(Tcl_CmdInfo));
     return 1;
    }
   else if (0)
    {
     /* If we didn't find the info but this is supposed to
        be a known Tk builtin then something may have gone wrong
        but "after" seems to occur regularly
      */
     CV *cv = TkXSUB(cmdName,NULL,NULL);
     if (cv)
      {
       LangDebug("No Tcl_GetCommandInfo for %s\n",cmdName);
      }
    }
  }
 return 0;
}

Tcl_Command
Tcl_CreateCommand(interp, cmdName, proc, clientData, deleteProc)
Tcl_Interp *interp;
CONST char *cmdName;
Tcl_CmdProc *proc;
ClientData clientData;
Tcl_CmdDeleteProc *deleteProc;
{
 return Tcl_CreateObjCommand(interp, cmdName, (Tcl_ObjCmdProc *) proc, clientData, deleteProc);
}

static SV *LangVar2 _((Tcl_Interp *interp, SV *sv, char *part2, int flags));

static SV *
LangVar2(interp, sv, part2, store)
Tcl_Interp *interp;
SV *sv;
char *part2;
int store;
{
 if (part2)
  {
   dTHX;
   if (SvTYPE(sv) == SVt_PVHV)
    {HV *hv = (HV *) sv;
     SV **x = hv_fetch(hv, part2, strlen(part2), store);
     if (x)
      return *x;
    }
   else
    {
     Tcl_Panic("two part %s not implemented", "Tcl_GetVar2");
    }
   return NULL;
  }
 else
  return sv;
}

Tcl_Obj *
Tcl_ObjGetVar2(interp, sv, part2, flags)
Tcl_Interp *interp;
SV *sv;
SV *part2;
int flags;
{
 dTHX;
 if (sv)
  {
   if (SvPOK(sv))
    {
     STRLEN len;
     char *s = SvPV(sv,len);
     if (len > 6 && !strncmp(s,"::tk::",6))
      {
       sv = FindTkVarName(s+6,0);
      }
    }
   if (SvROK(sv) && SvTYPE(SvRV(sv)) != SVt_PVAV)
    {
     sv = SvRV(sv);
    }
   if (part2)
    {
     sv = LangVar2(interp, sv, Tcl_GetString(part2), 0);
    }
  }
 else
  {
   sv = newSV(0);
  }
 return sv;
}

Tcl_Obj *
Tcl_ObjSetVar2(interp, sv, part2, newValue, flags)
Tcl_Interp *interp;
SV *sv;
SV *part2;
SV *newValue;
int flags;
{
 dTHX;
 if (SvROK(sv))
  sv = SvRV(sv);
 if (part2)
  sv = LangVar2(interp, sv , Tcl_GetString(part2), 1);
 SvSetMagicSV(sv, newValue);
 return sv;
}



char *
Tcl_SetVarArg(interp, sv, newValue, flags)
Tcl_Interp *interp;
SV *sv;
Tcl_Obj * newValue;
int flags;
{
 dTHX;
 STRLEN na;
 if (!newValue)
  newValue = &PL_sv_undef;
 SvSetMagicSV(sv, newValue);
 return SvPV(sv, na);
}

int
LangCmpOpt(opt,arg,len)
CONST char *opt;
CONST char *arg;
size_t len;
{
 int result = 0;
 if (!len)
  len = strlen(arg);
 if (*opt == '-')
  opt++;
 if (*arg == '-')
  {
   arg++;
   if (len)
    len--;
  }
 while (len--)
  {char ch = *arg++;;
   if ((result = *opt++ - ch) || !ch)
    break;
  }
 return result;
}

int
LangCmpArg(a,b)
CONST SV *a;
CONST SV *b;
{
 dTHX;
 STRLEN na;
 char *as;
 char *bs;
 if (a && SvGMAGICAL(a))
  mg_get((SV *) a);
 if (b && SvGMAGICAL(b))
  mg_get((SV *) b);
 as = (a && SvOK(a)) ? SvPV((SV *)a,na) : "";
 bs = (b && SvOK(b)) ? SvPV((SV *)b,na) : "";
 return strcmp(as,bs);
}

static I32
Perl_Value(pTHX_ IV ix, SV *sv)
{
 Tk_TraceInfo *p = INT2PTR(Tk_TraceInfo *, ix);
 char *result;

 /* We are a "magic" set processor, whether we like it or not
    because this is the hook we use to get called.
    So we are (I think) supposed to look at "private" flags
    and set the public ones if appropriate.
    e.g. "chop" sets SvPOKp as a hint but not SvPOK

    presumably other operators set other private bits.

    Question are successive "magics" called in correct order?

    i.e. if we are tracing a tied variable should we call
    some magic list or be careful how we insert ourselves in the list?

  */
 if (!SvPOK(sv) && SvPOKp(sv))
  SvPOK_on(sv);

 if (!SvNOK(sv) && SvNOKp(sv))
  SvNOK_on(sv);

 if (!SvIOK(sv) && SvIOKp(sv))
  SvIOK_on(sv);
 return 0;
}

static void
TraceExitHandler(ClientData clientData)
{
 dTHX;
 Tk_TraceInfo *p = (Tk_TraceInfo *) clientData;
 char *result;
 ENTER;
 SvREFCNT_inc(p->sv);
 save_freesv(p->sv);
 result = (*p->proc) (p->clientData, p->interp, p->sv, p->part2, 0);
 if (result)
  Tcl_Panic("Tcl_VarTraceProc returned '%s'", result);
 LEAVE;
}


static DECL_MG_UFUNC(Perl_Trace, ix, sv)
{
 Tk_TraceInfo *p = INT2PTR(Tk_TraceInfo *, ix);
 char *result;

 /* We are a "magic" set processor, whether we like it or not
    because this is the hook we use to get called.
    So we are (I think) supposed to look at "private" flags
    and set the public ones if appropriate.
    e.g. "chop" sets SvPOKp as a hint but not SvPOK

    presumably other operators set other private bits.

    Question are successive "magics" called in correct order?

    i.e. if we are tracing a tied variable should we call
    some magic list or be careful how we insert ourselves in the list?

  */

 /* This seems to be wrong in at least one case --- see t/Trace.t and
    Message-ID: <3ef348b.0304240510.299e5519@posting.google.com>
 */
#if 0
 if (!SvPOK(sv) && SvPOKp(sv))
  SvPOK_on(sv);

 if (!SvNOK(sv) && SvNOKp(sv))
  SvNOK_on(sv);

 if (!SvIOK(sv) && SvIOKp(sv))
  SvIOK_on(sv);
#endif

 ENTER;
 SvREFCNT_inc(sv);
 save_freesv(sv);
 result = (*p->proc) (p->clientData, p->interp, sv, p->part2, 0);
 if (result)
  Tcl_Panic("Tcl_VarTraceProc returned '%s'", result);
 LEAVE;
 return 0;
}

int
Lang_TraceVar2(interp, sv, part2, flags, tkproc, clientData)
Tcl_Interp *interp;
Tcl_Obj * sv;
char *part2;
int flags;
Lang_VarTraceProc *tkproc;
ClientData clientData;
{
 dTHX;
 Tk_TraceInfo *p;
 struct ufuncs *ufp;
 MAGIC **mgp;
 MAGIC *mg;
 MAGIC *mg_list;
 SV *exiting;
 int mgType = PERL_MAGIC_uvar;

 if (SvROK(sv))
  sv = SvRV(sv);

 if (SvTHINKFIRST(sv))
  {
   if (SvREADONLY(sv))
    {
     return EXPIRE((interp, "Cannot trace readonly variable"));
    }
  }
 if (!SvUPGRADE(sv, SVt_PVMG))
  {
   return EXPIRE((interp, "Trace SvUPGRADE failed"));
  }

 if (SvTYPE(sv) == SVt_PVAV)
  {
   mgType = PERL_MAGIC_ext;
  }

 /*
  * We can't use sv_magic() because it won't add in another magical struct
  * of type 'U' if there is already one there. We need multiple 'U'
  * magics hanging from one sv or else things like radiobuttons will
  * not work. That's because each radiobutton widget group needs to track
  * the same sv and update itself as necessary.
  */

 New(601, p, 1, Tk_TraceInfo);

 p->proc = tkproc;
 p->clientData = clientData;
 p->interp = interp;
 p->part2 = part2;
 p->sv    = sv;

 Tcl_CreateExitHandler(TraceExitHandler, (ClientData) p);

 /* We want to be last in the chain so that any
    other magic has been called first
    save the list so that this magic can be moved to the end
  */
 mg_list = SvMAGIC(sv);
 SvMAGIC(sv) = NULL;

 /* Add 'U' magic to sv with all NULL args */
 sv_magic(sv, 0, mgType, 0, 0);

 Newz(666, ufp, 1, struct ufuncs);
 ufp->uf_val = Perl_Value;
 ufp->uf_set = Perl_Trace;
 ufp->uf_index = PTR2IV(p);

 mg = SvMAGIC(sv);
 mg->mg_ptr = (char *) ufp;
 mg->mg_len = sizeof(struct ufuncs);


 /* put list back and add mg to end */

 SvMAGIC(sv) = mg_list;
 mgp = &SvMAGIC(sv);
 while ((mg_list = *mgp))
  {
   mgp = &mg_list->mg_moremagic;
  }
 *mgp = mg;

 if (mgType == PERL_MAGIC_ext)
  {
   /* We are not doing a real tie to an AV so
      we need to set the vtable and re-calc magic flags
    */
   mg->mg_virtual = &PL_vtbl_uvar;
   mg_magical(sv);
  }

 if (!SvMAGICAL(sv))
  abort();

 return TCL_OK;
}

SV *
FindTkVarName(varName,flags)
CONST char *varName;
int flags;
{
 dTHX;
 STRLEN na;
 SV *name = newSVpv(BASEEXT,strlen(BASEEXT));
 SV *sv;
 sv_catpv(name,"::");
 if (!strncmp(varName,"tk_",3))
  varName += 3;
 sv_catpv(name,varName);
 sv = get_sv(SvPV(name,na),flags);
 SvREFCNT_dec(name);
 return sv;
}

char *
LangLibraryDir()
{
 dTHX;
 STRLEN na;
 SV *sv = FindTkVarName("library",0);
 if (sv && SvPOK(sv))
  return SvPV(sv,na);
 return NULL;
}

static
DECL_MG_UFUNC(LinkIntSet,ix,sv)
{
 int *p = INT2PTR(int *, ix);
 (*p) = SvIV(sv);
 return 0;
}

static
DECL_MG_UFUNC(LinkDoubleSet,ix,sv)
{
 double *p = INT2PTR(double *, ix);
 (*p) = SvNV(sv);
 return 0;
}

static
DECL_MG_UFUNC(LinkCannotSet,ix,sv)
{
 croak("Attempt to set readonly linked variable");
 return 0;
}

static
DECL_MG_UFUNC(LinkIntVal,ix,sv)
{
 int *p = INT2PTR(int *, ix);
 sv_setiv(sv,*p);
 return 0;
}

static
DECL_MG_UFUNC(LinkDoubleVal,ix,sv)
{
 double *p = INT2PTR(double *, ix);
 sv_setnv(sv,*p);
 return 0;
}

int
Tcl_LinkVar(interp,varName,addr,type)
Tcl_Interp *interp;
CONST char *varName;
char *addr;
int type;
{
 dTHX;
 SV *sv = FindTkVarName(varName,0);
 if (sv)
  {
   struct ufuncs uf;
   uf.uf_index = PTR2IV(addr);
   switch(type & ~TCL_LINK_READ_ONLY)
    {
     case TCL_LINK_INT:
     case TCL_LINK_BOOLEAN:
      uf.uf_val   = LinkIntVal;
      uf.uf_set   = LinkIntSet;
      *((int *) addr) = SvIV(sv);
      break;
     case TCL_LINK_DOUBLE:
      uf.uf_val   = LinkDoubleVal;
      uf.uf_set   = LinkDoubleSet;
      *((double *) addr) = SvNV(sv);
      break;
     case TCL_LINK_STRING:
     default:
      return EXPIRE((interp,"Cannot link %s type %d\n",varName,type));
    }
   if (type & TCL_LINK_READ_ONLY)
    {
     uf.uf_set   = LinkCannotSet;
    }
   sv_magic(sv,NULL, PERL_MAGIC_uvar, (char *) (&uf), sizeof(uf));
   return TCL_OK;
  }
 else
  {
   return EXPIRE((interp,"No variable %s\n",varName));
  }
}

void
Tcl_UnlinkVar(interp,varName)
Tcl_Interp *interp;
CONST char *varName;
{
 dTHX;
 SV *sv = FindTkVarName(varName,0);
 if (sv)
  {
   sv_unmagic(sv,PERL_MAGIC_uvar);
  }
}

void
Lang_UntraceVar(interp, sv, flags, tkproc, clientData)
Tcl_Interp *interp;
SV *sv;
int flags;
Lang_VarTraceProc *tkproc;
ClientData clientData;
{
 int mgType = PERL_MAGIC_uvar;
 MAGIC **mgp;
 /* it may not be magical i.e. it may never have been traced
    This occurs for example when cascade Menu gets untraced
    by same code that untraces checkbutton menu items.
    If it is not magical just ignore it.
  */

 if (SvROK(sv))
  sv = SvRV(sv);

 if (SvTYPE(sv) == SVt_PVAV)
  {
   mgType = PERL_MAGIC_ext;
  }

 if (SvMAGICAL(sv) && (mgp = &SvMAGIC(sv)))
  {
   MAGIC *mg;
   for (mg = *mgp; mg; mg = *mgp)
    {
     /*
      * Trawl through the linked list of magic looking
      * for the 'U' one which is our proc and ix.
      */
     if (mg->mg_type == mgType && mg->mg_ptr &&
         mg->mg_len  == sizeof(struct ufuncs) &&
         ((struct ufuncs *) (mg->mg_ptr))->uf_set == Perl_Trace)
      {
       struct ufuncs *uf = (struct ufuncs *) (mg->mg_ptr);
       Tk_TraceInfo *p = INT2PTR(Tk_TraceInfo *, uf->uf_index);
       if (p && p->proc == tkproc && p->interp == interp &&
           p->clientData == clientData)
        {
         *mgp = mg->mg_moremagic;
         Tcl_DeleteExitHandler(TraceExitHandler, (ClientData) p);
         Safefree(p);
         uf->uf_index = 0;
         Safefree(mg->mg_ptr);
         mg->mg_ptr = NULL;
         Safefree(mg);
        }
       else
        mgp = &mg->mg_moremagic;
      }
     else
      mgp = &mg->mg_moremagic;
    }
   if (!SvMAGIC(sv))
    {
     SvMAGICAL_off(sv);
     SvFLAGS(sv) |= (SvFLAGS(sv) & (SVp_IOK|SVp_NOK|SVp_POK)) >> PRIVSHIFT;
    }
  }
}

int
Lang_TraceVar(interp, varName, flags, proc, clientData)
Tcl_Interp *interp;
Var varName;
int flags;
Lang_VarTraceProc *proc;
ClientData clientData;
{
 return Lang_TraceVar2(interp, varName, NULL, flags, proc, clientData);
}

Tcl_Obj *
LangFindVar(interp, tkwin, name)
Tcl_Interp *interp;
Tk_Window tkwin;
CONST char *name;
{
 dTHX;
 if (tkwin)
  {
   SV *sv = TkToWidget(tkwin,NULL);
   if (name == Tk_Name(tkwin))
    name = "Value";
   if (sv && SvROK(sv))
    {
     HV *hv = (HV *) SvRV(sv);
     STRLEN l = strlen(name);
     SV **x = hv_fetch(hv, name, l, 1);
     if (!x)
      x = hv_store(hv, name, l, newSVpv("", 0), 0);
     if (x)
      return SvREFCNT_inc(*x);
    }
  }
 else
  {
   SV *sv = FindTkVarName(name,1);
   if (sv)
    return SvREFCNT_inc(sv);
  }
 return newSVpv("", 0);
}

int
LangStringMatch(string, match)
char *string;
SV *match;
{
 dTHX;
 STRLEN na;
 /* match could be a callback to perl sub to do re match */
 return Tcl_StringMatch(string, SvPV(match, na));
}

int
LangSaveVar(interp,sv,vp,type)
Tcl_Interp *interp;
Tcl_Obj * sv;
Var *vp;
int type;
{
 dTHX;
 STRLEN na;
 int old_taint = PL_tainted;
 TAINT_NOT;
 *vp = NULL;
 if (!sv)
  {
   return TCL_OK;
  }
 if (SvGMAGICAL(sv))
  mg_get(sv);
 if (SvROK(sv))
  {
   sv = SvRV(sv);
   if (sv == &PL_sv_undef)
    warn("variable is 'undef'");
   switch(type)
    {
     case TK_CONFIG_HASHVAR:
      if (SvTYPE(sv) != SVt_PVHV)
       EXPIRE((interp,"%s is not a hash",SvPV(sv,na)));
      break;
     case TK_CONFIG_ARRAYVAR:
      if (SvTYPE(sv) != SVt_PVAV)
       EXPIRE((interp,"%s is not an array",SvPV(sv,na)));
      break;
     default:
     case TK_CONFIG_SCALARVAR:
      break;
    }
   *vp = SvREFCNT_inc(sv);
   PL_tainted = old_taint;
   return TCL_OK;
  }
 else if (SvPOK(sv))
  {
   dTHX;
   HV *old_stash = CopSTASH(PL_curcop);
   char *name;
   SV *x = NULL;
   int prefix = '?';
   name = SvPV(sv,na);
#ifdef USE_ITHREADS
   CopSTASHPV(PL_curcop) = NULL;
#else
   CopSTASH(PL_curcop) = NULL;
#endif
   switch (type)
    {
     case TK_CONFIG_SCALARVAR:
      prefix = '$';
     default:
      if (!strchr(name,':'))
       {
        x = FindTkVarName(name,1);
       }
      else
       {
        x = perl_get_sv(name,1);
       }
      break;
     case TK_CONFIG_ARRAYVAR:
      x = (SV *) perl_get_av(name,TRUE);
      prefix = '@';
      break;
     case TK_CONFIG_HASHVAR:
      x = (SV *) perl_get_hv(name,TRUE);
      prefix = '%';
      break;
    }
   CopSTASH_set(PL_curcop, old_stash);
   if (x)
    {
     *vp = SvREFCNT_inc(x);
     PL_tainted = old_taint;
     return TCL_OK;
    }
   else
    Tcl_SprintfResult(interp,"%c%s does not exist",prefix,name);
  }
 else
  {
   Tcl_SprintfResult(interp,"Not a reference %s",SvPV(sv,na));
  }
 PL_tainted = old_taint;
 return TCL_ERROR;
}

void
LangFreeVar(sv)
Var sv;
{
 dTHX;
 SvREFCNT_dec(sv);
}

int
LangConfigObj(Tcl_Interp *interp, Tcl_Obj **save, Tcl_Obj *obj, int type)
{
 dTHX;
 *save = Nullsv;
 switch (type)
  {
   case TK_OPTION_OBJ:
     if (obj)
      *save = LangCopyArg(obj);
     return TCL_OK;
   case TK_OPTION_CALLBACK:
     if (obj)
      *save = LangMakeCallback(obj);
     return TCL_OK;
   case TK_OPTION_SCALARVAR:
     return LangSaveVar(interp,obj,save,TK_CONFIG_SCALARVAR);
   case TK_OPTION_ARRAYVAR:
     return LangSaveVar(interp,obj,save,TK_CONFIG_ARRAYVAR);
   case TK_OPTION_HASHVAR:
     return LangSaveVar(interp,obj,save,TK_CONFIG_HASHVAR);
   default:
     Tcl_SprintfResult(interp,"Unexpected type %d for LangConfigObj(%"SVf")",
                       type,obj);
  }
 return TCL_ERROR;
}

int
Lang_CallWithArgs(interp, sub, argc, argv)
Tcl_Interp *interp;
char *sub;
int argc;
SV *CONST *argv;
{
 dTHX;
 dSP;
 STRLEN len;
 int count;
 SV *sv = newSVpv("",0);
 if (!strncmp(sub,"tk",2))
  {
   sv_catpv(sv,"Tk::");
   sub += 2;
  }
 sv_catpv(sv,sub);
 sub = SvPV(sv,len);
 ENTER;
 SAVETMPS;
 EXTEND(sp, argc);
 PUSHMARK(sp);
 while (argc-- > 0)
  {
   XPUSHs(*argv++);
  }
 PUTBACK;
 count = perl_call_pv(sub, G_EVAL|G_SCALAR);
 SetTclResult(interp,count);
 SvREFCNT_dec(sv);
 FREETMPS;
 LEAVE;
 return Check_Eval(interp);
}

int
LangMethodCall
#ifdef STANDARD_C
_((Tcl_Interp * interp, Tcl_Obj * sv, char *method, int result, int argc,...))
#else
(interp, sv, method, result, argc, va_alist)
Tcl_Interp *interp;
SV *sv;
char *method;
int result;
int argc;
va_dcl
#endif
{
 dTHX;
 dSP;
 int flags = (result) ? 0 : G_DISCARD;
 int count = 0;
 int old_taint = PL_tainted;
 ENTER;
 SAVETMPS;
 PUSHMARK(sp);
 XPUSHs(sv_mortalcopy(sv));
 PUTBACK;
 if (argc)
  {
   va_list ap;
#ifdef I_STDARG
   va_start(ap, argc);
#else
   va_start(ap);
#endif
   PushVarArgs(ap,argc);
   va_end(ap);
  }
 PL_tainted = 0;
 sv = sv_newmortal();
 sv_setpv(sv,method);
 PL_tainted = old_taint;
 count = LangCallCallback(sv, flags | G_EVAL);
 if (result)
  SetTclResult(interp,count);
 FREETMPS;
 LEAVE;
 return Check_Eval(interp);
}

int
Tcl_EvalObjEx (Tcl_Interp *interp,Tcl_Obj *objPtr, int flags)
{
 dTHX;
 int code;
 SV *cb = LangMakeCallback(objPtr);
 SV *sv = cb;
 SvREFCNT_inc(interp);
 ENTER;
 SAVETMPS;
 if (PushCallbackArgs(interp,&sv) == TCL_OK)
  {
   int count = LangCallCallback(sv, G_SCALAR | G_EVAL);
   SetTclResult(interp,count);
  }
 FREETMPS;
 LEAVE;
 SvREFCNT_dec(cb);
 code = Check_Eval(interp);
 SvREFCNT_dec(interp);
 return code;
}

int
Tcl_EvalObj(Tcl_Interp *interp,Tcl_Obj *objPtr)
{
 return Tcl_EvalObjEx(interp,objPtr,0);
}

/*
 * Tcl_EvalObjv is used by tkMenu.c's CloneMenu
 * In order to allow Tk::Menu::tkMenuDup to return
 * the "object" for the created menu we pass actual
 * objects not mortal copies.
 * We also avoid the overhead of creating, blessing and destroying
 * "Callback" object.
 */

int
Tcl_EvalObjv(Tcl_Interp * interp, int objc, Tcl_Obj *CONST objv[], int flags)
{
 SV *sv = objv[0];
 int i;
 dTHX;
 dSP;
 ENTER;
 SAVETMPS;
 PUSHMARK(sp);
 for (i=1; i < objc; i++)
  {
   XPUSHs(objv[i]);
  }
 PUTBACK;
 i = LangCallCallback(sv, G_SCALAR | G_EVAL);
 SetTclResult(interp,i);
 FREETMPS;
 LEAVE;
 return Check_Eval(interp);
}

int
Tcl_GlobalEval(Tcl_Interp *interp, CONST char *command)
{
 dTHX;
 if (!PL_tainting)
  {
   warn("Receive from Tk's 'send' ignored (no taint checking)\n");
   return EXPIRE((interp,"send to non-secure perl/Tk application rejected\n"));
  }
 else
  {
   dSP;
   int count = 0;
   int old_taint = PL_tainted;
   SV *sv;
   PL_tainted = 0;
   ENTER;
   SAVETMPS;
   PUSHMARK(sp);
   Set_widget(sv = WidgetRef(interp,"."));
   XPUSHs(sv_mortalcopy(sv));
   PL_tainted = 1;
   sv = newSVpv(command,strlen(command));
   SvTAINT(sv);
   PL_tainted = 0;
   XPUSHs(sv_2mortal(sv));
   PUTBACK;
   Tcl_ResetResult(interp);
   Lang_ClearErrorInfo(interp);
   sv = sv_2mortal(newSVpv("Receive",0));
   PL_tainted = old_taint;
   count = LangCallCallback(sv, G_ARRAY | G_EVAL);
   SetTclResult(interp,count);
   FREETMPS;
   LEAVE;
   return Check_Eval(interp);
  }
}

XS(XS_Tk__Widget_BindClientMessage)
{
 dXSARGS;
 if (items >= 1)
  {
   HV *hv = NULL;
   Lang_CmdInfo *info = WindowCommand(ST(0), &hv, 2);
   if (info)
    {
     HV *cm = FindHv(aTHX_ hv, "BindClientMessage", (items > 2), CM_KEY);
     if (items >= 2)
      {
       STRLEN len;
       char *key = SvPV(ST(1),len);
       if (items > 2)
        {
         SV *cb = LangMakeCallback(ST(2));
         hv_store(cm, key, len, cb ,0);
        }
       else
        {
         if (cm)
          {
           SV **x = hv_fetch(cm, key, len, 0);
           if (x)
            ST(0) = sv_mortalcopy(*x);
          }
        }
      }
     else
      {
       if (cm)
        ST(0) = sv_2mortal(newRV((SV *) cm));
      }
    }
  }
 else
  croak("Usage: $w->BindClientMessage(atom,callback)");
 XSRETURN(1);
}

#ifdef WIN32
int
Lang_WinEvent(tkwin, message, wParam, lParam, resultPtr)
    Tk_Window tkwin;
    UINT message;
    WPARAM wParam;
    LPARAM lParam;
    LRESULT *resultPtr;
{
 dTHX;
 Tcl_Interp *interp = NULL;
 SV *w = TkToWidget(tkwin,&interp);
 char key[32];
 HV *cm = NULL;
 STRLEN na;
 int code = 0;
  if ( !interp || !w || !SvROK(w))
  {
    return 0;
  }
 sprintf(key,"%d",message);
 if (SvROK(w))
  cm = FindHv(aTHX_ (HV *) SvRV(w),"Lang_WinMessage",0,CM_KEY);
 if (cm)
  {
   SV **x = hv_fetch(cm,key,strlen(key),0);
   SV *sv;
   if (!x)
    x = hv_fetch(cm,"0",1,0);
   if (x && (sv = *x))
    {
     dSP;
     SV *data = struct_sv(NULL, sizeof(EventAndKeySym));
     EventAndKeySym *info = (EventAndKeySym *) SvPVX(data);
     int result;
#if 0
     LangDebug("%s %d '%s'\n",Tk_PathName(tkwin), message,SvPV(sv,na));
#endif
     info->keySym = 0;
     info->interp = interp;
     info->window = w;
     info->tkwin  = tkwin;
     ENTER;
     SAVETMPS;
     Tcl_ResetResult(interp);
     Lang_ClearErrorInfo(interp);
     Set_widget(w);
     result = PushObjCallbackArgs(interp,&sv,info);
     SPAGAIN;
     if (result == TCL_OK)
      {
       XPUSHs(sv_2mortal(newSViv(message)));
       XPUSHs(sv_2mortal(newSViv(wParam)));
       XPUSHs(sv_2mortal(newSViv(lParam)));
       PUTBACK;
       result = LangCallCallback(sv, G_DISCARD | G_EVAL);
       if (result)
        {
         SPAGAIN;
         sv = POPs;
         PUTBACK;
         if (SvIOK(sv))
          {
           *resultPtr = SvIV(sv);
           code = 1;
          }
        }
      }
     Lang_MaybeError(interp,Check_Eval(interp),"ClientMessage handler");
     FREETMPS;
     LEAVE;
    }
  }
 return code;
}
#endif /* WIN32 */

void
LangClientMessage(interp, tkwin, event)
Tcl_Interp *interp;
Tk_Window tkwin;
XEvent *event;
{
 dTHX;
 SV *w = TkToWidget(tkwin,NULL);
 CONST char *key;
 HV *cm = NULL;
 if (!SvROK(w))
  {
   Tk_Window mainwin = (Tk_Window)((((TkWindow*)tkwin)->mainPtr)->winPtr);
   w = TkToWidget(mainwin,NULL);
  }
 key = Tk_GetAtomName(tkwin, event->xclient.message_type);
 if (SvROK(w))
  cm = FindHv(aTHX_ (HV *) SvRV(w),"LangClientMessage",0,CM_KEY);
 if (cm)
  {
   SV **x = hv_fetch(cm,key,strlen(key),0);
   SV *sv;
   if (!x)
    x = hv_fetch(cm,"any",3,0);
   if (x && (sv = *x))
    {
     dSP;
     SV *data = struct_sv(NULL, sizeof(EventAndKeySym));
     EventAndKeySym *info = (EventAndKeySym *) SvPVX(data);
     SV *e  = Blessed("XEvent", MakeReference(data));
     int result;
     info->event  = *event;
     info->keySym = 0;
     info->interp = interp;
     info->window = w;
     info->tkwin  = tkwin;
     ENTER;
     SAVETMPS;
     Tcl_ResetResult(interp);
     Lang_ClearErrorInfo(interp);
     Set_widget(w);
     Set_event(e);
     if (SvROK(w))
      {
       HV *hash = (HV *) SvRV(w);
       hv_store(hash, XEVENT_KEY, strlen(XEVENT_KEY), e, 0);
      }
     else
      Decrement(e,"Unused Event");
     result = PushObjCallbackArgs(interp,&sv,info);
     if (result == TCL_OK)
      LangCallCallback(sv, G_DISCARD | G_EVAL);
     Lang_MaybeError(interp,Check_Eval(interp),"ClientMessage handler");
     if (0 && SvROK(w))
      {
       HV *hash = (HV *) SvRV(w);
       hv_delete(hash, XEVENT_KEY, strlen(XEVENT_KEY), G_DISCARD);
      }
     FREETMPS;
     LEAVE;
    }
#if 0
   else
    {
     warn("%s has no handler for '%s'\n",Tk_PathName(tkwin),key);
    }
#endif
  }
#if 0
 else
  {
   warn("ClientMessage '%s' for %s\n", key, Tk_PathName(tkwin));
  }
#endif
}

int
LangEventCallback(cdata, interp, event, tkwin, keySym)
ClientData cdata;
Tcl_Interp *interp;
Tk_Window tkwin;
XEvent *event;
KeySym keySym;
{
 dTHX;
 SV *sv = (SV *) cdata;
 int result = TCL_ERROR;
 Tk_Window ewin = Tk_EventWindow(event);
#ifdef LEAK_CHECKING
 hash_ptr *save = NULL;
 long hwm = note_used(&save);
 fprintf(stderr, "Event Entry count=%ld hwm=%ld\n", ec = sv_count, hwm);
#endif
 Tcl_ResetResult(interp);
 Lang_ClearErrorInfo(interp);
 if (!SvOK(sv))
  {
   Tcl_SetResult(interp,"Call of undefined callback",TCL_STATIC);
   return TCL_ERROR;
  }
 if (ewin && tkwin)
  {
   dSP;
   int code;
   SV *data = struct_sv(NULL, sizeof(EventAndKeySym));
   EventAndKeySym *info = (EventAndKeySym *) SvPVX(data);
   SV *e = Blessed("XEvent", MakeReference(data));
   SV *w = TkToWidget(tkwin,NULL);
#ifdef DEBUG_GLUE
   fprintf(stderr, "%s:%s(%s) = %p\n", "LangEventCallback", SvPV_nolen(sv), Tk_PathName(tkwin), info);
#endif
   info->event = *event;
   info->keySym = keySym;
   info->interp = interp;
   info->window = w;
   info->tkwin  = tkwin;
   ENTER;
   PUSHSTACKi(PERLSI_MAGIC);
   SAVETMPS;
   PUTBACK;
   Tcl_ResetResult(interp);
   Lang_ClearErrorInfo(interp);
   Set_widget(w);
   Set_event(e);
   result = PushObjCallbackArgs(interp,&sv,info);
   if (SvROK(w))
    {
     HV *hash = (HV *) SvRV(w);
     hv_store(hash, XEVENT_KEY, strlen(XEVENT_KEY), e, 0);
    }
   else
    Decrement(e,"Unused Event");
   if (result == TCL_OK)
    {
     LangCallCallback(sv, G_DISCARD | G_EVAL);
     FREETMPS;
     result = Check_Eval(interp);
    }
   if (0 && SvROK(w))
    {
     HV *hash = (HV *) SvRV(w);
     hv_delete(hash, XEVENT_KEY, strlen(XEVENT_KEY), G_DISCARD);
    }
   POPSTACK;
   LEAVE;
  }
 else
  {
   /*
    * Event pertains to a window which has been/is being deleted.
    * Although we may be able to call perl code we cannot make
    * any method calls because the widget hash object has probably vanished.
    *
    * Quietly return "OK" having done nothing
    */
   result = TCL_OK;
  }
#ifdef LEAK_CHECKING
 fprintf(stderr, "sv_count was %ld, now %ld (%ld)\n", ec, sv_count, sv_count - ec);
 check_used(&save);
#endif
 return result;
}

void
LangFreeArg(sv, freeProc)
Tcl_Obj * sv;
Tcl_FreeProc *freeProc;
{
 dTHX;
 Decrement(sv, "LangFreeArg");
}

static int
handle_generic(clientData, eventPtr)
ClientData clientData;
XEvent *eventPtr;
{
 int code        = 0;
 Tk_Window tkwin = Tk_EventWindow(eventPtr);
 if (tkwin)
  {
   GenericInfo *p = (GenericInfo *) clientData;
   Tcl_Interp *interp = p->interp;
   SV *sv = p->cb;
   dTHX;
   dSP;
   SV *data = struct_sv(NULL, sizeof(EventAndKeySym));
   EventAndKeySym *info = (EventAndKeySym *) SvPVX(data);
   SV *e = Blessed("XEvent", MakeReference(data));
   SV *w = NULL;
   int count = 0;
   int result;
   info->event = *eventPtr;
   info->keySym = None;
   info->interp = interp;
   info->tkwin  = tkwin;
   do_watch();
   Tcl_ResetResult(interp);
   Lang_ClearErrorInfo(interp);
   ENTER;
   SAVETMPS;
   if (tkwin)
    w = TkToWidget(tkwin,&info->interp);  /* Pending REFCNT */
   if (!SvROK(w))
    w = Blessed("Window", MakeReference(newSViv((IV) (eventPtr->xany.window))));
   else
    Set_widget(w);
   result = PushObjCallbackArgs(interp, &sv,info);
   if (result == TCL_OK)
    {
     SPAGAIN;
     Set_event(e);
     XPUSHs(sv_mortalcopy(e));
     XPUSHs(sv_mortalcopy(w));
     PUTBACK;
     count = LangCallCallback(sv, G_EVAL);
     result = Check_Eval(interp);
    }
   if (count)
    {
     SPAGAIN;
     code = TOPi;
     sp -= count;
     PUTBACK;
    }
   else
    code = 0;
   Lang_MaybeError(interp,result,"Generic Event");

   FREETMPS;
   LEAVE;
  }
 return code;
}

static void
Perl_GeomRequest(clientData,tkwin)
ClientData clientData;
Tk_Window tkwin;
{
 Lang_CmdInfo *info = (Lang_CmdInfo *) clientData;
 SV *master = TkToWidget(info->tkwin,NULL);
 SV *slave  = TkToWidget(tkwin,NULL);
 dTHX;
 dSP;
 ENTER;
 SAVETMPS;
 Set_widget(master);
 PUSHMARK(sp);
 XPUSHs(sv_mortalcopy(master));
 XPUSHs(sv_mortalcopy(slave));
 PUTBACK;
 LangCallCallback(sv_2mortal(newSVpv("SlaveGeometryRequest",0)),G_DISCARD);
 FREETMPS;
 LEAVE;
}

static void
Perl_GeomLostSlave(clientData,tkwin)
ClientData clientData;
Tk_Window tkwin;
{
 Lang_CmdInfo *info = (Lang_CmdInfo *) clientData;
 SV *master = TkToWidget(info->tkwin,NULL);
 SV *slave  = TkToWidget(tkwin,NULL);
 dTHX;
 dSP;
 ENTER;
 SAVETMPS;
 PUSHMARK(sp);
 Set_widget(master);
 XPUSHs(sv_mortalcopy(master));
 XPUSHs(sv_mortalcopy(slave));
 PUTBACK;
 LangCallCallback(sv_2mortal(newSVpv("LostSlave",0)),G_DISCARD);
 FREETMPS;
 LEAVE;
}

XS(XS_Tk__Widget_ManageGeometry)
{
 dXSARGS;
 STRLEN na;
 if (items == 2)
  {
   HV *hash = NULL;
   Lang_CmdInfo *info   = WindowCommand(ST(0), &hash, 0);
   if (info && info->tkwin)
    {
     Lang_CmdInfo *slave  = WindowCommand(ST(1), NULL, 0);
     if (slave && slave->tkwin)
      {
       SV **x = hv_fetch(hash,GEOMETRY_KEY,strlen(GEOMETRY_KEY),0);
       SV *mgr_sv = NULL;
       if (!x)
        {
         Tk_GeomMgr mgr;
         mgr.name          = Tk_PathName(info->tkwin);
         mgr.requestProc   = Perl_GeomRequest;
         mgr.lostSlaveProc = Perl_GeomLostSlave;
         mgr_sv = struct_sv((char *) &mgr,sizeof(mgr));
         hv_store(hash,GEOMETRY_KEY,strlen(GEOMETRY_KEY),mgr_sv, 0);
        }
       else
        mgr_sv = *x;
       Tk_ManageGeometry(slave->tkwin, (Tk_GeomMgr *) SvPV(mgr_sv,na), (ClientData) info);
      }
     else
      croak("Not a (slave) widget %s",SvPV(ST(1),na));
    }
   else
    croak("Not a (master) widget %s",SvPV(ST(0),na));
  }
 else
  croak("usage $master->ManageGeometry($slave)");
 XSRETURN(1);
}

static void
handle_idle(clientData)
ClientData clientData;
{
 dTHX;
 GenericInfo *p = (GenericInfo *) clientData;
 SV *sv = p->cb;
 dSP;
 int count = 0;
 int code = 0;
 ENTER;
 SAVETMPS;
 Tcl_ResetResult(p->interp);
 Lang_ClearErrorInfo(p->interp);
 Set_widget(WidgetRef(p->interp,"."));
 code = PushCallbackArgs(p->interp,&sv);
 if (code == TCL_OK)
  {
   LangCallCallback(sv, G_DISCARD | G_EVAL);
   code = Check_Eval(p->interp);
  }
 Lang_MaybeError(p->interp,code,"Idle Callback");
 FREETMPS;
 LEAVE;
 LangFreeCallback(p->cb);
 DecInterp(p->interp, "handle_idle");
 ckfree((char *) p);
}


XS(XS_Tk_DoWhenIdle)
{
 dXSARGS;
 STRLEN na;
 if (items == 2)
  {
   Lang_CmdInfo *info = WindowCommand(ST(0), NULL, 0);
   if (info && info->interp && (info->tkwin || info->image))
    {
     /* Try to get result to prove things are "still alive" */
     if (Tcl_GetObjResult(info->interp))
      {
       GenericInfo *p = (GenericInfo *) ckalloc(sizeof(GenericInfo));
       IncInterp(info->interp,"Tk_DoWhenIdle");
       p->interp = info->interp;
       p->cb = LangMakeCallback(ST(1));
       Tcl_DoWhenIdle(handle_idle, (ClientData) p);
      }
    }
   else
    croak("Not a widget %s",SvPV(ST(0),na));
  }
 else
  croak("Usage $w->DoWhenIdle(callback)");
 XSRETURN(1);
}

XS(XS_Tk_CreateGenericHandler)
{
 dXSARGS;
 STRLEN na;
 if (items == 2)
  {
   Lang_CmdInfo *info = WindowCommand(ST(0), NULL, 0);
   if (info && info->interp && (info->tkwin || info->image))
    {
     if (Tcl_GetObjResult(info->interp))
      {
       GenericInfo *p = (GenericInfo *) ckalloc(sizeof(GenericInfo));
       IncInterp(info->interp,"Tk_CreateGenericHandler");
       p->interp = info->interp;
       p->cb = LangMakeCallback(ST(1));
       Tk_CreateGenericHandler(handle_generic, (ClientData) p);
      }
    }
   else
    croak("Not a widget %s",SvPV(ST(0),na));
  }
 else
  croak("Usage $w->CreateGenericHandler(callback)");
 XSRETURN(1);
}


SV *
XEvent_Info(obj,s)
EventAndKeySym *obj;
char *s;
{
 dTHX;
 SV *eventSv = sv_newmortal();
 I32 ix = (I32) *s;
 char scratch[256];
 if (obj)
  {
   if (ix == '@' || strncmp(s,"xy",2) == 0)
    {
     char result[80];
     strcpy(result, "@");
     strcat(result, Tk_EventInfo('x', obj->tkwin, &obj->event, obj->keySym, NULL, NULL, NULL, sizeof(scratch) - 1, scratch));
     strcat(result, ",");
     strcat(result, Tk_EventInfo('y', obj->tkwin, &obj->event, obj->keySym, NULL, NULL, NULL, sizeof(scratch) - 1, scratch));
     sv_setpv(eventSv, result);
    }
   else
    {
     int isNum = 0;
     int number = 0;
     int type = TK_EVENTTYPE_NONE;
     char *result = Tk_EventInfo(ix, obj->tkwin, &obj->event, obj->keySym, &number, &isNum, &type, sizeof(scratch) - 1, scratch);
     switch (type)
      {
       case TK_EVENTTYPE_WINDOW:
        {
         SV *w = &PL_sv_undef;
         if (result && result[0] == '.')
          w = WidgetRef(obj->interp, result);
         if (SvROK(w))
          SvSetMagicSV(eventSv, w);
         else
          {
           if (number)
            sv_setref_iv(eventSv, "Window", number);
          }
        }
        break;

       case TK_EVENTTYPE_DISPLAY:
        sv_setref_pv(eventSv, "DisplayPtr", (void *) number);
        break;

       case TK_EVENTTYPE_DATA:
        sv_setpvn(eventSv, result, (unsigned) number);
        break;

       default:
        if (result) {
         sv_setpv(eventSv, result);
	}
        if (isNum)
         {
          sv_setiv(eventSv, number);
          if (result)
           SvPOK_on(eventSv);
         }
        break;
      }
    }
  }
 return sv_maybe_utf8(eventSv);
}

EventAndKeySym *
SVtoEventAndKeySym(SV *arg)
{
 dTHX;
 SV *sv;
 if (sv_isobject(arg) && (sv = SvRV(arg)) &&
     SvPOK(sv) && SvCUR(sv) == sizeof(EventAndKeySym))
  {
   return (EventAndKeySym *) SvPVX(sv);
  }
 else
  croak("obj is not an XEvent");
 return NULL;
}

XS(XS_Tk__Widget_PassEvent)
{
 dXSARGS;
 Tk_Window tkwin = NULL;
 EventAndKeySym *obj = NULL;
 if (items == 2
     && (tkwin = (Tk_Window) SVtoWindow(ST(0)))
     && (obj = SVtoEventAndKeySym(ST(1)))
    )
  {
   if (Tk_WindowId(tkwin) == None)
    Tk_MakeWindowExist(tkwin);
   TkBindEventProc((TkWindow *)tkwin, &obj->event);
  }
 else
  croak("Usage: $widget->PassEvent($event)");
 ST(0) = &PL_sv_undef;
 XSRETURN(1);
}


void
Tk_ChangeScreen(interp, dispName, screenIndex)
Tcl_Interp *interp;
char *dispName;
int screenIndex;
{

}


/* These are for file name handling which needs further abstraction */

char *
Tcl_TranslateFileName(interp, name, bufferPtr)
Tcl_Interp *interp;
CONST char *name;
Tcl_DString *bufferPtr;
{
 dTHX;
 dSP;
 IV count;
 ENTER;
 SAVETMPS;
 PUSHMARK(sp);
 XPUSHs(sv_2mortal(newSVpv((char *) name,0)));
 PUTBACK;
 perl_call_pv("Tk::TranslateFileName",G_EVAL|G_SCALAR);
 SPAGAIN;
 *bufferPtr = POPs;
 PUTBACK;
 SvREFCNT_inc(*bufferPtr);
 FREETMPS;
 LEAVE;
 return Tcl_DStringValue(bufferPtr);
}

CONST char *
Tcl_PosixError(interp)
Tcl_Interp *interp;
{
 dTHX;
 return Strerror(errno);
}

#ifdef STANDARD_C
void
EnterWidgetMethods(char *package,...)
#else
/*VARARGS0 */
void
EnterWidgetMethods(package, va_alist)
char *package;
va_dcl
#endif
{
 dTHX;
 va_list ap;
 char buf[80];
 char *method;
#ifdef I_STDARG
 va_start(ap, package);
#else
 va_start(ap);
#endif
 while ((method = va_arg(ap, char *)))
  {
   CV *cv;
   if (strcmp(method, "configure") && strcmp(method, "cget"))
    {
     sprintf(buf, "Tk::%s::%s", package, method);
     cv = newXS(buf, XStoWidget, __FILE__);
     CvXSUBANY(cv).any_ptr = newSVpv(method, 0);
    }
  }
}

void
Lang_SetErrorCode(interp, code)
Tcl_Interp *interp;
char *code;
{

}

void
Tcl_SetObjErrorCode (Tcl_Interp * interp,Tcl_Obj * errorObjPtr)
{

}

char *
Lang_GetErrorCode(interp)
Tcl_Interp *interp;
{
 warn("Lang_GetErrorCode not implemented");
 return "";
}

char *
Lang_GetErrorInfo(interp)
Tcl_Interp *interp;
{
 warn("Lang_GetErrorInfo not implemented");
 return "";
}

void
LangBadFile(fd)
int fd;
{
 warn("File (%d) closed without deleting handler",fd);
}

int
LangEventHook(flags)
int flags;
/* Used by Tcl_Async stuff for signal handling */
{
#if 0
#if defined(WNOHANG) && (defined(HAS_WAITPID) || defined(HAS_WAIT4))
 int status = -1;
 I32 pid = wait4pid(-1,&status,WNOHANG);
 if (pid > 0)
  {
   pidgone(pid, status);
   warn("Child process %d status=%d",pid,status);
   return 1;
  }
#endif
#endif
 return 0;
}

/* Tcl caches compiled regexps so does not free them */

struct WrappedRegExp
{
#if HAS_PMOP_EXTRA_FLAGS
 PMOP   op;
#else
 U32 flags;
#endif
#if USE_REGEXP_511
 REGEXP *pat;
#else
 regexp *pat;
#endif
 SV    *source;
};

void
Lang_FreeRegExp(re)
Tcl_RegExp re;
{
 dTHX;
 if (re->pat)
  ReREFCNT_dec(re->pat);
 if (re->source)
  SvREFCNT_dec(re->source);
 Safefree(re);
}

/* An "XS" routine to call with G_EVAL set */
static void
do_comp(pTHX_ CV *cv)
{
 dMARK;
 dAX;
 struct WrappedRegExp *p = (struct WrappedRegExp *) CvXSUBANY(cv).any_ptr;
#if USE_PREGCOMP_31027
 p->pat = pregcomp(p->source,p->flags);
#else /* USE_PREGCOMP_31027 */
 int len = 0;
 char *string = Tcl_GetStringFromObj(p->source,&len);

#if HAS_PMOP_EXTRA_FLAGS
 p->op.op_pmdynflags |= PMdf_DYN_UTF8;
 p->pat = pregcomp(string,string+len,&p->op);
#else
 p->pat = pregcomp(string,string+len,p->flags);
#endif
#if 0
 LangDebug("/%.*s/ => %p\n",len,string,p->pat);
#endif
#endif /* USE_PREGCOMP_31027 */
 XSRETURN(0);
}

I32
Lang_catch(pTHX_ XSUBADDR_t subaddr, void *any, I32 flags,char *filename)
{
 dSP;
 CV *cv = (CV *) sv_newmortal();
 int count;
 SV **oldSP = sp;
 sv_upgrade((SV *)cv, SVt_PVCV);
 CvFILE(cv) = filename;
 CvXSUB(cv) = subaddr;
 CvXSUBANY(cv).any_ptr = any;
#ifdef CvISXSUB_on
 CvISXSUB_on(cv); /* this is needed for perl5.9@27244 */
#endif
 count = call_sv((SV *)cv,flags|G_EVAL);
 SPAGAIN;
 if (sp != oldSP)
  {
   LangDebug("Stack moved %p => %p\n",oldSP,sp);
  }
 return count;
}

Tcl_RegExp
Tcl_GetRegExpFromObj(Tcl_Interp *interp, Tcl_Obj *obj, int flags)
{
 dTHX;
 Tcl_RegExp re;
 MAGIC *mg = Null(MAGIC*);

 Newz('R', re, 1, struct WrappedRegExp);

 re->source = Tcl_DuplicateObj(obj);

 /* If source is a reference and thing refrered to has right
    magic we can use regexp from the qr//
  */
 if (SvROK(re->source)) {
	SV *sv = SvRV(re->source);
	if(SvMAGICAL(sv))
	    mg = mg_find(sv, PERL_MAGIC_qr);
 }

#if HAS_PMOP_EXTRA_FLAGS
 /* Could do more conversions here
    Not sure how/if to override case-ness of qr// pattern
  */
 if (flags & TCL_REG_NOCASE) {
   re->op.op_pmflags |= PMf_FOLD;
 }
#else
#if USE_REGEXP_511
 re->flags =            (flags & TCL_REG_NOCASE ? RXf_PMf_FOLD : 0);
#else
 re->flags = RXf_UTF8 | (flags & TCL_REG_NOCASE ? RXf_PMf_FOLD : 0);
#endif
#endif

 if (mg)
  {
#if USE_REGEXP_511
   re->pat = (REGEXP *)mg->mg_obj;
#else
   re->pat = (regexp *)mg->mg_obj;
#endif
   /* Guess wildly ... */
   ReREFCNT_inc(re->pat);
  }
 else
  {
   dSP;
   SV *err;
   ENTER;
   SAVETMPS;
   PUSHMARK(sp);
   Lang_catch(aTHX_ do_comp, re, G_VOID, __FILE__);
   FREETMPS;
   LEAVE;
   err = ERRSV;
   if (SvTRUE(err))
    {
     Lang_FreeRegExp(re);
     Tcl_SetResult(interp,SvPV_nolen(err),TCL_VOLATILE);
     return NULL;
    }
  }
 return re;
}

int
Tcl_RegExpExec(interp, re, cstring, cstart)
Tcl_Interp *interp;
Tcl_RegExp re;
CONST char *cstring;
CONST char *cstart;
{
 dTHX;
 SV *tmp = sv_newmortal();
 int code;
 sv_upgrade(tmp,SVt_PV);
 SvCUR_set(tmp,strlen(cstring));
 SvPVX(tmp) = (char *) cstring;
 SvLEN(tmp) = 0;
 SvREADONLY_on(tmp);
 SvPOK_on(tmp);
 /* From Tk all strings are UTF-8 */
 SvUTF8_on(tmp);
#ifdef ROPT_MATCH_UTF8
 RX_MATCH_UTF8_on(re->pat);
#else
 /* eeek what do we do now ... */
#endif
 code = pregexec(re->pat,SvPVX(tmp),SvEND(tmp),(char *) cstart,0,
                 tmp,REXEC_COPY_STR);
#if 0
 LangDebug("%d '%.*s'\n",code,SvCUR(tmp),SvPVX(tmp));
 sv_dump(tmp);
 regdump(re->pat);
#endif
 return code;
}

void
Tcl_RegExpRange(wrap, index, startPtr, endPtr)
Tcl_RegExp wrap;
int index;
CONST84 char **startPtr;
CONST84 char **endPtr;
{
#if USE_REGEXP_511
 REGEXP *rx = wrap->pat;
 regexp *const re = (struct regexp *)SvANY(rx);
#else
 regexp *re = wrap->pat;
#endif
#if USE_NEWSTYLE_REGEXP_STRUCT
 if (re->offs[index].start != -1 && re->offs[index].end != -1)
  {
   *startPtr = re->subbeg+re->offs[index].start;
   *endPtr   = re->subbeg+re->offs[index].end;
  }
#else
 if (re->startp[index] != -1 && re->endp[index] != -1)
  {
   *startPtr = re->subbeg+re->startp[index];
   *endPtr   = re->subbeg+re->endp[index];
  }
#endif
 else
  {
   *startPtr = NULL;
   *endPtr   = NULL;
  }
}

void
Lang_BuildInImages()
{
#if 0
	Tk_CreateImageType(&tkBitmapImageType);
	Tk_CreateImageType(&tkPixmapImageType);
	Tk_CreateImageType(&tkPhotoImageType);

	/*
	 * Create built-in photo image formats.
	 */

	Tk_CreatePhotoImageFormat(&tkImgFmtPPM);
#endif
}


ClientData
Tcl_GetAssocData(interp,name,procPtr)
Tcl_Interp *interp;
CONST char *name;
Tcl_InterpDeleteProc **procPtr;
{
 dTHX;
 HV *cm = FindHv(aTHX_ interp, "Tcl_GetAssocData", 0, ASSOC_KEY);
 SV **x  = hv_fetch(cm, name, strlen(name), 0);
 if (x)
  {
   STRLEN sz;
   Assoc_t *info = (Assoc_t *) SvPV(*x,sz);
   if (sz != sizeof(*info))
    croak("%s corrupted",ASSOC_KEY);
   if (procPtr)
    *procPtr = info->proc;
   return info->clientData;
  }
 return NULL;
}

void
Tcl_SetAssocData(interp,name,proc,clientData)
Tcl_Interp *interp;
CONST char *name;
Tcl_InterpDeleteProc *proc;
ClientData clientData;
{
 dTHX;
 HV *cm = FindHv(aTHX_ interp, "Tcl_SetAssocData", 1, ASSOC_KEY);
 Assoc_t info;
 SV *d;
 info.proc = proc;
 info.clientData = clientData;
 d = struct_sv((char *) &info,sizeof(info));
 hv_store(cm,name,strlen(name),d,0);
}

#define MkXSUB(str,name,xs,proc)                  \
extern XSdec(name);                               \
XS(name)                                          \
{                                                 \
 CvXSUB(cv) = xs;                                 \
 CvXSUBANY(cv).any_ptr = (VOID *) proc;           \
 xs(aTHX_ cv);                                    \
}
#include "TkXSUB.def"
#undef MkXSUB


void
install_vtab(name, table, size)
char *name;
void *table;
size_t size;
{
 dTHX;
 if (table)
  {
   typedef unsigned (*fptr)_((void));
   fptr *q = table;
   unsigned i;
   if ((*q[0])() != size)
    {
     croak("%s table is %u not %u",name,(*q[0])(),(unsigned) size);
    }
   sv_setiv(FindTkVarName(name,GV_ADD|GV_ADDMULTI),PTR2IV(table));
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



XS(XS_Tk_INIT)
{
 dXSARGS;
 InitVtabs();
 XSRETURN_EMPTY;
}

void
Boot_Glue
_((pTHX))
{
 dSP;
 /* A wonder how you call $e-># ? */
 char *XEventMethods = "abcdfhkmopstvwxyABDEKNRSTWXY#";
 char buf[128];
 CV *cv;
#if PERL_REVISION > 5 || (PERL_REVISION == 5 && PERL_VERSION >= 9)
#define COP_WARNINGS_TYPE STRLEN*
#else
#define COP_WARNINGS_TYPE SV*
#endif
#ifdef pWARN_NONE
 COP_WARNINGS_TYPE old_warn = PL_curcop->cop_warnings;
 PL_curcop->cop_warnings = pWARN_NONE;
#endif

 /* Arrange to call initialization code - an XSUB called INIT */
 cv = newXS("Tk::INIT", XS_Tk_INIT, __FILE__);

#ifdef pWARN_NONE
 PL_curcop->cop_warnings = old_warn;
#endif

 initialized = 0;
 InitVtabs();

#ifdef VERSION
 sprintf(buf, "%s::VERSION", BASEEXT);
 sv_setpv(perl_get_sv(buf,1),VERSION);
#endif

 sprintf(buf, "%s::Widget::%s", BASEEXT, "BindClientMessage");
 cv = newXS(buf, XS_Tk__Widget_BindClientMessage, __FILE__);

 sprintf(buf, "%s::Widget::%s", BASEEXT, "PassEvent");
 cv = newXS(buf, XS_Tk__Widget_PassEvent, __FILE__);

 sprintf(buf, "%s::Widget::%s", BASEEXT, "SelectionGet");
 cv = newXS(buf, XS_Tk__Widget_SelectionGet, __FILE__);

 cv = newXS("Tk::MainWindow::Create", XS_Tk__MainWindow_Create, __FILE__);


 newXS("Tk::DoWhenIdle", XS_Tk_DoWhenIdle, __FILE__);
 newXS("Tk::CreateGenericHandler", XS_Tk_CreateGenericHandler, __FILE__);


 sprintf(buf, "%s::Widget::%s", BASEEXT, "ManageGeometry");
 cv = newXS(buf, XS_Tk__Widget_ManageGeometry, __FILE__);

 cv = newXS("Tk::Interp::DESTROY", XS_Tk__Interp_DESTROY, __FILE__);

#define MkXSUB(str,name,xs,proc) \
 newXS(str, name, __FILE__);
#include "TkXSUB.def"
#undef MkXSUB

 Tk_CreateImageType(&tkPhotoImageType);
 Tk_CreatePhotoImageFormat(&tkImgFmtPPM);
#if 0
 Tk_CreatePhotoImageFormat(&imgFmtGIF);
#else
 Tk_CreatePhotoImageFormat(&tkImgFmtGIF);
#endif
 Tk_CreatePhotoImageFormat(&imgFmtXBM);
 Tk_CreatePhotoImageFormat(&imgFmtXPM);
 Tk_CreatePhotoImageFormat(&imgFmtBMP);
}

void
Tcl_AllowExceptions (Tcl_Interp *interp)
{
 /* FIXME: What should this do ? */
}


static HV *uidHV;

Tk_Uid
Tk_GetUid(key)
    CONST char *key;		/* String to convert. */
{
    dTHX;
    STRLEN klen;
    SV *svkey = newSVpv((char *)key,strlen(key));
    HE *he;
    if (!uidHV)
     uidHV = newHV();
    he = hv_fetch_ent(uidHV,svkey,0,0); /* added by SRT: prevents leak of auto-created SVs */
    if (!he)
     he = hv_store_ent(uidHV,svkey,Nullsv,0); /* ... */
    SvREFCNT_dec(svkey);
    return (Tk_Uid) HePV(he,klen);
}


Tcl_Obj*
Tcl_FSGetCwd(interp)
    Tcl_Interp *interp;
{
 dTHX;
 dSP;
 SV *ret = Nullsv;
 ENTER;
 SAVETMPS;
 PUSHMARK(sp);
 PUTBACK;
 if (call_pv("Cwd::getcwd",G_SCALAR) == 1)
  {
   SPAGAIN;
   ret = POPs;
   PUTBACK;
   SvREFCNT_inc(ret);
  }
 else
  {
   SPAGAIN;
  }
 FREETMPS;
 LEAVE;
 return ret;
}


char *
Tcl_GetCwd(interp, cwdPtr)
    Tcl_Interp *interp;
    Tcl_DString *cwdPtr;
{
    Tcl_Obj *cwd;
    cwd = Tcl_FSGetCwd(interp);
    if (cwd == NULL) {
	return NULL;
    } else {
	Tcl_DStringInit(cwdPtr);
	Tcl_DStringAppend(cwdPtr, Tcl_GetString(cwd), -1);
	Tcl_DecrRefCount(cwd);
	return Tcl_DStringValue(cwdPtr);
    }
}

void
LangSelectHook(CONST char *what,Tk_Window tkwin,
               Atom selection, Atom target, Atom type)
{
#if 0
 /* There is still something not-quite-right about Selection
    but we don't want all this noise in the release
  */
 TkWindow *winPtr = (TkWindow *)tkwin;
 char *name = (tkwin == winPtr->dispPtr->clipWindow)
              ? "ClipWindow" : Tk_PathName(tkwin);
 LangDebug("%s sel=%s target=%s type=%s win=%p '%s'\n", what,
	      Tk_GetAtomName(tkwin, selection),
	      Tk_GetAtomName(tkwin, target),
	      (type == None) ? "None" : Tk_GetAtomName(tkwin, type),
              tkwin, name);
#endif
}


