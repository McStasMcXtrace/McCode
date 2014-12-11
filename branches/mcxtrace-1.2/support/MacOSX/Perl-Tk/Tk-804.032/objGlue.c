/*
  Copyright (c) 1997-2004 Nick Ing-Simmons. All rights reserved.
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

static int
Expire(int code)
{
 return code;
}

int
has_highbit(CONST char *s,int l)
{
 CONST char *e = s+l;
 while (s < e)
  {
   if (*s++ & 0x80)
    return 1;
  }
 return 0;
}

SV *
sv_maybe_utf8(SV *sv)
{
#ifdef SvUTF8_on
 if (SvPOK(sv))
  {
   if (has_highbit(SvPVX(sv),SvCUR(sv)))
    SvUTF8_on(sv);
  }
#endif
 return sv;
}

#define EXPIRE(args) \
  ( Tcl_SprintfResult args, Expire(TCL_ERROR) )

/*
 * This file maps Tcl_Obj * onto perl's SV *
 * They are very similar.
 * One area of worry is that Tcl_Obj are created with refCount = 0,
 * while SV's have SvREFCNT == 1
 * None the less normal idiom is
 *
 *   Tcl_Obj *obj = Tcl_NewFooObj(...)
 *   ...
 *   Tcl_DecrRefCount(obj)
 *
 * So difference should be transparent.
 *
 * Also :
 *
 *   Tcl_Obj *obj = Tcl_NewFooObj(...)
 *   Tcl_ListAppendElement(list,obj);
 *
 * Again this is consistent with perl's assumption that refcount is 1
 * and that av_push() does not increment it.
 *
 */

int
Tcl_IsShared(Tcl_Obj *objPtr)
{
 return SvREFCNT(objPtr) > 1;
}

void
Tcl_IncrRefCount(Tcl_Obj *objPtr)
{
 dTHX;
 SvREFCNT_inc(objPtr);
}

void
Tcl_DecrRefCount(Tcl_Obj *objPtr)
{
 dTHX;
 SvREFCNT_dec(objPtr);
}

static SV *ForceScalar(pTHX_ SV *sv);

static SV *ForceScalarLvalue(pTHX_ SV *sv);

static void
Scalarize(pTHX_ SV *sv, AV *av)
{
 int n    = av_len(av)+1;
 if (n == 0)
  sv_setpvn(sv,"",0);
 else
  {
   SV **svp;
   if (n == 1 && (svp = av_fetch(av, 0, 0)))
    {
     STRLEN len = 0;
     char *s  = SvPV(*svp,len);
#ifdef SvUTF8
     int utf8 = SvUTF8(*svp);
     sv_setpvn(sv,s,len);
     if (utf8)
      SvUTF8_on(sv);
#else
     sv_setpvn(sv,s,len);
#endif
    }
   else
    {
     Tcl_DString ds;
     int i;
     Tcl_DStringInit(&ds);
     for (i=0; i < n; i++)
      {
       if ((svp = av_fetch(av, i, 0)))
        {
         SV *el = *svp;
         int temp = 0;
         if (SvROK(el) && !SvOBJECT(SvRV(el)) && SvTYPE(SvRV(el)) == SVt_PVAV)
          {
           el = newSVpv("",0);
           temp = 1;
           if ((AV *) SvRV(*svp) == av)
            abort();
           Scalarize(aTHX_ el,(AV *) SvRV(*svp));
          }
         Tcl_DStringAppendElement(&ds,Tcl_GetString(el));
         if (temp)
          SvREFCNT_dec(el);
        }
      }
     sv_setpvn(sv,Tcl_DStringValue(&ds), Tcl_DStringLength(&ds));
     sv_maybe_utf8(sv);
     Tcl_DStringFree(&ds);
    }
  }
}

static SV *
ForceScalar(pTHX_ SV *sv)
{
 if (SvGMAGICAL(sv))
  mg_get(sv);
 if (SvTYPE(sv) == SVt_PVAV)
  {
   AV *av = (AV *) sv;
   SV *newsv = newSVpv("",0);
   Scalarize(aTHX_ newsv, (AV *) av);
   av_clear(av);
   av_store(av,0,newsv);
   return newsv;
  }
 else
  {
   if (SvROK(sv) && !SvOBJECT(SvRV(sv)) && SvTYPE(SvRV(sv)) == SVt_PVAV)
    {
     /* Callbacks and lists often get stringified by mistake due to
        Tcl/Tk's string fixation - don't change the real value
      */
     SV *newsv = newSVpv("",0);
     Scalarize(aTHX_ newsv, (AV *) SvRV(sv));
     return sv_2mortal(newsv);
    }
   else if (!SvOK(sv))
    {
     /* Map undef to null string */
     if (SvREADONLY(sv))
      {
       SV *newsv = newSVpv("",0);
       return sv_2mortal(newsv);
      }
     else
      sv_setpvn(sv,"",0);
    }
   return sv;
  }
}

static SV *
ForceScalarLvalue(pTHX_ SV *sv)
{
 if (SvTYPE(sv) == SVt_PVAV)
  {
   AV *av = (AV *) sv;
   SV *newsv = newSVpv("",0);
   av_clear(av);
   av_store(av,0,newsv);
   return newsv;
  }
 else
  {
   return sv;
  }
}

void
Tcl_SetBooleanObj (Tcl_Obj *objPtr, int value)
{
 dTHX;
 sv_setiv(ForceScalarLvalue(aTHX_ objPtr),value != 0);
}

void
Tcl_SetDoubleObj (Tcl_Obj *objPtr, double value)
{
 dTHX;
 sv_setnv(ForceScalarLvalue(aTHX_ objPtr),value);
}

void
Tcl_SetIntObj (Tcl_Obj *objPtr, int value)
{
 dTHX;
 sv_setiv(ForceScalarLvalue(aTHX_ objPtr),value);
}

void
Tcl_SetLongObj (Tcl_Obj *objPtr, long value)
{
 dTHX;
 sv_setiv(ForceScalarLvalue(aTHX_ objPtr),value);
}

void
Tcl_SetStringObj (Tcl_Obj *objPtr, CONST char *bytes, int length)
{
 dTHX;
 if (length < 0)
  length = strlen(bytes);
 objPtr = ForceScalarLvalue(aTHX_ objPtr);
 sv_setpvn(objPtr, bytes, length);
 sv_maybe_utf8(objPtr);
}

int
Tcl_GetLongFromObj (Tcl_Interp *interp, Tcl_Obj *obj, long *longPtr)
{
 dTHX;
 SV *sv = ForceScalar(aTHX_ obj);
 if (SvIOK(sv) || looks_like_number(sv))
  *longPtr = SvIV(sv);
 else
  {
   *longPtr = 0;
   return EXPIRE((interp, "'%s' isn't numeric", SvPVX(sv)));
  }
 return TCL_OK;
}

int
Tcl_GetBooleanFromObj (Tcl_Interp *interp, Tcl_Obj *obj, int *boolPtr)
{
 dTHX;
 SV *sv = ForceScalar(aTHX_ obj);
 static char *yes[] = {"y", "yes", "true", "on", NULL};
 static char *no[] =  {"n", "no", "false", "off", NULL};
 if (SvPOK(sv))
  {
   STRLEN na;
   char *s = SvPV(sv, na);
   char **p = yes;
   while (*p)
    {
     if (!strcasecmp(s, *p++))
      {
       *boolPtr = 1;
       return TCL_OK;
      }
    }
   p = no;
   while (*p)
    {
     if (!strcasecmp(s, *p++))
      {
       *boolPtr = 0;
       return TCL_OK;
      }
    }
  }
 *boolPtr = SvTRUE(sv);
 return TCL_OK;
}

int
Tcl_GetIntFromObj (Tcl_Interp *interp, Tcl_Obj *obj, int *intPtr)
{
 dTHX;
 SV *sv = ForceScalar(aTHX_ obj);
 if (SvIOK(sv) || looks_like_number(sv))
  *intPtr = SvIV(sv);
 else
  {
   *intPtr = 0;
   return EXPIRE((interp, "'%s' isn't numeric", SvPVX(sv)));
  }
 return TCL_OK;
}

int
Tcl_GetDoubleFromObj (Tcl_Interp *interp, Tcl_Obj *obj, double *doublePtr)
{
 dTHX;
 SV *sv = ForceScalar(aTHX_ obj);
 if (SvNOK(sv) || looks_like_number(sv))
  *doublePtr = SvNV(sv);
 else
  {
   *doublePtr = 0;
   return EXPIRE((interp, "'%s' isn't numeric", SvPVX(sv)));
  }
 return TCL_OK;
}

Tcl_Obj *
Tcl_NewIntObj (int value)
{
 dTHX;
 return newSViv(value);
}

Tcl_Obj *
Tcl_NewBooleanObj (int value)
{
 dTHX;
 return newSViv(value);
}

Tcl_Obj *
Tcl_NewObj(void)
{
 dTHX;
 return newSVsv(&PL_sv_undef);
}

Tcl_Obj *
Tcl_NewLongObj(long value)
{
 dTHX;
 return newSViv(value);
}

Tcl_Obj *
Tcl_NewDoubleObj(double value)
{
 dTHX;
 return newSVnv(value);
}

Tcl_Obj *
Tcl_NewStringObj (CONST char *bytes, int length)
{
 dTHX;
 if (bytes)
  {
   SV *sv;
   if (length < 0)
    length = strlen(bytes);
   sv = newSV(length);
   sv_setpvn(sv,(char *)bytes,length);
   return sv_maybe_utf8(sv);
  }
 else
  return &PL_sv_undef;
}

Tcl_Obj *
Tcl_NewListObj (int objc, Tcl_Obj *CONST objv[])
{
 dTHX;
 AV *av = newAV();
 if (objc)
  {
   while (objc-- > 0)
    {
     SV *sv = objv[objc];
     if (sv)
      {
       /* tkConfig.c passes Tcl_NewStringObj() or LangSetDefault()
          so REFCNT should be ok as-is
        */
       if (SvREFCNT(sv) <= 0 || SvTEMP(sv))
        {
         LangDebug("%s %d:\n",__FUNCTION__, objc);
         sv_dump(sv);
        }
       av_store(av,objc,sv);
      }
    }
  }
 return MakeReference((SV *) av);
}

static char * LangString(SV *sv);

/*
 * Workaround for http://rt.cpan.org/Public/Bug/Display.html?id=41436
 * This seems to be necessary for perl < 5.10.0 and if a magic
 * readonly variable like $1 is about to be utf8-ified, and only for
 * bytes >= 0x80 and <= 0xff
 *
 */
static char *
FixBuggyUTF8String(SV *sv)
{
 dTHX;
 char* s = NULL;
 if (SvREADONLY(sv))
  {
   STRLEN len = 0;
   SvREADONLY_off(sv);
   (void) SvPV_force(sv,len);
   s = LangString(sv);
   SvREADONLY_on(sv);
  }
 else
  {
   LangDebug("%s @ %d not utf8 and cannot be fixed\n",__FUNCTION__,__LINE__);
   sv_dump(sv);
   abort();
  }
 return s;
}

static char *
LangString(SV *sv)
{
 dTHX;
 if (!sv)
  return "";
 if (SvGMAGICAL(sv)) mg_get(sv);
 if (SvPOK(sv))
  {
   if (!SvUTF8(sv))
    sv_utf8_upgrade(sv);
   return SvPV_nolen(sv);
  }
 else
  {
   if (SvROK(sv))
    {
     SV *rv = SvRV(sv);
     STRLEN len;
     char *s;
     if (SvOBJECT(rv))
      {
       /* Special case "our" objects and certainb legacy hacks ... */
       if (SvTYPE(rv) == SVt_PVHV)
        {
         SV **p = hv_fetch((HV *) rv,"_TkValue_",9,0);
         if (p)
          {
           return SvPV_nolen(*p);
          }
         else
          {
           Lang_CmdInfo *info = WindowCommand(sv, NULL, 0);
           if (info)
            {
             if (info->tkwin)
              {
               char *val = Tk_PathName(info->tkwin);
               hv_store((HV *) rv,"_TkValue_",9,Tcl_NewStringObj(val,strlen(val)),0);
               return val;
              }
             if (info->image)
              {
               return SvPV_nolen(info->image);
              }
            }
          }
        }
       else if (SvPOK(rv))
        {
         /* ref to string is special cased for some reason ? */
         if (!SvUTF8(rv))
          sv_utf8_upgrade(rv);
         return SvPV_nolen(rv);
        }
      } /* Object */
     s = SvPV(sv, len);
     if (!is_utf8_string(s,len))
      {
       sv_setpvn(sv,s,len);
       sv_utf8_upgrade(sv);
       s = SvPV(sv, len);
      }
     if (!is_utf8_string(s,len))
      {
       LangDebug("%s @ %d not utf8 '%.*s'\n",__FUNCTION__,__LINE__,(int) len, s);
       sv_dump(sv);
       abort();
      }
     return s;
    } /* reference */
   else if (SvOK(sv))
    {
     if (SvROK(sv) && SvPOK(SvRV(sv)) && !SvUTF8(SvRV(sv)))
      sv_utf8_upgrade(SvRV(sv));
     else if (SvPOKp(sv) && !SvPOK(sv))
      {
       if (SvTYPE(sv) == SVt_PVLV && !SvUTF8(sv))
        {
         /* LVs e.g. substr() don't upgrade */
         SV *copy = newSVsv(sv);
         sv_utf8_upgrade(copy);
         sv_setsv(sv,copy);
         SvREFCNT_dec(copy);
        }
       else
        {
         /* Slaven's for magical (tied) SVs with only SvPOKp */
         SvPOK_on(sv);
         sv_utf8_upgrade(sv);
         SvPOK_off(sv);
         SvPOKp_on(sv);
        }
      }
     return SvPVutf8_nolen(sv);
    }
   else
    return "";
  }
}

char *
Tcl_GetStringFromObj (Tcl_Obj *objPtr, int *lengthPtr)
{
 if (objPtr)
  {
   dTHX;
   char *s;
   if ((SvROK(objPtr) && !SvOBJECT(SvRV(objPtr))
        && SvTYPE(SvRV(objPtr)) == SVt_PVAV) ||
       (SvTYPE(objPtr) == SVt_PVAV))
    objPtr = ForceScalar(aTHX_ objPtr);
   if (SvPOK(objPtr))
    {
     STRLEN len;
#ifdef SvUTF8
     if (!SvUTF8(objPtr))
      sv_utf8_upgrade(objPtr);
#endif
     s = SvPV(objPtr, len);
#ifdef SvUTF8
     if (!is_utf8_string(s,len))
      {
     /*
       LangDebug("%s @ %d not utf8\n",__FUNCTION__,__LINE__);
       sv_dump(objPtr);
     */
       s = SvPV(objPtr, len);
       if (!is_utf8_string(s,len))
        {
         U8 *p = (U8 *) s;
	 U8 *e = p + len;
	 while (p < e)
	  {
	   if (*p > 0x7F)
	    *p = '?';
	   p++;
	  }
	}
      }
#endif
     if (lengthPtr)
      *lengthPtr = len;
    }
   else
    {
     s = LangString(objPtr);
#ifdef SvUTF8
     if (!is_utf8_string(s,strlen(s)))
      {
       s = FixBuggyUTF8String(objPtr);
      }
     if (!is_utf8_string(s,strlen(s)))
      {
       LangDebug("%s @ %d not utf8\n",__FUNCTION__,__LINE__);
       sv_dump(objPtr);
       abort();
      }
#endif
     if (lengthPtr)
      *lengthPtr = strlen(s);
    }
   return s;
  }
 return NULL;
}


char *
Tcl_GetString(Tcl_Obj *objPtr)
{
 return Tcl_GetStringFromObj(objPtr, NULL);
}

unsigned char *
Tcl_GetByteArrayFromObj(Tcl_Obj * objPtr, int * lengthPtr)
{
 /* FIXME: presumably should downgrade from UTF-8,
    what frees it ?
  */
 /* SRT: Is this correct? */
 dTHX;
 sv_utf8_downgrade(objPtr, 0);
 if (lengthPtr)
  {
   return (unsigned char *) SvPV(objPtr, *lengthPtr);
  }
 else
  {
   return (unsigned char *) SvPV(objPtr, PL_na);
  }
/* return (unsigned char *) Tcl_GetStringFromObj (objPtr, lengthPtr); */
}


AV *
ForceList(pTHX_ Tcl_Interp *interp, Tcl_Obj *sv)
{
 if (SvTYPE(sv) == SVt_PVAV)
  {
   return (AV *) sv;
  }
 else
  {
   int object = sv_isobject(sv);
   if (!object && SvROK(sv) && SvTYPE(SvRV(sv)) == SVt_PVAV)
    {
     return (AV *) SvRV(sv);
    }
   else
    {
     AV *av = newAV();
     if (!object && (SvIOK(sv) || SvNOK(sv)))
      {
       /* Simple case of single number */
       av_store(av,0,SvREFCNT_inc(sv));
      }
     else
      {
       /* Parse TCL like strings
          {} are quotes - and can be nested
          \ quotes \ itself and whitespace

          Older Tk used this perl code ...
          local $_ = shift;
          my (@arr, $tmp);
          while (/\{([^{}]*)\}|((?:[^\s\\]|\\.)+)/gs) {
            if (defined $1) { push @arr, $1 }
            else { $tmp = $2 ; $tmp =~ s/\\([\s\\])/$1/g; push @arr, $tmp }
          }
       */
       unsigned char *s = (unsigned char *) Tcl_GetString(sv);
       int i = 0;
       while (*s)
        {
         unsigned char *base;
         /* Skip leading whitespace */
         while (isspace(*s))
          s++;
         if (!*s)
          break;
         base = s;
         if (*s == '{')
          {
           /* Slurp chars till we find matching '}' */
           int count = 1;  /* number of open '{' */
           base = ++s;
           while (*s)
            {
             if (*s == '{')
              count++;
             else if (*s == '}' && (--count <= 0))
              break;
             s++;
            }
           if (*s != '}')
            {
             /* Found end of string before closing '}'
                TCL would set an error, we will just include the
                un-matched opening '{' in the string.
              */
             base--;
            }
          }
         else if (*s)
          {
           /* Find a "word" */
           while (*s && !isspace(*s))
            {
             if (*s == '\\' && s[1]) /* \ quotes anything except end of string */
              s++;
             s++;
            }
          }
         av_store(av,i++,Tcl_NewStringObj(base,(s-base)));
         if (*s == '}')
          s++;
        }
      }
     /* Now have an AV populated decide how to return */
     if (SvREADONLY(sv))
      {
       sv_2mortal((SV *) av);
       return av;
      }
     else
      {
       SV *ref = MakeReference((SV *) av);
       SvSetMagicSV(sv,ref);
       SvREFCNT_dec(ref);
      }
     return (AV *) SvRV(sv);
    }
  }
}

void
Tcl_SetListObj(Tcl_Obj * objPtr,int objc, Tcl_Obj *CONST objv[])
{
 dTHX;
 AV *av = ForceList(aTHX_ NULL,objPtr);
 av_clear(av);
 while (objc-- > 0)
  {
   /* Used by tkListbox.c passing in array from Tcl_ListObjGetEelements()
    * so we need to increment REFCNT
    */
   av_store(av,objc,SvREFCNT_inc(objv[objc]));
  }
}

int
Tcl_ListObjAppendElement (Tcl_Interp *interp, Tcl_Obj *listPtr,
			    Tcl_Obj *objPtr)
{
 dTHX;
 AV *av = ForceList(aTHX_ interp,listPtr);
 if (!objPtr)
  objPtr = &PL_sv_undef;
 if (av)
  {
   av_push(av, objPtr);
   return TCL_OK;
  }
 return TCL_ERROR;
}

void
Tcl_AppendElement(interp, string)
Tcl_Interp *interp;
CONST char *string;
{
 dTHX;
 Tcl_Obj *result = Tcl_GetObjResult(interp);
 Tcl_Obj *value  = Tcl_NewStringObj(string,-1);
 if (1 || SvOK(result))
  {
   Tcl_ListObjAppendElement(interp,result,value);
  }
 else
  {
   SvSetMagicSV(result, value);
   LangDumpVec(__FUNCTION__,1,&result);
  }
}



AV *
MaybeForceList(pTHX_ Tcl_Interp *interp, Tcl_Obj *sv)
{
 AV *av;
 int object = sv_isobject(sv);
 if (!object && SvROK(sv) && SvTYPE(SvRV(sv)) == SVt_PVAV)
  {
   return (AV *) SvRV(sv);
  }
 else if (!object && (SvIOK(sv) || SvNOK(sv)))
  {
   av = newAV();
   av_store(av,0,SvREFCNT_inc(sv));
   sv_2mortal((SV *) av);
   return av;
  }
 else if (SvREADONLY(sv))
  {
   /* returns mortal list anyway */
   return ForceList(aTHX_ interp,sv);
  }
 else
  {
   SvREADONLY_on(sv);
   av = ForceList(aTHX_ interp,sv);
   SvREADONLY_off(sv);
   /* If there was more than one element set the SV */
   if (av && av_len(av) > 0)
    {
     /* AV is mortal - so we want newRV not MakeReference as we need extra REFCNT */
     SV *ref = newRV((SV *) av);
     SvSetMagicSV(sv,ref);
     SvREFCNT_dec(ref);
    }
   return av;
  }
}

int
Tcl_ListObjGetElements (Tcl_Interp *interp, Tcl_Obj *listPtr,
			    int *objcPtr, Tcl_Obj ***objvPtr)
{
 if (listPtr)
  {
   dTHX;
   AV *av = MaybeForceList(aTHX_ interp,listPtr);
   if (av)
    {
     *objcPtr = av_len(av)+1;
     *objvPtr = AvARRAY(av);
     return TCL_OK;
    }
  }
 *objcPtr = 0;
 *objvPtr = NULL;
 return TCL_OK;
}

int
Tcl_ListObjIndex (Tcl_Interp *interp,  Tcl_Obj *listPtr, int index,
			    Tcl_Obj **objPtrPtr)
{
 dTHX;
 AV *av = ForceList(aTHX_ interp,listPtr);
 if (av)
  {
   SV **svp = av_fetch(av, index, 0);
   if (svp)
    {
     *objPtrPtr = *svp;
     return TCL_OK;
    }
   return EXPIRE((interp, "No element %d",index));
  }
 return TCL_ERROR;
}

int
Tcl_ListObjLength (Tcl_Interp *interp, Tcl_Obj *listPtr, int *intPtr)
{
 dTHX;
 AV *av = ForceList(aTHX_ interp,listPtr);
 if (av)
  {
   *intPtr = av_len(av)+1;
   return TCL_OK;
  }
 return TCL_ERROR;
}

int
Tcl_ListObjReplace (Tcl_Interp *interp, Tcl_Obj *listPtr, int first, int count,
			    int objc, Tcl_Obj *CONST objv[])
{
 dTHX;
 AV *av = ForceList(aTHX_ interp,listPtr);
 if (av)
  {
   int len = av_len(av)+1;
   int newlen;
   int i;
   if (first < 0)
    first = 0;
   if (first >= len)
     first = len;	/* So we'll insert after last element. */
   if (first + count > len)
    count = first-len;
   newlen = len-count+objc;
   if (newlen > len)
    {
     /* Move entries beyond old range up to make room for new */
     av_extend(av,newlen-1);
     for (i=len-1; i >= (first+count); i--)
      {
       SV **svp = av_fetch(av,i,0);
       if (svp)
        av_store(av,i+newlen-len,SvREFCNT_inc(*svp));
      }
    }
   else if (newlen < len)
    {
     /* Delete array elements which will be sliced away */
     for (i=first; i < first+count; i++)
      {
       av_delete(av,i,0);
      }
     /* Move entries beyond old range down to new location */
     for (i=first+count; i < len; i++)
      {
       SV **svp = av_fetch(av,i,0);
       if (svp)
        av_store(av,i+newlen-len,SvREFCNT_inc(*svp));
      }
#ifdef AvFILLp
     AvFILLp(av) = newlen-1;
#else
     AvFILL(av) = newlen-1;
#endif
    }
   /* Store new values */
   for (i=0; i < objc; i++)
    {
     /* In tkListbox.c used with incoming objv
      * so we need to make copies
      */
     av_store(av,first+i,newSVsv(objv[i]));
    }
   return TCL_OK;
  }
 return TCL_ERROR;
}

int
Tcl_ListObjAppendList(Tcl_Interp * interp, Tcl_Obj * listPtr,Tcl_Obj * elemListPtr)
{
 dTHX;
 Tcl_Obj **objv;
 int objc = 0;
 int code;
 AV *av = ForceList(aTHX_ interp,listPtr);
 if ((code = Tcl_ListObjGetElements(interp,elemListPtr,&objc,&objv)) == TCL_OK)
  {
   dTHX;
   int j = av_len(av)+1;
   int i;
   for (i=0; i < objc; i++)
    {
     av_store(av,j++,objv[i]);
    }
  }
 return code;
}




Tcl_Obj *
Tcl_ConcatObj (int objc, Tcl_Obj *CONST objv[])
{
 /* This is very like Tcl_NewListObj() - but is typically
    called on a command's objv - which will not have REFCNT
    set way Tcl_NewListObj() is expecting. So correct that
    then call Tcl_NewListObj().
  */
 dTHX;
 int i;
 for (i=0; i < objc; i++)
  {
   SV *sv = (SV *)objv[i];
   if (sv)
    {
     SvREFCNT_inc(sv);
    }
  }
 return Tcl_NewListObj (objc, objv);
}


char *
Tcl_DStringAppendElement(dsPtr, string)
    Tcl_DString *dsPtr;		/* Structure describing dynamic string. */
    CONST char *string;		/* String to append.  Must be
				 * null-terminated. */
{
    CONST char *s = string;
    int ch;
    while ((ch = *s))
     {
      if (isspace(ch))
       break;
      s++;
     }
    if (Tcl_DStringLength(dsPtr)) {
	Tcl_DStringAppend(dsPtr, " ", 1);
    }
    if (*s) {
	Tcl_DStringAppend(dsPtr, "{", 1);
    }
    Tcl_DStringAppend(dsPtr, string, -1);
    if (*s) {
	Tcl_DStringAppend(dsPtr, "}", 1);
    }
    return Tcl_DStringValue(dsPtr);
}

void
Tcl_AppendStringsToObj (Tcl_Obj *obj,...)
{
 dTHX;
 va_list ap;
 char *s;
 SV *sv = ForceScalar(aTHX_ obj);
 va_start(ap,obj);
 while ((s = va_arg(ap,char *)))
  {
   Tcl_AppendToObj(sv,s,-1);
  }
 va_end(ap);
 if (sv != obj && SvROK(obj))
  {
   SvSetMagicSV(obj,sv);
  }
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_GetIndexFromObj --
 *
 *	This procedure looks up an object's value in a table of strings
 *	and returns the index of the matching string, if any.
 *
 * Results:

 *	If the value of objPtr is identical to or a unique abbreviation
 *	for one of the entries in objPtr, then the return value is
 *	TCL_OK and the index of the matching entry is stored at
 *	*indexPtr.  If there isn't a proper match, then TCL_ERROR is
 *	returned and an error message is left in interp's result (unless
 *	interp is NULL).  The msg argument is used in the error
 *	message; for example, if msg has the value "option" then the
 *	error message will say something flag 'bad option "foo": must be
 *	...'
 *
 * Side effects:
 *	The result of the lookup is cached as the internal rep of
 *	objPtr, so that repeated lookups can be done quickly.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_GetIndexFromObj(interp, objPtr, tablePtr, msg, flags, indexPtr)
    Tcl_Interp *interp; 	/* Used for error reporting if not NULL. */
    Tcl_Obj *objPtr;		/* Object containing the string to lookup. */
    CONST char **tablePtr;		/* Array of strings to compare against the
				 * value of objPtr; last entry must be NULL
				 * and there must not be duplicate entries. */
    CONST char *msg;			/* Identifying word to use in error messages. */
    int flags;			/* 0 or TCL_EXACT */
    int *indexPtr;		/* Place to store resulting integer index. */
{
    int index, length, i, numAbbrev;
    CONST char *key, *p1, *p2, **entryPtr;
    Tcl_Obj *resultPtr;

    /*
     * Lookup the value of the object in the table.  Accept unique
     * abbreviations unless TCL_EXACT is set in flags.
     */

    key = Tcl_GetStringFromObj(objPtr, &length);
    index = -1;
    numAbbrev = 0;
    for (entryPtr = tablePtr, i = 0; *entryPtr != NULL; entryPtr++, i++) {
	for (p1 = key, p2 = *entryPtr; *p1 == *p2; p1++, p2++) {
	    if (*p1 == 0) {
		index = i;
		goto done;
	    }
	}
	if (*p1 == 0) {
	    /*
	     * The value is an abbreviation for this entry.  Continue
	     * checking other entries to make sure it's unique.  If we
	     * get more than one unique abbreviation, keep searching to
	     * see if there is an exact match, but remember the number
	     * of unique abbreviations and don't allow either.
	     */

	    numAbbrev++;
	    index = i;
	}
    }
    if ((flags & TCL_EXACT) || (numAbbrev != 1)) {
	goto error;
    }

    done:
    *indexPtr = index;
    return TCL_OK;

    error:
    if (interp != NULL) {
	resultPtr = Tcl_GetObjResult(interp);
	Tcl_AppendStringsToObj(resultPtr,
		(numAbbrev > 1) ? "ambiguous " : "bad ", msg, " \"",
		key, "\": must be ", *tablePtr, (char *) NULL);
	for (entryPtr = tablePtr+1; *entryPtr != NULL; entryPtr++) {
	    if (entryPtr[1] == NULL) {
		Tcl_AppendStringsToObj(resultPtr, ", or ", *entryPtr,
			(char *) NULL);
	    } else {
		Tcl_AppendStringsToObj(resultPtr, ", ", *entryPtr,
			(char *) NULL);
	    }
	}
    }
    return TCL_ERROR;
}

void
Tcl_AppendToObj(objPtr, bytes, length)
    register Tcl_Obj *objPtr;	/* Points to the object to append to. */
    CONST char *bytes;		/* Points to the bytes to append to the
				 * object. */
    register int length;	/* The number of bytes to append from
				 * "bytes". If < 0, then append all bytes
				 * up to NULL byte. */
{
 dTHX;
 SV *sv = ForceScalar(aTHX_ objPtr);
 int hi;
 if (length < 0)
  length = strlen(bytes);
#ifdef SvUTF8
 if ((hi = has_highbit(bytes,length)))
  {
   sv_utf8_upgrade(sv);
  }
 sv_catpvn(sv, bytes, length);
 if (hi)
  SvUTF8_on(sv);
#else
 sv_catpvn(sv, bytes, length);
#endif
 if (sv != objPtr && SvROK(objPtr))
  SvSetMagicSV(objPtr,sv);
}

void
Tcl_AppendObjToObj(Tcl_Obj * objPtr,Tcl_Obj * appendObjPtr)
{
 int len = 0;
 char *s = Tcl_GetStringFromObj(appendObjPtr,&len);
 Tcl_AppendToObj(objPtr,s,len);
}



void
Tcl_WrongNumArgs(interp, objc, objv, message)
    Tcl_Interp *interp;			/* Current interpreter. */
    int objc;				/* Number of arguments to print
					 * from objv. */
    Tcl_Obj *CONST objv[];		/* Initial argument objects, which
					 * should be included in the error
					 * message. */
    CONST char *message;		/* Error message to print after the
					 * leading objects in objv. The
					 * message may be NULL. */
{
    Tcl_Obj *objPtr;
    char **tablePtr;
    int i;

    objPtr = Tcl_GetObjResult(interp);
    Tcl_AppendToObj(objPtr, "wrong # args: should be \"", -1);
    for (i = 0; i < objc; i++) {
	Tcl_AppendStringsToObj(objPtr,
		    Tcl_GetStringFromObj(objv[i], (int *) NULL),
		    (char *) NULL);
	if (i < (objc - 1)) {
	    Tcl_AppendStringsToObj(objPtr, " ", (char *) NULL);
	}
    }
    if (message) {
      Tcl_AppendStringsToObj(objPtr, " ", message, (char *) NULL);
    }
    Tcl_AppendStringsToObj(objPtr, "\"", (char *) NULL);
}


#define DStringSV(svp) ((*svp) ? (*svp = ForceScalar(aTHX_ *svp)) : (*svp = newSVpv("",0), *svp))

#undef Tcl_DStringInit
void
Tcl_DStringInit(Tcl_DString *svp)
{
 *svp = NULL;
}

void
Tcl_DbDStringInit(Tcl_DString *svp,char *file,int line)
{
 Tcl_DStringInit(svp);
}

void
Tcl_DStringFree(Tcl_DString *svp)
{
 SV *sv;
 if ((sv = *svp))
  {
   dTHX;
   SvREFCNT_dec(sv);
   *svp = Nullsv;
  }
}

void
Tcl_DStringResult(Tcl_Interp *interp, Tcl_DString *svp)
{
 dTHX;
 SV *sv = DStringSV(svp);
 /* Tcl8.1+ strings are UTF-8 */
 Tcl_SetObjResult(interp,sv_maybe_utf8(sv));
 /* Now "free" the DString - the SvREFCNT_dec has been done by SetObjResult */
 *svp = Nullsv;
}

char *
Tcl_DStringAppend(Tcl_DString *svp, CONST char *s, int len)
{
 dTHX;
 SV *sv = DStringSV(svp);
 Tcl_AppendToObj(sv,(char *)s,len);
 return SvPVX(sv);
}

int
Tcl_DStringLength(Tcl_DString *svp)
{
 dTHX;
 return (int) ((*svp) ? SvCUR(DStringSV(svp)) : 0);
}

void
Tcl_DStringSetLength(Tcl_DString *svp,int len)
{
 dTHX;
 SV *sv = DStringSV(svp);
 char *s = SvGROW(sv,(Size_t)(len+1));
 s[len] = '\0';
 SvCUR(sv) = len;
}

char *
Tcl_DStringValue(Tcl_DString *svp)
{
 dTHX;
 SV *sv = DStringSV(svp);
 STRLEN len;
 return SvPV(sv,len);
}

void
Tcl_DStringGetResult(Tcl_Interp *interp, Tcl_DString *svp)
{
 int len;
 char *s = Tcl_GetStringFromObj(Tcl_GetObjResult(interp),&len);
 Tcl_DStringAppend(svp,s,len);
}

/* Now fake Tcl_Obj * internals routines */

static void
DummyFreeProc(Tcl_Obj *obj)
{
}

static void
IntUpdateStringProc(Tcl_Obj *obj)
{
 dTHX;
 STRLEN len;
 (void) SvPV(obj,len);
}

static void
IntDupProc(Tcl_Obj *src,Tcl_Obj *dst)
{
 dTHX;
 SvSetMagicSV(dst,src);
 TclObjSetType(dst,TclObjGetType(src));
}

static int
IntSetFromAnyProc(Tcl_Interp *interp, Tcl_Obj *obj)
{
 Tcl_ObjType *typePtr;
 Tcl_GetString(obj);
 typePtr = TclObjGetType(obj);
 if ((typePtr != NULL) && (typePtr->freeIntRepProc != NULL)) {
	(*typePtr->freeIntRepProc)(obj);
  }
 TclObjSetType(obj,&tclIntType);
 return TCL_OK;
}

extern Tcl_ObjType   tclDoubleType;

static int
DoubleSetFromAnyProc(Tcl_Interp *interp, Tcl_Obj *obj)
{
 Tcl_ObjType *typePtr;
 Tcl_GetString(obj);
 typePtr = TclObjGetType(obj);
 if ((typePtr != NULL) && (typePtr->freeIntRepProc != NULL)) {
	(*typePtr->freeIntRepProc)(obj);
  }
 TclObjSetType(obj,&tclDoubleType);
 return TCL_OK;
}

Tcl_ObjType tclIntType = {
  "int",
  DummyFreeProc,
  IntDupProc,
  IntUpdateStringProc,
  IntSetFromAnyProc
};

Tcl_ObjType tclDoubleType = {
  "double",
  DummyFreeProc,
  IntDupProc,
  IntUpdateStringProc,
  DoubleSetFromAnyProc
};

Tcl_ObjType perlDummyType = {
  "scalar",
  DummyFreeProc,
  IntDupProc,
  IntUpdateStringProc,
  IntSetFromAnyProc
};

typedef struct
{
 Tcl_ObjType *type;
 Tcl_InternalRep internalRep;
} TclObjMagic_t;

static int
TclObj_get(pTHX_ SV *sv, MAGIC *mg)
{
 TclObjMagic_t *info = (TclObjMagic_t *)SvPVX(mg->mg_obj);
 if (info->type == &tclIntType)
  {
   SvIV_set(sv,info->internalRep.longValue);
   SvIOK_on(sv);
   LangDebug("%s %p %s %ld'\n",__FUNCTION__,sv,info->type->name,SvIV(sv));
   return 0;
  }
 else if (info->type == &tclDoubleType)
  {
   SvNV_set(sv,info->internalRep.doubleValue);
   SvNOK_on(sv);
   LangDebug("%s %p %s %g'\n",__FUNCTION__,sv,info->type->name,SvNV(sv));
   return 0;
  }
 else if (SvROK(sv) || info->type == &perlDummyType)
  {
   if (!SvPOK(sv) && SvPOKp(sv))
    SvPOK_on(sv);

   if (!SvNOK(sv) && SvNOKp(sv))
    SvNOK_on(sv);

   if (!SvIOK(sv) && SvIOKp(sv))
    SvIOK_on(sv);
  }
 else
  {
   Tcl_GetString(sv);
   SvPOK_on(sv);
#if 0
   LangDebug("%s %p %s '%s'\n",__FUNCTION__,sv,info->type->name,SvPV_nolen(sv));
#endif
  }
 return 0;
}

static int
TclObj_free(pTHX_ SV *sv, MAGIC *mg)
{
 TclObjMagic_t * info;
 if (SvTYPE(mg->mg_obj) == SVTYPEMASK)
  {
   /* Oops!! Our magic info SV has already been sweeped away
    * during global destruction.  In this case we might leak
    * some the stuff hanging off the Tcl_InternalRep, but there
    * are not really much more we can do here.
    */
   return 0;
  }
 info = (TclObjMagic_t *)SvPVX(mg->mg_obj);
 if (info->type)
  {
#ifdef DEBUG_TCLOBJ
   LangDebug("%s %p %s\n",__FUNCTION__,sv,info->type->name);
#endif
   if (info->type->freeIntRepProc != NULL)
    {
     /* We _use_ MAGIC chain to locate interal rep so
      * re-link mg for duration of callback
      */
     MAGIC *save = SvMAGIC(sv);
     SvMAGIC(sv) = mg;
     mg->mg_moremagic = NULL;
     (*info->type->freeIntRepProc)(sv);
     SvMAGIC(sv) = save;
    }
  }
 else
  {
   /* We can have pretened we are double or int without setting a type */
#if 0
   LangDebug("%s %p NULL\n",__FUNCTION__,sv);
   sv_dump(sv);
#endif
  }
 return 0;
}

static int
TclObj_set(pTHX_ SV *sv, MAGIC *mg)
{
#ifdef DEBUG_TCLOBJ
 TclObjMagic_t *info = (TclObjMagic_t *)SvPVX(mg->mg_obj);
 LangDebug("%s %p %s\n",__FUNCTION__,sv,info->type->name);
#endif
 sv_unmagic(sv,PERL_MAGIC_ext);  /* sv_unmagic calls free proc */
 return 0;
}

static U32
TclObj_len(pTHX_ SV *sv, MAGIC *mg)
{
#ifdef DEBUG_TCLOBJ
 TclObjMagic_t *info = (TclObjMagic_t *)SvPVX(mg->mg_obj);
 LangDebug("%s %s\n",__FUNCTION__,info->type->name);
#endif
 return 0;
}

static int
TclObj_clear(pTHX_ SV *sv, MAGIC *mg)
{
#ifdef DEBUG_TCLOBJ
 TclObjMagic_t *info = (TclObjMagic_t *)SvPVX(mg->mg_obj);
 LangDebug("%s %p %s\n",__FUNCTION__,sv,info->type->name);
#endif
 sv_unmagic(sv,PERL_MAGIC_ext);  /* sv_unmagic calls free proc */
 return 0;
}


MGVTBL TclObj_vtab = {
 TclObj_get,
 TclObj_set,
 NULL, /* TclObj_len, */
 TclObj_clear,
 TclObj_free
};

static TclObjMagic_t *
Tcl_ObjMagic(Tcl_Obj *obj,int add)
{
 dTHX;
 MAGIC *mg = (SvTYPE(obj) >= SVt_PVMG) ? mg_find(obj,PERL_MAGIC_ext) : NULL;
 SV *data = NULL;
 TclObjMagic_t *iv;
 if (mg)
  {
   if (mg->mg_virtual == &TclObj_vtab)
    {
     data = mg->mg_obj;
    }
   else
    {
     if (add)
      {
       warn("Wrong kind of '~' magic on %"SVf,obj);
       sv_dump(obj);
       abort();
      }
    }
  }
 else if (add)
  {
   Tcl_ObjType *type =  TclObjGetType(obj);
   int rdonly = SvREADONLY(obj);
   data = newSV(sizeof(TclObjMagic_t));
   Zero(SvPVX(data),sizeof(TclObjMagic_t),char);
   if (rdonly)
    SvREADONLY_off(obj);
   sv_upgrade(obj,SVt_PVMG);
   sv_magic(obj,data,PERL_MAGIC_ext,NULL,0);
   SvREFCNT_dec(data);
   SvRMAGICAL_off(obj);
   mg = mg_find(obj,PERL_MAGIC_ext);
   if (mg->mg_obj != data)
    abort();
   mg->mg_virtual = &TclObj_vtab;
   mg_magical(obj);
   if (rdonly)
    SvREADONLY_on(obj);
   iv = (TclObjMagic_t *) SvPVX(data);
   iv->type = type;
   if (iv->type == &tclIntType)
    {
#ifdef HAS_SVIV_NOMG
     iv->internalRep.longValue = SvIV_nomg(obj);
#else
     iv->internalRep.longValue = SvIV(obj);
#endif
    }
   else if (iv->type == &tclDoubleType)
    {
#ifdef HAS_SVNV_NOMG
     iv->internalRep.doubleValue = SvNV_nomg(obj);
#else
     iv->internalRep.doubleValue = SvNV(obj);
#endif
    }
   return iv;
  }
 if (data)
  {
   TclObjMagic_t *iv = (TclObjMagic_t *) SvPVX(data);
   return iv;
  }
 return NULL;
}

Tcl_Obj *
Tcl_DuplicateObj(Tcl_Obj *src)
{
 dTHX;
 /* We get AVs either from SvRV test below, or
  * "suspect" ResultAv scheme
  */
 int object = sv_isobject(src);
 if (SvTYPE(src) == SVt_PVAV)
  {
   abort();
  }
 else if (!object && SvROK(src) && SvTYPE(SvRV(src)) == SVt_PVAV)
  {
   AV *av  = (AV *) SvRV(src);
   IV max  = av_len(av);
   AV *dst = newAV();
   int i;
   for (i=0; i <= max; i++)
    {
     /* Do a deep copy and hope there are no loops */
     SV **svp = av_fetch(av,i,0);
     SV *d    = (svp && *svp) ? Tcl_DuplicateObj(*svp) : &PL_sv_undef;
     av_store(dst,i,d);
    }
   return MakeReference((SV *) dst);
  }
 else
  {
   SV *dup = newSVsv(src);
   TclObjMagic_t *m = Tcl_ObjMagic(src,0);
   if (m && m->type)
    {
     if (m->type->dupIntRepProc)
      {
       (*m->type->dupIntRepProc)(src,dup);
      }
     else
      {
       TclObjMagic_t *n = Tcl_ObjMagic(dup,1);
       n->type = m->type;
       n->internalRep = m->internalRep;
      }
    }
   return dup;
  }
}

Tcl_ObjType *
Tcl_GetObjType(CONST char *name)
{
 if (strEQ(name,"int"))
  return &tclIntType;
 if (strEQ(name,"double"))
  return &tclDoubleType;
 LangDebug("%s wanted %s\n",__FUNCTION__,name);
 return &perlDummyType;
}

static void
NoFreeProc(Tcl_Obj *obj)
{
 TclObjMagic_t *m = Tcl_ObjMagic(obj,1);
 LangDebug("%s %p %s\n",__FUNCTION__,obj,m->type->name);
}

Tcl_ObjType *
TclObjGetType(Tcl_Obj *obj)
{
 TclObjMagic_t *m = Tcl_ObjMagic(obj,0);
 if (m)
  {
#ifdef DEBUG_TCLOBJ
   if (!m->type->freeIntRepProc)
    m->type->freeIntRepProc = &NoFreeProc;
#endif
   return m->type;
  }
 if (SvNOK(obj))
  {
   return &tclDoubleType;
  }
 else if (SvIOK(obj))
  {
   return &tclIntType;
  }
 return &perlDummyType;
}

int
TclObjLength(Tcl_Obj *obj)
{
 dTHX;
 STRLEN len;
 char *s = SvPV(obj,len);
 return len;
}

void
TclObjSetType(Tcl_Obj *obj,Tcl_ObjType *type)
{
 TclObjMagic_t *m;
 if (type != NULL && !SvOK(obj))
  {
   if (type)
    {
     croak("Cannot use undef value for object of type '%s'", type->name);
    }
   else
    {
     croak("Cannot assign magic to undef");
    }
  }
 m = Tcl_ObjMagic(obj,1);
#ifdef DEBUG_TCLOBJ
 if (m->type)
  {
   LangDebug("%s %p was %s\n",__FUNCTION__,obj,m->type->name);
  }
 if (type)
  {
   LangDebug("%s %p now %s\n",__FUNCTION__,obj,type->name);
  }
#endif
 m->type = type;
}

int
Tcl_ConvertToType(Tcl_Interp * interp, Tcl_Obj * objPtr,
                  Tcl_ObjType * typePtr)
{
    if (TclObjGetType(objPtr) == typePtr) {
	return TCL_OK;
    }

    /*
     * Use the target type's Tcl_SetFromAnyProc to set "objPtr"s internal
     * form as appropriate for the target type. This frees the old internal
     * representation.
     */

    return typePtr->setFromAnyProc(interp, objPtr);
}


Tcl_InternalRep *
TclObjInternal(Tcl_Obj *obj)
{
 TclObjMagic_t *m = Tcl_ObjMagic(obj,1);
 return &(m->internalRep);
}

void
Tcl_RegisterObjType(Tcl_ObjType *type)
{
}


Tcl_Obj *
LangCopyArg(sv)
SV *sv;
{
 if (sv)
  {
   dTHX;
   MAGIC *mg = (SvTYPE(sv) >= SVt_PVMG) ? mg_find(sv,PERL_MAGIC_ext) : NULL;
   if (mg && mg->mg_virtual == &TclObj_vtab)
    {
     return Tcl_DuplicateObj(sv);
    }
   if (SvROK(sv) && SvTYPE(SvRV(sv)) == SVt_PVCV)
    {
     return LangMakeCallback(sv);
    }
   sv = newSVsv(sv);
  }
 return sv;
}






