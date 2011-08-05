/*
  Copyright (c) 2000-2004 Nick Ing-Simmons. All rights reserved.
  This program is free software; you can redistribute it and/or
  modify it under the same terms as Perl itself.
*/

#define PERL_NO_GET_CONTEXT
#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>

#ifdef HAS_NL_LANGINFO
#include <langinfo.h>
#endif

#define U8 U8
#include "tkGlue.def"

#include "pTk/tkPort.h"
#include "pTk/tkInt.h"
#include "tkGlue.h"

#ifdef WIN32
#include "pTk/tkWinInt.h"
#endif


#ifdef SvUTF8

#ifndef utf8_to_uv
#define utf8_to_uv utf8_to_uvchr
#endif

#ifndef UTF8_MAXBYTES_CASE
#define UTF8_MAXBYTES_CASE UTF8_MAXLEN_UCLC
#endif

/* -------------------------------------------------------------------------- */
/* UTF8-ness routines
/* -------------------------------------------------------------------------- */

int
Tcl_UtfCharComplete(str, len)
CONST char *str;		/* String to check if first few bytes
				 * contain a complete UTF-8 character. */
int len;			/* Length of above string in bytes. */
{
    return len >= UTF8SKIP((U8 *) str);
}


Tcl_UniChar
Tcl_UniCharToUpper(int ch)
{
 dTHX;
 U8 tmpbuf[UTF8_MAXBYTES_CASE+1];
 STRLEN len;
 return Perl_to_uni_upper(aTHX_ ch, tmpbuf, &len);
}

Tcl_UniChar
Tcl_UniCharToLower(int ch)
{
 dTHX;
 U8 tmpbuf[UTF8_MAXBYTES_CASE+1];
 STRLEN len;
 return Perl_to_uni_lower(aTHX_ ch, tmpbuf, &len);
}

int
Tcl_UniCharIsAlpha(int ch)
{
 dTHX;
 return Perl_is_uni_alpha(aTHX_ ch);
}

int
Tcl_UniCharIsWordChar(int ch)
{
 dTHX;
 return Perl_is_uni_alnum(aTHX_ ch);
}

int
Tcl_UniCharIsSpace(int ch)
{
 dTHX;
 return Perl_is_uni_space(aTHX_ ch);
}

int
Tcl_UniCharIsUpper(int ch)
{
 dTHX;
 return Perl_is_uni_upper(aTHX_ ch);
}

int
Tcl_NumUtfChars(CONST char * src, int len)
{
 U8 *s = (U8 *) src;
 U8 *send;
 if (len < 0)
  len = strlen(src);
 send = s + len;
 len = 0;
 while (s < send)
  {
   s += UTF8SKIP(s);
   len++;
  }
 return len;
}

CONST char *
Tcl_UtfNext (CONST char * src)
{
 CONST U8 *s = (CONST U8 *) src;
 if (*s)
  src += UTF8SKIP(s);
 return src;
}

CONST char *
Tcl_UtfPrev (CONST char * src,CONST char * start)
{
 dTHX;
 U8 *s = (U8 *) src;
 if (src > start)
  return (CONST char *) Perl_utf8_hop(aTHX_ s,-1);
 else
  return (CONST char *) s;
}

CONST char *
Tcl_UtfAtIndex (CONST char * src, int index)
{
 dTHX;
 U8 *s = (U8 *) src;
 return (CONST char*)Perl_utf8_hop(aTHX_ s,index);
}

int
Tcl_UtfToUniChar (CONST char * src,Tcl_UniChar * chPtr)
{
 dTHX;
#if defined(utf8_to_uvchr)
 STRLEN len;
 *chPtr = utf8_to_uv((U8 *)src,&len);
#else
 I32 len;
 *chPtr = utf8_to_uv((U8 *)src,&len);
#endif
 return len;
}

int
Tcl_UniCharToUtf(int ch, char * buf)
{
 dTHX;
 /* We "allow any" as the page cache algorithm hits at least U+FFFE */
#ifdef UNICODE_ALLOW_ANY
 U8 *p = uvchr_to_utf8_flags((U8 *) buf,ch, UNICODE_ALLOW_ANY);
#else
 U8 *p = Perl_uv_to_utf8(aTHX_ (U8 *) buf,ch);
#endif
 return p - (U8 *) buf;
}

char *
Tcl_UniCharToUtfDString(wString, numChars, dsPtr)
    CONST Tcl_UniChar *wString;	/* Unicode string to convert to UTF-8. */
    int numChars;		/* Length of Unicode string in Tcl_UniChars
				 * (must be >= 0). */
    Tcl_DString *dsPtr;		/* UTF-8 representation of string is
				 * appended to this previously initialized
				 * DString. */
{
    CONST Tcl_UniChar *w, *wEnd;
    char *p, *string;
    int oldLength;

    /*
     * UTF-8 string length in bytes will be <= Unicode string length *
     * TCL_UTF_MAX.
     */

    oldLength = Tcl_DStringLength(dsPtr);
    Tcl_DStringSetLength(dsPtr, (oldLength + numChars + 1) * UTF8_MAXBYTES_CASE);
    string = Tcl_DStringValue(dsPtr) + oldLength;

    p = string;
    wEnd = wString + numChars;
    for (w = wString; w < wEnd; ) {
	p += Tcl_UniCharToUtf(*w, p);
	w++;
    }
    Tcl_DStringSetLength(dsPtr, oldLength + (p - string));

    return string;
}

Tcl_UniChar *
Tcl_UtfToUniCharDString(string, length, dsPtr)
    CONST char *string;		/* UTF-8 string to convert to Unicode. */
    int length;			/* Length of UTF-8 string in bytes, or -1
				 * for strlen(). */
    Tcl_DString *dsPtr;		/* Unicode representation of string is
				 * appended to this previously initialized
				 * DString. */
{
    Tcl_UniChar *w, *wString;
    CONST char *p, *end;
    int oldLength;

    if (length < 0) {
	length = strlen(string);
    }

    /*
     * Unicode string length in Tcl_UniChars will be <= UTF-8 string length
     * in bytes.
     */

    oldLength = Tcl_DStringLength(dsPtr);
    Tcl_DStringSetLength(dsPtr,
	    (int) ((oldLength + length + 1) * sizeof(Tcl_UniChar)));
    wString = (Tcl_UniChar *) (Tcl_DStringValue(dsPtr) + oldLength);

    w = wString;
    end = string + length;
    for (p = string; p < end; ) {
	p += Tcl_UtfToUniChar(p, w);
	w++;
    }
    *w = '\0';
    Tcl_DStringSetLength(dsPtr,
	    (oldLength + ((char *) w - (char *) wString)));

    return wString;
}

int
Tcl_UniCharLen(str)
    CONST Tcl_UniChar *str;	/* Unicode string to find length of. */
{
    int len = 0;

    while (*str != '\0') {
	len++;
	str++;
    }
    return len;
}


/* Doing these in-place seems risky ... */

int
Tcl_UtfToLower (char * src)
{
 dTHX;
 U8 *s = (U8 *)src;
 U8 *d = s;
 while (*s)
  {
   STRLEN len;
   Perl_to_utf8_lower(aTHX_ s, d, &len );
   d += len;
   s += len;
  }
 *d = '\0';
 return (d-(U8 *)src);
}

int
Tcl_UtfToUpper(char * src)
{
 dTHX;
 U8 *s = (U8 *)src;
 U8 *d = s;
 while (*s)
  {
   STRLEN len;
   Perl_to_utf8_upper(aTHX_ s, d, &len );
   d += len;
   s += len;
  }
 *d = '\0';
 return (d-(U8 *)src);
}

#else
/* -------------------------------------------------------------------------- */
/* Dummy UTF8-ness routines
/* -------------------------------------------------------------------------- */

Tcl_UniChar
Tcl_UniCharToUpper(int ch)
{
 return toupper(ch);
}

Tcl_UniChar
Tcl_UniCharToLower(int ch)
{
 return tolower(ch);
}

int
Tcl_UniCharIsAlpha(int ch)
{
 return isalpha(ch);
}

int
Tcl_UniCharIsUpper(int ch)
{
 return isupper(ch);
}

int
Tcl_NumUtfChars(CONST char * src, int len)
{
 if (len < 0)
  return strlen(src);
 return len;
}

int
Tcl_UtfToLower (char * src)
{
 char *s = src;
 int n = 0;
 while (*s)
  {
   *s = tolower(UCHAR(*s));
   s++;
  }
 *s = '\0';
 return (s-src);
}

int
Tcl_UtfToUpper(char * src)
{
 char *s = src;
 int n = 0;
 while (*s)
  {
   *s = toupper(UCHAR(*s));
   s++;
  }
 *s = '\0';
 return (s-src);
}

CONST char *
Tcl_UtfNext (CONST char * src)
{
 return src+1;
}

char *
Tcl_UtfPrev (CONST char * src,CONST char * start)
{
 if (src > start)
  src--;
 return (char *)src;
}

char *
Tcl_UtfAtIndex (CONST char * src, int index)
{
 return (char*)src+index;
}

int
Tcl_UtfToUniChar (CONST char * src,Tcl_UniChar * chPtr)
{
 *chPtr = *src;
 return 1;
}

int
Tcl_UniCharToUtf(int ch, char * buf)
{
 *buf = ch;
 return 1;
}

#endif /* SvUTF8 */

/*
 *----------------------------------------------------------------------
 *
 * Tcl_StringMatch --
 *
 *	See if a particular string matches a particular pattern.
 *
 * Results:
 *	The return value is 1 if string matches pattern, and
 *	0 otherwise.  The matching operation permits the following
 *	special characters in the pattern: *?\[] (see the manual
 *	entry for details on what these mean).
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_StringMatch(string, pattern)
    CONST char *string;		/* String. */
    CONST char *pattern;	/* Pattern, which may contain special
				 * characters. */
{
    int p, s;
    CONST char *pstart = pattern;

    while (1) {
	p = *pattern;
	s = *string;

	/*
	 * See if we're at the end of both the pattern and the string.  If
	 * so, we succeeded.  If we're at the end of the pattern but not at
	 * the end of the string, we failed.
	 */

	if (p == '\0') {
	    if (s == '\0') {
		return 1;
	    } else {
		return 0;
	    }
	}
	if ((s == '\0') && (p != '*')) {
	    return 0;
	}

	/* Check for a "*" as the next pattern character.  It matches
	 * any substring.  We handle this by calling ourselves
	 * recursively for each postfix of string, until either we
	 * match or we reach the end of the string.
	 */

	if (p == '*') {
	    pattern++;
	    if (*pattern == '\0') {
		return 1;
	    }
	    while (1) {
		if (Tcl_StringMatch(string, pattern)) {
		    return 1;
		}
		if (*string == '\0') {
		    return 0;
		}
		string++;
	    }
	}

	/* Check for a "?" as the next pattern character.  It matches
	 * any single character.
	 */

	if (p == '?') {
	    Tcl_UniChar ch;

	    pattern++;
	    string += Tcl_UtfToUniChar(string, &ch);
	    continue;
	}

	/* Check for a "[" as the next pattern character.  It is followed
	 * by a list of characters that are acceptable, or by a range
	 * (two characters separated by "-").
	 */

	if (p == '[') {
	    Tcl_UniChar ch, startChar, endChar;

	    pattern++;
	    string += Tcl_UtfToUniChar(string, &ch);

	    while (1) {
		if ((*pattern == ']') || (*pattern == '\0')) {
		    return 0;
		}
		pattern += Tcl_UtfToUniChar(pattern, &startChar);
		if (*pattern == '-') {
		    pattern++;
		    if (*pattern == '\0') {
			return 0;
		    }
		    pattern += Tcl_UtfToUniChar(pattern, &endChar);
		    if (((startChar <= ch) && (ch <= endChar))
			    || ((endChar <= ch) && (ch <= startChar))) {
			/*
			 * Matches ranges of form [a-z] or [z-a].
			 */

			break;
		    }
		} else if (startChar == ch) {
		    break;
		}
	    }
	    while (*pattern != ']') {
		if (*pattern == '\0') {
		    pattern = Tcl_UtfPrev(pattern, pstart);
		    break;
		}
		pattern++;
	    }
	    pattern++;
	    continue;
	}

	/* If the next pattern character is '\', just strip off the '\'
	 * so we do exact matching on the character that follows.
	 */

	if (p == '\\') {
	    pattern++;
	    p = *pattern;
	    if (p == '\0') {
		return 0;
	    }
	}

	/* There's no special character.  Just make sure that the next
	 * bytes of each string match.
	 */

	if (s != p) {
	    return 0;
	}
	pattern++;
	string++;
    }
}

static HV *encodings = NULL;

Tcl_Encoding system_encoding = NULL;

Tcl_Encoding
GetSystemEncoding(void)
{
 if (!system_encoding)
  {
   char *codeset = NULL;
/* This assumes perl's Configure probe stuff is #include-d above */
#if defined(HAS_NL_LANGINFO) && defined(CODESET)
   codeset = nl_langinfo(CODESET);
#endif
   if (!codeset)
    codeset = "iso8859-1";
   system_encoding = Tcl_GetEncoding(NULL,codeset);
   if (!system_encoding)
    system_encoding = Tcl_GetEncoding(NULL,"iso8859-1");
  }
 return system_encoding;
}

#define PerlEncObj(enc) (HeVAL((HE *) (enc)))

SV *
Lang_SystemEncoding(void)
{
 dTHX;
 return SvREFCNT_inc(PerlEncObj(GetSystemEncoding()));
}

Tcl_Encoding
Tcl_GetEncoding (Tcl_Interp * interp, CONST char * name)
{
 dTHX;
 HE *he;
 STRLEN len = strlen(name);
 SV *sv   = NULL;
 SV *nmsv  = newSVpv((char *)name,len);
 if (!encodings)
  {
   encodings = newHV();
  }
 he = hv_fetch_ent(encodings,nmsv,0,0);
 if (!he || !HeVAL(he))
  {
   dSP;
   ENTER;
   SAVETMPS;
   PUSHMARK(sp);
   XPUSHs(sv_2mortal(newSVpv("Tk",0)));
   XPUSHs(nmsv);
   PUTBACK;
   perl_call_method("getEncoding",G_SCALAR);
   SPAGAIN;
   sv = POPs;
   PUTBACK;
   he = hv_store_ent(encodings,nmsv,newSVsv(sv),0);
   if (0 && !SvOK(sv))
    warn("Cannot find '%s'",name);
   FREETMPS;
   LEAVE;
  }
 SvREFCNT_dec(nmsv);
 sv = HeVAL(he);
 if (sv_isobject(sv))
  {
   SvREFCNT_inc(sv);
   return (Tcl_Encoding) he;
  }
 else
  {
   if (SvOK(sv))
    warn("Strange encoding %"SVf,sv);
  }
 return NULL;
}

Tcl_Encoding
Lang_CreateEncoding(CONST char *encodingName,
    Tcl_EncodingConvertProc *toUtfProc,
    Tcl_EncodingConvertProc *fromUtfProc,
    Tcl_EncodingFreeProc *freeProc,
    ClientData clientData,
    int nullSize)
{
 return Tcl_GetEncoding(NULL,encodingName);
}

void
Tcl_FreeEncoding (Tcl_Encoding t)
{
 if (t)
  {
   dTHX;
   HE *he = (HE *) t;
   SV *sv = HeVAL(he);
   SvREFCNT_dec(sv);
  }
}

CONST char *
Tcl_GetEncodingName(Tcl_Encoding encoding)
{
 dTHX;
 HE *he;
 STRLEN len;
 if (!encoding)
  encoding = GetSystemEncoding();
 he = (HE *) encoding;
 return HePV(he,len);
}

static int
CallEncode(Tcl_Interp * interp,
	   Tcl_Encoding encoding, CONST char * src,
	   int srcLen, int flags,
	   Tcl_EncodingState * statePtr, char * dst,
	   int dstLen, int * srcReadPtr,
	   int * dstWrotePtr, int * dstCharsPtr,
	   const char *method)
{
 dTHX;
 int srcRead;
 int dstWrote;
 int dstChars;
 int code = TCL_OK;
 U8 *s = (U8 *) src;
 U8 *send;
 U8 *d = (U8 *) dst;
 U8 *dend;
 int chars = 0;
 dSP;
 SV *quiet;
 SV *stmp;
 SV *dtmp;
 char *td;
 STRLEN dbytes;
 if (flags & TCL_ENCODING_STOPONERROR)
  quiet = get_sv("Tk::encodeStopOnError",0);
 else
  quiet = get_sv("Tk::encodeFallback",0);
 if (!encoding)
  encoding = GetSystemEncoding();
 if (!sv_isobject(PerlEncObj(encoding)))
  abort();
 if (!srcReadPtr)
  srcReadPtr = &srcRead;
 if (!dstWrotePtr)
  dstWrotePtr = &dstWrote;
 if (!dstCharsPtr)
  dstCharsPtr = &dstChars;
 else
  {
   LangDebug("%s wants char count\n",method);
  }
 if (!src)
  srcLen = 0;
 if (srcLen < 0)
  srcLen = strlen(src);
 send = s+srcLen;
 dstLen -= 2;
 dend = d + dstLen;
 stmp = newSV(srcLen);
 while (s < send)
  {
   STRLEN len = srcLen;
   if (*method == 'e')
    {
#if 0
     /* We used to do things one char at a time ... can't remember why
        perhaps to handle partial chars ?
        we got perl to tell us length of one char using call below
        Only makes sense for encode when source is UTF-8, though
        by luck it worked for "decode" of UTF-8 as well
        provided we did not set SvUTF8_on which upset Encode.xs
      */
     UV ch = utf8n_to_uvchr(s, send-s, &len, UTF8_ALLOW_ANY|UTF8_CHECK_ONLY);
#endif
     sv_setpvn(stmp,s,len);
     if (has_highbit(s,len))
      SvUTF8_on(stmp);
    }
   else
    {
     sv_setpvn(stmp,s,len);
    }
   SPAGAIN;
   PUSHMARK(sp);
   XPUSHs(PerlEncObj(encoding));
   XPUSHs(stmp);
   XPUSHs(quiet);
   PUTBACK;
   perl_call_method(method,G_SCALAR|G_EVAL);
   if (SvTRUE(ERRSV))
    {
     code = TCL_ERROR;
     if (interp)
      {
       Tcl_SetResult(interp,SvPV_nolen(ERRSV),TCL_VOLATILE);
      }
     else
      {
       warn("%"SVf,ERRSV);
      }
     break;
    }
   SPAGAIN;
   dtmp = POPs;
   PUTBACK;
#if 0
   /* XXX This code seems to be wrong since Encode 2.10, when LEAVE_SRC was
    * default (is this true?).
    * This would fix the "selection conversion left too many bytes unconverted"
    * aborts.
    */
   if (SvCUR(stmp))
    {
     /* This could also be TCL_CONVERT_MULTIBYTE - how do we tell ? */
     code = TCL_CONVERT_UNKNOWN;
     break;
    }
#endif
   td = SvPV(dtmp,dbytes);
   if (!dbytes)
    {
     code = TCL_CONVERT_UNKNOWN;
     break;
    }
   if (d+dbytes > dend)
    {
     code = TCL_CONVERT_NOSPACE;
     dbytes = dend-d;
     break;
    }
   memcpy(d,td,dbytes);
   d += dbytes;
   /* FIXME? : Char count is bogus unless we do one-at-atime - if
      we find something that wants it we need to get it some
      other way - e.g. UTF8_SKIP()ing over whichever of src/dst is UTF-8
    */
   chars++;
   s += len;
  }
 SvREFCNT_dec(stmp);
 *srcReadPtr  = (s - (U8 *)src);
 *dstCharsPtr = chars;
 dst[dstLen]   = '\0';
 dst[dstLen+1]   = '\0';
 /* If dest is wide single '\0' may not be enough */
 Zero(d,dend-d,char);
 *dstWrotePtr = (d- (U8 *)dst);
 return code;
}

int
Tcl_ExternalToUtf (Tcl_Interp * interp,
				Tcl_Encoding encoding, CONST char * src,
				int srcLen, int flags,
				Tcl_EncodingState * statePtr, char * dst,
				int dstLen, int * srcReadPtr,
				int * dstWrotePtr, int * dstCharsPtr)
{
 return CallEncode(interp,encoding,src,srcLen,flags,statePtr,dst,dstLen,
                   srcReadPtr,dstWrotePtr,dstCharsPtr,"decode");
}

int
Tcl_UtfToExternal(Tcl_Interp * interp,
				Tcl_Encoding encoding, CONST char * src,
				int srcLen, int flags,
				Tcl_EncodingState * statePtr, char * dst,
				int dstLen, int * srcReadPtr,
				int * dstWrotePtr, int * dstCharsPtr)
{
 return CallEncode(interp,encoding,src,srcLen,flags,statePtr,dst,dstLen,
                   srcReadPtr,dstWrotePtr,dstCharsPtr,"encode");
}



char *
Tcl_UtfToExternalDString(Tcl_Encoding encoding, CONST char * src,
                         int srcLen, Tcl_DString * dsPtr)
{
 dTHX;
 dSP;
 SV *sv;
 char *s    = "";
 STRLEN len = 0;
 SV *fallback = get_sv("Tk::encodeFallback",0);
 Tcl_DStringInit(dsPtr);

 if (!encoding)
  encoding = GetSystemEncoding();
 if (!src)
  srcLen = 0;
 if (srcLen < 0)
  srcLen = strlen(src);
 if (srcLen)
  {
   int count;
   SPAGAIN;
   ENTER;
   SAVETMPS;
   PUSHMARK(sp);
   XPUSHs(PerlEncObj(encoding));
   sv = newSV(srcLen);
   sv_setpvn(sv,src,srcLen);
   sv_maybe_utf8(sv);
   XPUSHs(sv_2mortal(sv));
   XPUSHs(fallback);
   PUTBACK;
   count = perl_call_method("encode",G_SCALAR);
   SPAGAIN;
   if (count > 0)
    {
     sv = POPs;
     PUTBACK;
     if (sv && SvPOK(sv))
      s  = SvPV(sv,len);
    }
   else
    {
     LangDebug("Encode did not return a value:%s\n",SvPV_nolen(ERRSV));
    }
   Tcl_DStringAppend(dsPtr,s,len);
   FREETMPS;
   LEAVE;
  }
 else
  {
   Tcl_DStringAppend(dsPtr,"\0",1);
  }
 /* Perl has appended a \0 for us, but that may not be enough
    if encoding is "wide"
  */
 Tcl_DStringAppend(dsPtr,"\0\0\0",3);
 Tcl_DStringSetLength(dsPtr,len);
 return Tcl_DStringValue(dsPtr);
}

char *
Tcl_ExternalToUtfDString(Tcl_Encoding encoding, CONST char * src,
                         int srcLen, Tcl_DString * dsPtr)
{
 dTHX;
 dSP;
 SV *sv;
 char *s;
 STRLEN len;
 if (!encoding)
  encoding = GetSystemEncoding();
 SPAGAIN;
 ENTER;
 SAVETMPS;
 if (!src)
  srcLen = 0;
 if (srcLen < 0) {
  /* FIXME - this is supposed to be based on size of encoding's thingies ! */
#ifdef WIN32
   if (encoding == TkWinGetUnicodeEncoding())
    {
     srcLen = sizeof(Tcl_UniChar)*Tcl_UniCharLen((Tcl_UniChar *) src);
    }
   else
#endif
   srcLen = strlen(src);
 }
 SPAGAIN;
 PUSHMARK(sp);
 XPUSHs(PerlEncObj(encoding));
 sv = newSV(srcLen);
 sv_setpvn(sv,src,srcLen);
 XPUSHs(sv_2mortal(sv));
 PUTBACK;
 perl_call_method("decode",G_SCALAR);
 SPAGAIN;
 sv = POPs;
 PUTBACK;
 s  = SvPV(sv,len);
 Tcl_DStringInit(dsPtr);
 Tcl_DStringAppend(dsPtr,s,len);
 FREETMPS;
 LEAVE;
 return Tcl_DStringValue(dsPtr);
}

#if defined(WIN32) || (defined(__WIN32__) && defined(__CYGWIN__))
/*
 *---------------------------------------------------------------------------
 *
 * Tcl_WinUtfToTChar, Tcl_WinTCharToUtf --
 *
 *	Convert between UTF-8 and Unicode when running Windows NT or
 *	the current ANSI code page when running Windows 95.
 *
 *	On Mac, Unix, and Windows 95, all strings exchanged between Tcl
 *	and the OS are "char" oriented.  We need only one Tcl_Encoding to
 *	convert between UTF-8 and the system's native encoding.  We use
 *	NULL to represent that encoding.
 *
 *	On NT, some strings exchanged between Tcl and the OS are "char"
 *	oriented, while others are in Unicode.  We need two Tcl_Encoding
 *	APIs depending on whether we are targeting a "char" or Unicode
 *	interface.
 *
 *	Calling Tcl_UtfToExternal() or Tcl_ExternalToUtf() with an
 *	encoding of NULL should always used to convert between UTF-8
 *	and the system's "char" oriented encoding.  The following two
 *	functions are used in Windows-specific code to convert between
 *	UTF-8 and Unicode strings (NT) or "char" strings(95).  This saves
 *	you the trouble of writing the following type of fragment over and
 *	over:
 *
 *		if (running NT) {
 *		    encoding <- Tcl_GetEncoding("unicode");
 *		    nativeBuffer <- UtfToExternal(encoding, utfBuffer);
 *		    Tcl_FreeEncoding(encoding);
 *		} else {
 *		    nativeBuffer <- UtfToExternal(NULL, utfBuffer);
 *		}
 *
 *	By convention, in Windows a TCHAR is a character in the ANSI code
 *	page on Windows 95, a Unicode character on Windows NT.  If you
 *	plan on targeting a Unicode interfaces when running on NT and a
 *	"char" oriented interface while running on 95, these functions
 *	should be used.  If you plan on targetting the same "char"
 *	oriented function on both 95 and NT, use Tcl_UtfToExternal()
 *	with an encoding of NULL.
 *
 * Results:
 *	The result is a pointer to the string in the desired target
 *	encoding.  Storage for the result string is allocated in
 *	dsPtr; the caller must call Tcl_DStringFree() when the result
 *	is no longer needed.
 *
 * Side effects:
 *	None.
 *
 *---------------------------------------------------------------------------
 */
static Tcl_Encoding tclWinTCharEncoding;

void
TclWinSetInterfaces(
    int wide)			/* Non-zero to use wide interfaces, 0
				 * otherwise. */
{
    Tcl_FreeEncoding(tclWinTCharEncoding);
    if (wide) {
	tclWinTCharEncoding = Tcl_GetEncoding(NULL, "unicode");
    }
    else {
	tclWinTCharEncoding = NULL;
    }
}


TCHAR *
Tcl_WinUtfToTChar(string, len, dsPtr)
    CONST char *string;		/* Source string in UTF-8. */
    int len;			/* Source string length in bytes, or < 0 for
				 * strlen(). */
    Tcl_DString *dsPtr;		/* Uninitialized or free DString in which
				 * the converted string is stored. */
{
    TCHAR *res = (TCHAR *) Tcl_UtfToExternalDString(tclWinTCharEncoding,
	    string, len, dsPtr);
    return res;
}

char *
Tcl_WinTCharToUtf(string, len, dsPtr)
    CONST TCHAR *string;	/* Source string in Unicode when running
				 * NT, ANSI when running 95. */
    int len;			/* Source string length in bytes, or < 0 for
				 * platform-specific string length. */
    Tcl_DString *dsPtr;		/* Uninitialized or free DString in which
				 * the converted string is stored. */
{
    return Tcl_ExternalToUtfDString(tclWinTCharEncoding,
	    (CONST char *) string, len, dsPtr);
}


#endif
