
/*	$Id: tixPort.h,v 1.1.1.1 2000/05/17 11:08:42 idiscovery Exp $	*/

/*
 * tixPort.h --
 *
 *	This header file handles porting issues that occur because of
 *	differences between systems.  It reads in platform specific
 *	portability files.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */
#ifndef _TIX_PORT_H_
#define _TIX_PORT_H_

#ifndef _TKPORT
#include "tkPort.h"
#endif

#ifndef _LANG
#include "Lang.h"
#endif

#ifndef _TK
#include "tk.h"
#endif

#if (!defined(__WIN32__)) && (!defined(_WIN32)) && (!defined(MAC_TCL)) && (!defined(__PM__))
    /*
     * The Tcl/Tk porting stuff is needed only in Unix.
     */
#if !defined(_TCLPORT) && !defined(_TKPORT)
#  if defined(_TKINT) || defined(_LANG)
#    include "tkPort.h"
#  else
#    include "tclPort.h"
#  endif
#endif
#endif


#if defined(WIN32) || defined(_WIN32) || defined(__WIN32__) || defined(__PM__)
#   include "tixWinPort.h"
#else
#   if defined(MAC_TCL)
#	include "tixMacPort.h"
#   else
#	include "tixUnixPort.h"
#   endif
#endif


EXTERN Tcl_HashTable *	TixGetHashTable _ANSI_ARGS_((Tcl_Interp * interp,
			    char * name, Tcl_InterpDeleteProc *deleteProc));
#define _TixGetHashTable(i,n,p) TixGetHashTable(i,n,p)

#if (TK_MAJOR_VERSION > 4)

/*
 * The font handling is changed in Tk 8.0 and later
 */

typedef Tk_Font TixFont;
#define TixFontId(font) Tk_FontId(font)

EXTERN void		TixComputeTextGeometry _ANSI_ARGS_((
			    TixFont fontStructPtr, char *string,
			    int numChars, int wrapLength, int *widthPtr,
			    int *heightPtr));
EXTERN void		TixDisplayText _ANSI_ARGS_((Display *display,
			    Drawable drawable, TixFont font,
			    char *string, int numChars, int x, int y,
			    int length, Tk_Justify justify, int underline,
			    GC gc));

#define TixFreeFont Tk_FreeFont
#define TixNameOfFont Tk_NameOfFont
#define TixGetFont Tk_GetFont

#else

typedef XFontStruct* TixFont;
#define TixFontId(font) ((font)->fid)
#define TixComputeTextGeometry TkComputeTextGeometry
#define TixDisplayText TkDisplayText
#define TixFreeFont Tk_FreeFontStruct
#define TixNameOfFont Tk_NameOfFontStruct
#define TixGetFont Tk_GetFontStruct

#ifndef TkDisplayText
EXTERN void		TkDisplayText _ANSI_ARGS_((Display *display,
			    Drawable drawable, XFontStruct *fontStructPtr,
			    char *string, int numChars, int x, int y,
			    int length, Tk_Justify justify, int underline,
			    GC gc));
#endif
#ifndef TkComputeTextGeometry
EXTERN void		TkComputeTextGeometry _ANSI_ARGS_((
			    XFontStruct *fontStructPtr, char *string,
			    int numChars, int wrapLength, int *widthPtr,
			    int *heightPtr));
#endif

#endif

#endif /* _TIX_PORT_H_ */
