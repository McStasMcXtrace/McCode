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

/* A few sample routines to get at Xlib via perl
   Will eventaually be extended to the point where
   simple pTk widgets can be *implemented* in perl

   Main stumbling block is a clean way of filling in
   a GC.

   The XDraw*() functions may be a bit messy, but
   should be okay via CODE: bodies and variable number
   of args and/or passing in array refs
*/

/* Now we have some sneakyness for Benefit of Win32.
 * As we have tkVMacro.h above any function which exists
 * should be #define'd so we can test for existance.
 */

#ifndef XLoadFont
#define XLoadFont(dpy,name) None
#endif

#ifndef XDrawString
#define XDrawString(dpy,win,gc,x,y,string,len)
#endif

static IV
SvGCIVOBJ(pTHX_ char *class,SV *sv)
{
 if (sv_isa(sv, class))
  return SvIV((SV*)SvRV(sv));
 else
  croak("Not of type %s",class);
 return 0;
}

#define SvGCint(x)           SvIV(x)
#define SvGCBool(x)          SvIV(x)
#define SvGCunsigned_long(x) SvIV(x)
#define SvGCPixmap(x)        (Pixmap) SvGCIVOBJ(aTHX_ "Pixmap",x)
#define SvGCFont(x)          (Font)   SvGCIVOBJ(aTHX_ "Font",x)

#define GCField(name,bit,field,func) \
 if (!strcmp(key,name)) {            \
  values->field = func(value);       \
  valuemask |= bit;                  \
 } else

unsigned long
GCSetValue(pTHX_ unsigned long valuemask,
           XGCValues *values,char *key,SV *value)
{
#include "GC.def"
 croak("Setting GC %s not implemented",key);
 return valuemask;
}

static void
tmpLine(tkwin,x1,y1,x2,y2,flags)
Tk_Window tkwin;
int x1,y1,x2,y2;
int flags;
{
 GC gc = None;
 XGCValues values;
 Window root = Tk_WindowId(tkwin);
 unsigned long valuemask = GCForeground | GCBackground
                           | GCSubwindowMode | GCFunction
#if 0
                           | GCLineStyle
#endif
                           ;
 values.line_style     = LineDoubleDash;

 if (flags & 4)
  {
   Window child;
   root = XRootWindow(Tk_Display(tkwin), Tk_ScreenNumber(tkwin));
   XTranslateCoordinates(Tk_Display(tkwin),Tk_WindowId(tkwin),root,
                         x1, y1, &x1, &y1, &child);
   XTranslateCoordinates(Tk_Display(tkwin),Tk_WindowId(tkwin),root,
                         x2, y2, &x2, &y2, &child);
   values.subwindow_mode = IncludeInferiors;
  }
 else
  {
   values.subwindow_mode = ClipByChildren;
  }

 if (flags & 2)
  {
   values.background     = 0x0a;
   values.foreground     = 0x05;
   values.function       = GXxor;
  }
 else
  {
   values.function       = GXcopy;
   if (flags & 1)
    {
     values.foreground     = BlackPixelOfScreen(Tk_Screen(tkwin));
     values.background     = WhitePixelOfScreen(Tk_Screen(tkwin));
    }
   else
    {
     values.background     = BlackPixelOfScreen(Tk_Screen(tkwin));
     values.foreground     = WhitePixelOfScreen(Tk_Screen(tkwin));
    }
  }
 gc = Tk_GetGC(tkwin, valuemask, &values);
 if (gc != None)
  {
   XDrawLine(Tk_Display(tkwin), root, gc, x1, y1, x2, y2);
   Tk_FreeGC(Tk_Display(tkwin),gc);
  }
 else
  croak("Cannot get graphic context");
}

#if defined(WIN32) || defined(__WIN32__) || defined(__PM__)
/* wrap the naive macro versions of these ... */
static int
pTk_XSync(Display *dpy, int flush)
{
 XSync(dpy,flush);
 return 0;
}

static int
pTk_XFlush(Display *dpy)
{
 XFlush(dpy);
 return 0;
}
#else
#define pTk_XSync  XSync
#define pTk_XFlush XFlush
#endif /* WIN32 or friends */



MODULE = Tk::Xlib	PACKAGE = Tk::Widget

void
tmpLine(win,x1,y1,x2,y2,onroot = 0)
Tk_Window	win
int	x1
int	y1
int	x2
int	y2
int	onroot


MODULE = Tk::Xlib	PACKAGE = ScreenPtr

PROTOTYPES: DISABLE

int
WidthOfScreen(s)
Screen *	s

int
WidthMMOfScreen(s)
Screen *	s

int
HeightOfScreen(s)
Screen *	s

int
HeightMMOfScreen(s)
Screen *	s

GC
DefaultGCOfScreen(s)
Screen *	s

unsigned long
BlackPixelOfScreen(s)
Screen *	s

unsigned long
WhitePixelOfScreen(s)
Screen *	s

MODULE = Tk::Xlib	PACKAGE = DisplayPtr	PREFIX = pTk_

int
pTk_XSync(dpy,discard = False)
Display *	dpy
int		discard

int
pTk_XFlush(dpy)
Display *	dpy

int
ConnectionNumber(dpy)
Display *	dpy

Font
XLoadFont(dpy,name)
Display *	dpy
char *		name

void
XListFonts(dpy,pattern,max)
Display *	dpy
char *		pattern
int		max
PPCODE:
 {
  int  count = 0;
#if !defined(__WIN32__) && !defined(__PM__)
  char **list = XListFonts(dpy, pattern, max, &count);
  int i;
  EXTEND(sp, count);
  for (i=0; i < count; i++)
   {
    PUSHs(sv_2mortal(newSVpv(list[i],0)));
   }
  XFreeFontNames(list);
#endif
  XSRETURN(count);
 }

void
XDrawLine(dpy,win,gc,x1,y1,x2,y2)
Display *	dpy
Window		win
GC		gc
int		x1
int		y1
int		x2
int		y2

void
XDrawRectangle(dpy,win,gc,x,y,width,height)
Display *	dpy
Window		win
GC		gc
int		x
int		y
int		width
int		height

void
XDrawString(dpy,win,gc,x,y,string)
Display *	dpy
Window		win
GC		gc
int		x
int		y
SV *		string
CODE:
 {
  if (SvOK(string))
   {STRLEN len;
    char *s = SvPV(string,len);
    if (s && len)
     {
      XDrawString(dpy,win,gc,x,y,s,len);
     }
   }
 }

Window
RootWindow(dpy,scr = DefaultScreen(dpy))
Display *	dpy
int		scr

char *
DisplayString(dpy)
Display *	dpy

int
DefaultScreen(dpy)
Display *	dpy

Screen *
ScreenOfDisplay(dpy,scr = DefaultScreen(dpy))
Display *	dpy
int		scr

GC
DefaultGC(dpy,scr)
Display *	dpy
int		scr

void
XQueryTree(dpy,w,root = NULL,parent = NULL)
Display *	dpy
Window		w
SV *		root
SV *		parent
PPCODE:
 {Window *children = NULL;
  unsigned int count = 0;
  Window pw = None;
  Window rw = None;
  if (XQueryTree(dpy, w, &rw, &pw, &children, &count))
   {
    unsigned int i;
    for (i=0; i < count; i++)
     {
      SV *sv = sv_newmortal();
      sv_setref_iv(sv, "Window", (IV) (children[i]));
      XPUSHs(sv);
     }
    XFree((char *) children);
   }
  else
   {
    count = 0;
    XSRETURN(0);
   }
  if (parent)
   {
    if (pw == None)
     sv_setsv(parent,&PL_sv_undef);
    else
     sv_setref_iv(parent, "Window", (IV) (pw));
   }
  if (root)
   {
    if (rw == None)
     sv_setsv(root,&PL_sv_undef);
    else
     sv_setref_iv(root, "Window", (IV) (rw));
   }
  XSRETURN(count);
 }

MODULE = Tk::Xlib	PACKAGE = GC	PREFIX = XSet

static GC
GC::new(dpy,win,...)
Display *	dpy
Window		win
CODE:
  {unsigned long valuemask = 0;
   XGCValues values;
   STRLEN na;
   int i;
   for (i=3; i < items; i += 2)
    {char *key = SvPV(ST(i),na);
     if (i+1 < items)
      valuemask = GCSetValue(aTHX_ valuemask,&values,key,ST(i+1));
     else
      croak("No value for %s",key);
    }
   RETVAL = XCreateGC(dpy,win,valuemask,&values);
  }
OUTPUT:
  RETVAL

void
XSetForeground(dpy,gc,val)
Display *	dpy
GC		gc
unsigned long	val

BOOT:
 {
  IMPORT_VTABLES;
 }

