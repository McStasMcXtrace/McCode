#ifndef _TKCANVASES
#define _TKCANVASES

#ifndef _TKCANVAS
#include "tkCanvas.h"
#endif

EXTERN Tk_CustomOption tk_CanvasTagsOption;


EXTERN void		Tk_CanvasDrawableCoords _ANSI_ARGS_((Tk_Canvas canvas,
			    double x, double y, short *drawableXPtr,
			    short *drawableYPtr));
EXTERN void		Tk_CanvasEventuallyRedraw _ANSI_ARGS_((
			    Tk_Canvas canvas, int x1, int y1, int x2,
			    int y2));
EXTERN int		Tk_CanvasGetCoord _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, CONST char *string,
			    double *doublePtr));
EXTERN Tk_CanvasTextInfo *Tk_CanvasGetTextInfo _ANSI_ARGS_((Tk_Canvas canvas));
EXTERN int		Tk_CanvasPsBitmap _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, Pixmap bitmap, int x, int y,
			    int width, int height));
EXTERN int		Tk_CanvasPsColor _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, XColor *colorPtr));
EXTERN int		Tk_CanvasPsFont _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, Tk_Font font));
EXTERN void		Tk_CanvasPsPath _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, double *coordPtr, int numPoints));
EXTERN int		Tk_CanvasPsStipple _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, Pixmap bitmap));
EXTERN double		Tk_CanvasPsY _ANSI_ARGS_((Tk_Canvas canvas, double y));
EXTERN void		Tk_CanvasSetStippleOrigin _ANSI_ARGS_((
			    Tk_Canvas canvas, GC gc));
EXTERN Tk_Window	Tk_CanvasTkwin _ANSI_ARGS_((Tk_Canvas canvas));
EXTERN void		Tk_CanvasWindowCoords _ANSI_ARGS_((Tk_Canvas canvas,
			    double x, double y, short *screenXPtr,
			    short *screenYPtr));
EXTERN void		Tk_CreateItemType _ANSI_ARGS_((Tk_ItemType *typePtr));
EXTERN Tk_ItemType *	Tk_GetItemTypes _ANSI_ARGS_((void));

/* These are from tkInt.h */

extern void		TkBezierPoints _ANSI_ARGS_((double control[],
			    int numSteps, double *coordPtr));
extern void		TkBezierScreenPoints _ANSI_ARGS_((Tk_Canvas canvas,
			    double control[], int numSteps,
			    XPoint *xPointPtr));
extern void		TkFillPolygon _ANSI_ARGS_((Tk_Canvas canvas,
			    double *coordPtr, int numPoints, Display *display,
			    Drawable drawable, GC gc, GC outlineGC));
extern int		TkMakeBezierCurve _ANSI_ARGS_((Tk_Canvas canvas,
			    double *pointPtr, int numPoints, int numSteps,
			    XPoint xPoints[], double dblPoints[]));
extern void		TkMakeBezierPostscript _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, double *pointPtr,
			    int numPoints));
extern void		TkIncludePoint _ANSI_ARGS_((Tk_Item *itemPtr,
			    double *pointPtr));
extern void		TkGetButtPoints _ANSI_ARGS_((double p1[], double p2[],
			    double width, int project, double m1[],
			    double m2[]));
extern int		TkGetMiterPoints _ANSI_ARGS_((double p1[], double p2[],
			    double p3[], double width, double m1[],
			    double m2[]));
extern int		TkLineToArea _ANSI_ARGS_((double end1Ptr[2],
			    double end2Ptr[2], double rectPtr[4]));
extern double		TkLineToPoint _ANSI_ARGS_((double end1Ptr[2],
			    double end2Ptr[2], double pointPtr[2]));
extern int		TkOvalToArea _ANSI_ARGS_((double *ovalPtr,
			    double *rectPtr));
extern double		TkOvalToPoint _ANSI_ARGS_((double ovalPtr[4],
			    double width, int filled, double pointPtr[2]));
extern int		TkPolygonToArea _ANSI_ARGS_((double *polyPtr,
			    int numPoints, double *rectPtr));
extern double		TkPolygonToPoint _ANSI_ARGS_((double *polyPtr,
			    int numPoints, double *pointPtr));

#include "tkVMacro.h"
#endif

