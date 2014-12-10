#define PERL_NO_GET_CONTEXT
#ifdef __cplusplus
extern "C" {
#endif
#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>
#ifdef __cplusplus
}
#endif

#include <X11/X.h>

static int
not_here(s)
char *s;
{
    croak("%s not implemented on this architecture", s);
    return -1;
}

static double
constant(name, arg)
char *name;
int arg;
{
    errno = 0;
    switch (*name) {
    case 'A':
	if (strEQ(name, "Above"))
#ifdef Above
	    return Above;
#else
	    goto not_there;
#endif
	if (strEQ(name, "AllTemporary"))
#ifdef AllTemporary
	    return AllTemporary;
#else
	    goto not_there;
#endif
	if (strEQ(name, "AllocAll"))
#ifdef AllocAll
	    return AllocAll;
#else
	    goto not_there;
#endif
	if (strEQ(name, "AllocNone"))
#ifdef AllocNone
	    return AllocNone;
#else
	    goto not_there;
#endif
	if (strEQ(name, "AllowExposures"))
#ifdef AllowExposures
	    return AllowExposures;
#else
	    goto not_there;
#endif
	if (strEQ(name, "AlreadyGrabbed"))
#ifdef AlreadyGrabbed
	    return AlreadyGrabbed;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Always"))
#ifdef Always
	    return Always;
#else
	    goto not_there;
#endif
	if (strEQ(name, "AnyButton"))
#ifdef AnyButton
	    return AnyButton;
#else
	    goto not_there;
#endif
	if (strEQ(name, "AnyKey"))
#ifdef AnyKey
	    return AnyKey;
#else
	    goto not_there;
#endif
	if (strEQ(name, "AnyModifier"))
#ifdef AnyModifier
	    return AnyModifier;
#else
	    goto not_there;
#endif
	if (strEQ(name, "AnyPropertyType"))
#ifdef AnyPropertyType
	    return AnyPropertyType;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ArcChord"))
#ifdef ArcChord
	    return ArcChord;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ArcPieSlice"))
#ifdef ArcPieSlice
	    return ArcPieSlice;
#else
	    goto not_there;
#endif
	if (strEQ(name, "AsyncBoth"))
#ifdef AsyncBoth
	    return AsyncBoth;
#else
	    goto not_there;
#endif
	if (strEQ(name, "AsyncKeyboard"))
#ifdef AsyncKeyboard
	    return AsyncKeyboard;
#else
	    goto not_there;
#endif
	if (strEQ(name, "AsyncPointer"))
#ifdef AsyncPointer
	    return AsyncPointer;
#else
	    goto not_there;
#endif
	if (strEQ(name, "AutoRepeatModeDefault"))
#ifdef AutoRepeatModeDefault
	    return AutoRepeatModeDefault;
#else
	    goto not_there;
#endif
	if (strEQ(name, "AutoRepeatModeOff"))
#ifdef AutoRepeatModeOff
	    return AutoRepeatModeOff;
#else
	    goto not_there;
#endif
	if (strEQ(name, "AutoRepeatModeOn"))
#ifdef AutoRepeatModeOn
	    return AutoRepeatModeOn;
#else
	    goto not_there;
#endif
	break;
    case 'B':
	if (strEQ(name, "BadAccess"))
#ifdef BadAccess
	    return BadAccess;
#else
	    goto not_there;
#endif
	if (strEQ(name, "BadAlloc"))
#ifdef BadAlloc
	    return BadAlloc;
#else
	    goto not_there;
#endif
	if (strEQ(name, "BadAtom"))
#ifdef BadAtom
	    return BadAtom;
#else
	    goto not_there;
#endif
	if (strEQ(name, "BadColor"))
#ifdef BadColor
	    return BadColor;
#else
	    goto not_there;
#endif
	if (strEQ(name, "BadCursor"))
#ifdef BadCursor
	    return BadCursor;
#else
	    goto not_there;
#endif
	if (strEQ(name, "BadDrawable"))
#ifdef BadDrawable
	    return BadDrawable;
#else
	    goto not_there;
#endif
	if (strEQ(name, "BadFont"))
#ifdef BadFont
	    return BadFont;
#else
	    goto not_there;
#endif
	if (strEQ(name, "BadGC"))
#ifdef BadGC
	    return BadGC;
#else
	    goto not_there;
#endif
	if (strEQ(name, "BadIDChoice"))
#ifdef BadIDChoice
	    return BadIDChoice;
#else
	    goto not_there;
#endif
	if (strEQ(name, "BadImplementation"))
#ifdef BadImplementation
	    return BadImplementation;
#else
	    goto not_there;
#endif
	if (strEQ(name, "BadLength"))
#ifdef BadLength
	    return BadLength;
#else
	    goto not_there;
#endif
	if (strEQ(name, "BadMatch"))
#ifdef BadMatch
	    return BadMatch;
#else
	    goto not_there;
#endif
	if (strEQ(name, "BadName"))
#ifdef BadName
	    return BadName;
#else
	    goto not_there;
#endif
	if (strEQ(name, "BadPixmap"))
#ifdef BadPixmap
	    return BadPixmap;
#else
	    goto not_there;
#endif
	if (strEQ(name, "BadRequest"))
#ifdef BadRequest
	    return BadRequest;
#else
	    goto not_there;
#endif
	if (strEQ(name, "BadValue"))
#ifdef BadValue
	    return BadValue;
#else
	    goto not_there;
#endif
	if (strEQ(name, "BadWindow"))
#ifdef BadWindow
	    return BadWindow;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Below"))
#ifdef Below
	    return Below;
#else
	    goto not_there;
#endif
	if (strEQ(name, "BottomIf"))
#ifdef BottomIf
	    return BottomIf;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Button1"))
#ifdef Button1
	    return Button1;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Button1Mask"))
#ifdef Button1Mask
	    return Button1Mask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Button1MotionMask"))
#ifdef Button1MotionMask
	    return Button1MotionMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Button2"))
#ifdef Button2
	    return Button2;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Button2Mask"))
#ifdef Button2Mask
	    return Button2Mask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Button2MotionMask"))
#ifdef Button2MotionMask
	    return Button2MotionMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Button3"))
#ifdef Button3
	    return Button3;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Button3Mask"))
#ifdef Button3Mask
	    return Button3Mask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Button3MotionMask"))
#ifdef Button3MotionMask
	    return Button3MotionMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Button4"))
#ifdef Button4
	    return Button4;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Button4Mask"))
#ifdef Button4Mask
	    return Button4Mask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Button4MotionMask"))
#ifdef Button4MotionMask
	    return Button4MotionMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Button5"))
#ifdef Button5
	    return Button5;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Button5Mask"))
#ifdef Button5Mask
	    return Button5Mask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Button5MotionMask"))
#ifdef Button5MotionMask
	    return Button5MotionMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ButtonMotionMask"))
#ifdef ButtonMotionMask
	    return ButtonMotionMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ButtonPress"))
#ifdef ButtonPress
	    return ButtonPress;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ButtonPressMask"))
#ifdef ButtonPressMask
	    return ButtonPressMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ButtonRelease"))
#ifdef ButtonRelease
	    return ButtonRelease;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ButtonReleaseMask"))
#ifdef ButtonReleaseMask
	    return ButtonReleaseMask;
#else
	    goto not_there;
#endif
	break;
    case 'C':
	if (strEQ(name, "CWBackPixel"))
#ifdef CWBackPixel
	    return CWBackPixel;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CWBackPixmap"))
#ifdef CWBackPixmap
	    return CWBackPixmap;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CWBackingPixel"))
#ifdef CWBackingPixel
	    return CWBackingPixel;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CWBackingPlanes"))
#ifdef CWBackingPlanes
	    return CWBackingPlanes;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CWBackingStore"))
#ifdef CWBackingStore
	    return CWBackingStore;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CWBitGravity"))
#ifdef CWBitGravity
	    return CWBitGravity;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CWBorderPixel"))
#ifdef CWBorderPixel
	    return CWBorderPixel;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CWBorderPixmap"))
#ifdef CWBorderPixmap
	    return CWBorderPixmap;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CWBorderWidth"))
#ifdef CWBorderWidth
	    return CWBorderWidth;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CWColormap"))
#ifdef CWColormap
	    return CWColormap;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CWCursor"))
#ifdef CWCursor
	    return CWCursor;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CWDontPropagate"))
#ifdef CWDontPropagate
	    return CWDontPropagate;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CWEventMask"))
#ifdef CWEventMask
	    return CWEventMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CWHeight"))
#ifdef CWHeight
	    return CWHeight;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CWOverrideRedirect"))
#ifdef CWOverrideRedirect
	    return CWOverrideRedirect;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CWSaveUnder"))
#ifdef CWSaveUnder
	    return CWSaveUnder;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CWSibling"))
#ifdef CWSibling
	    return CWSibling;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CWStackMode"))
#ifdef CWStackMode
	    return CWStackMode;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CWWidth"))
#ifdef CWWidth
	    return CWWidth;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CWWinGravity"))
#ifdef CWWinGravity
	    return CWWinGravity;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CWX"))
#ifdef CWX
	    return CWX;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CWY"))
#ifdef CWY
	    return CWY;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CapButt"))
#ifdef CapButt
	    return CapButt;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CapNotLast"))
#ifdef CapNotLast
	    return CapNotLast;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CapProjecting"))
#ifdef CapProjecting
	    return CapProjecting;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CapRound"))
#ifdef CapRound
	    return CapRound;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CenterGravity"))
#ifdef CenterGravity
	    return CenterGravity;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CirculateNotify"))
#ifdef CirculateNotify
	    return CirculateNotify;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CirculateRequest"))
#ifdef CirculateRequest
	    return CirculateRequest;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ClientMessage"))
#ifdef ClientMessage
	    return ClientMessage;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ClipByChildren"))
#ifdef ClipByChildren
	    return ClipByChildren;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ColormapChangeMask"))
#ifdef ColormapChangeMask
	    return ColormapChangeMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ColormapInstalled"))
#ifdef ColormapInstalled
	    return ColormapInstalled;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ColormapNotify"))
#ifdef ColormapNotify
	    return ColormapNotify;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ColormapUninstalled"))
#ifdef ColormapUninstalled
	    return ColormapUninstalled;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Complex"))
#ifdef Complex
	    return Complex;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ConfigureNotify"))
#ifdef ConfigureNotify
	    return ConfigureNotify;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ConfigureRequest"))
#ifdef ConfigureRequest
	    return ConfigureRequest;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ControlMapIndex"))
#ifdef ControlMapIndex
	    return ControlMapIndex;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ControlMask"))
#ifdef ControlMask
	    return ControlMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Convex"))
#ifdef Convex
	    return Convex;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CoordModeOrigin"))
#ifdef CoordModeOrigin
	    return CoordModeOrigin;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CoordModePrevious"))
#ifdef CoordModePrevious
	    return CoordModePrevious;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CopyFromParent"))
#ifdef CopyFromParent
	    return CopyFromParent;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CreateNotify"))
#ifdef CreateNotify
	    return CreateNotify;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CurrentTime"))
#ifdef CurrentTime
	    return CurrentTime;
#else
	    goto not_there;
#endif
	if (strEQ(name, "CursorShape"))
#ifdef CursorShape
	    return CursorShape;
#else
	    goto not_there;
#endif
	break;
    case 'D':
	if (strEQ(name, "DefaultBlanking"))
#ifdef DefaultBlanking
	    return DefaultBlanking;
#else
	    goto not_there;
#endif
	if (strEQ(name, "DefaultExposures"))
#ifdef DefaultExposures
	    return DefaultExposures;
#else
	    goto not_there;
#endif
	if (strEQ(name, "DestroyAll"))
#ifdef DestroyAll
	    return DestroyAll;
#else
	    goto not_there;
#endif
	if (strEQ(name, "DestroyNotify"))
#ifdef DestroyNotify
	    return DestroyNotify;
#else
	    goto not_there;
#endif
	if (strEQ(name, "DirectColor"))
#ifdef DirectColor
	    return DirectColor;
#else
	    goto not_there;
#endif
	if (strEQ(name, "DisableAccess"))
#ifdef DisableAccess
	    return DisableAccess;
#else
	    goto not_there;
#endif
	if (strEQ(name, "DisableScreenInterval"))
#ifdef DisableScreenInterval
	    return DisableScreenInterval;
#else
	    goto not_there;
#endif
	if (strEQ(name, "DisableScreenSaver"))
#ifdef DisableScreenSaver
	    return DisableScreenSaver;
#else
	    goto not_there;
#endif
	if (strEQ(name, "DoBlue"))
#ifdef DoBlue
	    return DoBlue;
#else
	    goto not_there;
#endif
	if (strEQ(name, "DoGreen"))
#ifdef DoGreen
	    return DoGreen;
#else
	    goto not_there;
#endif
	if (strEQ(name, "DoRed"))
#ifdef DoRed
	    return DoRed;
#else
	    goto not_there;
#endif
	if (strEQ(name, "DontAllowExposures"))
#ifdef DontAllowExposures
	    return DontAllowExposures;
#else
	    goto not_there;
#endif
	if (strEQ(name, "DontPreferBlanking"))
#ifdef DontPreferBlanking
	    return DontPreferBlanking;
#else
	    goto not_there;
#endif
	break;
    case 'E':
	if (strEQ(name, "EastGravity"))
#ifdef EastGravity
	    return EastGravity;
#else
	    goto not_there;
#endif
	if (strEQ(name, "EnableAccess"))
#ifdef EnableAccess
	    return EnableAccess;
#else
	    goto not_there;
#endif
	if (strEQ(name, "EnterNotify"))
#ifdef EnterNotify
	    return EnterNotify;
#else
	    goto not_there;
#endif
	if (strEQ(name, "EnterWindowMask"))
#ifdef EnterWindowMask
	    return EnterWindowMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "EvenOddRule"))
#ifdef EvenOddRule
	    return EvenOddRule;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Expose"))
#ifdef Expose
	    return Expose;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ExposureMask"))
#ifdef ExposureMask
	    return ExposureMask;
#else
	    goto not_there;
#endif
	break;
    case 'F':
	if (strEQ(name, "FamilyChaos"))
#ifdef FamilyChaos
	    return FamilyChaos;
#else
	    goto not_there;
#endif
	if (strEQ(name, "FamilyDECnet"))
#ifdef FamilyDECnet
	    return FamilyDECnet;
#else
	    goto not_there;
#endif
	if (strEQ(name, "FamilyInternet"))
#ifdef FamilyInternet
	    return FamilyInternet;
#else
	    goto not_there;
#endif
	if (strEQ(name, "FillOpaqueStippled"))
#ifdef FillOpaqueStippled
	    return FillOpaqueStippled;
#else
	    goto not_there;
#endif
	if (strEQ(name, "FillSolid"))
#ifdef FillSolid
	    return FillSolid;
#else
	    goto not_there;
#endif
	if (strEQ(name, "FillStippled"))
#ifdef FillStippled
	    return FillStippled;
#else
	    goto not_there;
#endif
	if (strEQ(name, "FillTiled"))
#ifdef FillTiled
	    return FillTiled;
#else
	    goto not_there;
#endif
	if (strEQ(name, "FirstExtensionError"))
#ifdef FirstExtensionError
	    return FirstExtensionError;
#else
	    goto not_there;
#endif
	if (strEQ(name, "FocusChangeMask"))
#ifdef FocusChangeMask
	    return FocusChangeMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "FocusIn"))
#ifdef FocusIn
	    return FocusIn;
#else
	    goto not_there;
#endif
	if (strEQ(name, "FocusOut"))
#ifdef FocusOut
	    return FocusOut;
#else
	    goto not_there;
#endif
	if (strEQ(name, "FontChange"))
#ifdef FontChange
	    return FontChange;
#else
	    goto not_there;
#endif
	if (strEQ(name, "FontLeftToRight"))
#ifdef FontLeftToRight
	    return FontLeftToRight;
#else
	    goto not_there;
#endif
	if (strEQ(name, "FontRightToLeft"))
#ifdef FontRightToLeft
	    return FontRightToLeft;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ForgetGravity"))
#ifdef ForgetGravity
	    return ForgetGravity;
#else
	    goto not_there;
#endif
	break;
    case 'G':
	if (strEQ(name, "GCArcMode"))
#ifdef GCArcMode
	    return GCArcMode;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GCBackground"))
#ifdef GCBackground
	    return GCBackground;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GCCapStyle"))
#ifdef GCCapStyle
	    return GCCapStyle;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GCClipMask"))
#ifdef GCClipMask
	    return GCClipMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GCClipXOrigin"))
#ifdef GCClipXOrigin
	    return GCClipXOrigin;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GCClipYOrigin"))
#ifdef GCClipYOrigin
	    return GCClipYOrigin;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GCDashList"))
#ifdef GCDashList
	    return GCDashList;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GCDashOffset"))
#ifdef GCDashOffset
	    return GCDashOffset;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GCFillRule"))
#ifdef GCFillRule
	    return GCFillRule;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GCFillStyle"))
#ifdef GCFillStyle
	    return GCFillStyle;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GCFont"))
#ifdef GCFont
	    return GCFont;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GCForeground"))
#ifdef GCForeground
	    return GCForeground;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GCFunction"))
#ifdef GCFunction
	    return GCFunction;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GCGraphicsExposures"))
#ifdef GCGraphicsExposures
	    return GCGraphicsExposures;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GCJoinStyle"))
#ifdef GCJoinStyle
	    return GCJoinStyle;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GCLastBit"))
#ifdef GCLastBit
	    return GCLastBit;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GCLineStyle"))
#ifdef GCLineStyle
	    return GCLineStyle;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GCLineWidth"))
#ifdef GCLineWidth
	    return GCLineWidth;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GCPlaneMask"))
#ifdef GCPlaneMask
	    return GCPlaneMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GCStipple"))
#ifdef GCStipple
	    return GCStipple;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GCSubwindowMode"))
#ifdef GCSubwindowMode
	    return GCSubwindowMode;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GCTile"))
#ifdef GCTile
	    return GCTile;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GCTileStipXOrigin"))
#ifdef GCTileStipXOrigin
	    return GCTileStipXOrigin;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GCTileStipYOrigin"))
#ifdef GCTileStipYOrigin
	    return GCTileStipYOrigin;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GXand"))
#ifdef GXand
	    return GXand;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GXandInverted"))
#ifdef GXandInverted
	    return GXandInverted;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GXandReverse"))
#ifdef GXandReverse
	    return GXandReverse;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GXclear"))
#ifdef GXclear
	    return GXclear;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GXcopy"))
#ifdef GXcopy
	    return GXcopy;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GXcopyInverted"))
#ifdef GXcopyInverted
	    return GXcopyInverted;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GXequiv"))
#ifdef GXequiv
	    return GXequiv;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GXinvert"))
#ifdef GXinvert
	    return GXinvert;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GXnand"))
#ifdef GXnand
	    return GXnand;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GXnoop"))
#ifdef GXnoop
	    return GXnoop;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GXnor"))
#ifdef GXnor
	    return GXnor;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GXor"))
#ifdef GXor
	    return GXor;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GXorInverted"))
#ifdef GXorInverted
	    return GXorInverted;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GXorReverse"))
#ifdef GXorReverse
	    return GXorReverse;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GXset"))
#ifdef GXset
	    return GXset;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GXxor"))
#ifdef GXxor
	    return GXxor;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GrabFrozen"))
#ifdef GrabFrozen
	    return GrabFrozen;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GrabInvalidTime"))
#ifdef GrabInvalidTime
	    return GrabInvalidTime;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GrabModeAsync"))
#ifdef GrabModeAsync
	    return GrabModeAsync;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GrabModeSync"))
#ifdef GrabModeSync
	    return GrabModeSync;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GrabNotViewable"))
#ifdef GrabNotViewable
	    return GrabNotViewable;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GrabSuccess"))
#ifdef GrabSuccess
	    return GrabSuccess;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GraphicsExpose"))
#ifdef GraphicsExpose
	    return GraphicsExpose;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GravityNotify"))
#ifdef GravityNotify
	    return GravityNotify;
#else
	    goto not_there;
#endif
	if (strEQ(name, "GrayScale"))
#ifdef GrayScale
	    return GrayScale;
#else
	    goto not_there;
#endif
	break;
    case 'H':
	if (strEQ(name, "HostDelete"))
#ifdef HostDelete
	    return HostDelete;
#else
	    goto not_there;
#endif
	if (strEQ(name, "HostInsert"))
#ifdef HostInsert
	    return HostInsert;
#else
	    goto not_there;
#endif
	break;
    case 'I':
	if (strEQ(name, "IncludeInferiors"))
#ifdef IncludeInferiors
	    return IncludeInferiors;
#else
	    goto not_there;
#endif
	if (strEQ(name, "InputFocus"))
#ifdef InputFocus
	    return InputFocus;
#else
	    goto not_there;
#endif
	if (strEQ(name, "InputOnly"))
#ifdef InputOnly
	    return InputOnly;
#else
	    goto not_there;
#endif
	if (strEQ(name, "InputOutput"))
#ifdef InputOutput
	    return InputOutput;
#else
	    goto not_there;
#endif
	if (strEQ(name, "IsUnmapped"))
#ifdef IsUnmapped
	    return IsUnmapped;
#else
	    goto not_there;
#endif
	if (strEQ(name, "IsUnviewable"))
#ifdef IsUnviewable
	    return IsUnviewable;
#else
	    goto not_there;
#endif
	if (strEQ(name, "IsViewable"))
#ifdef IsViewable
	    return IsViewable;
#else
	    goto not_there;
#endif
	break;
    case 'J':
	if (strEQ(name, "JoinBevel"))
#ifdef JoinBevel
	    return JoinBevel;
#else
	    goto not_there;
#endif
	if (strEQ(name, "JoinMiter"))
#ifdef JoinMiter
	    return JoinMiter;
#else
	    goto not_there;
#endif
	if (strEQ(name, "JoinRound"))
#ifdef JoinRound
	    return JoinRound;
#else
	    goto not_there;
#endif
	break;
    case 'K':
	if (strEQ(name, "KBAutoRepeatMode"))
#ifdef KBAutoRepeatMode
	    return KBAutoRepeatMode;
#else
	    goto not_there;
#endif
	if (strEQ(name, "KBBellDuration"))
#ifdef KBBellDuration
	    return KBBellDuration;
#else
	    goto not_there;
#endif
	if (strEQ(name, "KBBellPercent"))
#ifdef KBBellPercent
	    return KBBellPercent;
#else
	    goto not_there;
#endif
	if (strEQ(name, "KBBellPitch"))
#ifdef KBBellPitch
	    return KBBellPitch;
#else
	    goto not_there;
#endif
	if (strEQ(name, "KBKey"))
#ifdef KBKey
	    return KBKey;
#else
	    goto not_there;
#endif
	if (strEQ(name, "KBKeyClickPercent"))
#ifdef KBKeyClickPercent
	    return KBKeyClickPercent;
#else
	    goto not_there;
#endif
	if (strEQ(name, "KBLed"))
#ifdef KBLed
	    return KBLed;
#else
	    goto not_there;
#endif
	if (strEQ(name, "KBLedMode"))
#ifdef KBLedMode
	    return KBLedMode;
#else
	    goto not_there;
#endif
	if (strEQ(name, "KeyPress"))
#ifdef KeyPress
	    return KeyPress;
#else
	    goto not_there;
#endif
	if (strEQ(name, "KeyPressMask"))
#ifdef KeyPressMask
	    return KeyPressMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "KeyRelease"))
#ifdef KeyRelease
	    return KeyRelease;
#else
	    goto not_there;
#endif
	if (strEQ(name, "KeyReleaseMask"))
#ifdef KeyReleaseMask
	    return KeyReleaseMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "KeymapNotify"))
#ifdef KeymapNotify
	    return KeymapNotify;
#else
	    goto not_there;
#endif
	if (strEQ(name, "KeymapStateMask"))
#ifdef KeymapStateMask
	    return KeymapStateMask;
#else
	    goto not_there;
#endif
	break;
    case 'L':
	if (strEQ(name, "LASTEvent"))
#ifdef LASTEvent
	    return LASTEvent;
#else
	    goto not_there;
#endif
	if (strEQ(name, "LSBFirst"))
#ifdef LSBFirst
	    return LSBFirst;
#else
	    goto not_there;
#endif
	if (strEQ(name, "LastExtensionError"))
#ifdef LastExtensionError
	    return LastExtensionError;
#else
	    goto not_there;
#endif
	if (strEQ(name, "LeaveNotify"))
#ifdef LeaveNotify
	    return LeaveNotify;
#else
	    goto not_there;
#endif
	if (strEQ(name, "LeaveWindowMask"))
#ifdef LeaveWindowMask
	    return LeaveWindowMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "LedModeOff"))
#ifdef LedModeOff
	    return LedModeOff;
#else
	    goto not_there;
#endif
	if (strEQ(name, "LedModeOn"))
#ifdef LedModeOn
	    return LedModeOn;
#else
	    goto not_there;
#endif
	if (strEQ(name, "LineDoubleDash"))
#ifdef LineDoubleDash
	    return LineDoubleDash;
#else
	    goto not_there;
#endif
	if (strEQ(name, "LineOnOffDash"))
#ifdef LineOnOffDash
	    return LineOnOffDash;
#else
	    goto not_there;
#endif
	if (strEQ(name, "LineSolid"))
#ifdef LineSolid
	    return LineSolid;
#else
	    goto not_there;
#endif
	if (strEQ(name, "LockMapIndex"))
#ifdef LockMapIndex
	    return LockMapIndex;
#else
	    goto not_there;
#endif
	if (strEQ(name, "LockMask"))
#ifdef LockMask
	    return LockMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "LowerHighest"))
#ifdef LowerHighest
	    return LowerHighest;
#else
	    goto not_there;
#endif
	break;
    case 'M':
	if (strEQ(name, "MSBFirst"))
#ifdef MSBFirst
	    return MSBFirst;
#else
	    goto not_there;
#endif
	if (strEQ(name, "MapNotify"))
#ifdef MapNotify
	    return MapNotify;
#else
	    goto not_there;
#endif
	if (strEQ(name, "MapRequest"))
#ifdef MapRequest
	    return MapRequest;
#else
	    goto not_there;
#endif
	if (strEQ(name, "MappingBusy"))
#ifdef MappingBusy
	    return MappingBusy;
#else
	    goto not_there;
#endif
	if (strEQ(name, "MappingFailed"))
#ifdef MappingFailed
	    return MappingFailed;
#else
	    goto not_there;
#endif
	if (strEQ(name, "MappingKeyboard"))
#ifdef MappingKeyboard
	    return MappingKeyboard;
#else
	    goto not_there;
#endif
	if (strEQ(name, "MappingModifier"))
#ifdef MappingModifier
	    return MappingModifier;
#else
	    goto not_there;
#endif
	if (strEQ(name, "MappingNotify"))
#ifdef MappingNotify
	    return MappingNotify;
#else
	    goto not_there;
#endif
	if (strEQ(name, "MappingPointer"))
#ifdef MappingPointer
	    return MappingPointer;
#else
	    goto not_there;
#endif
	if (strEQ(name, "MappingSuccess"))
#ifdef MappingSuccess
	    return MappingSuccess;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Mod1MapIndex"))
#ifdef Mod1MapIndex
	    return Mod1MapIndex;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Mod1Mask"))
#ifdef Mod1Mask
	    return Mod1Mask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Mod2MapIndex"))
#ifdef Mod2MapIndex
	    return Mod2MapIndex;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Mod2Mask"))
#ifdef Mod2Mask
	    return Mod2Mask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Mod3MapIndex"))
#ifdef Mod3MapIndex
	    return Mod3MapIndex;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Mod3Mask"))
#ifdef Mod3Mask
	    return Mod3Mask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Mod4MapIndex"))
#ifdef Mod4MapIndex
	    return Mod4MapIndex;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Mod4Mask"))
#ifdef Mod4Mask
	    return Mod4Mask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Mod5MapIndex"))
#ifdef Mod5MapIndex
	    return Mod5MapIndex;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Mod5Mask"))
#ifdef Mod5Mask
	    return Mod5Mask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "MotionNotify"))
#ifdef MotionNotify
	    return MotionNotify;
#else
	    goto not_there;
#endif
	break;
    case 'N':
	if (strEQ(name, "NoEventMask"))
#ifdef NoEventMask
	    return NoEventMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "NoExpose"))
#ifdef NoExpose
	    return NoExpose;
#else
	    goto not_there;
#endif
	if (strEQ(name, "NoSymbol"))
#ifdef NoSymbol
	    return NoSymbol;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Nonconvex"))
#ifdef Nonconvex
	    return Nonconvex;
#else
	    goto not_there;
#endif
	if (strEQ(name, "None"))
#ifdef None
	    return None;
#else
	    goto not_there;
#endif
	if (strEQ(name, "NorthEastGravity"))
#ifdef NorthEastGravity
	    return NorthEastGravity;
#else
	    goto not_there;
#endif
	if (strEQ(name, "NorthGravity"))
#ifdef NorthGravity
	    return NorthGravity;
#else
	    goto not_there;
#endif
	if (strEQ(name, "NorthWestGravity"))
#ifdef NorthWestGravity
	    return NorthWestGravity;
#else
	    goto not_there;
#endif
	if (strEQ(name, "NotUseful"))
#ifdef NotUseful
	    return NotUseful;
#else
	    goto not_there;
#endif
	if (strEQ(name, "NotifyAncestor"))
#ifdef NotifyAncestor
	    return NotifyAncestor;
#else
	    goto not_there;
#endif
	if (strEQ(name, "NotifyDetailNone"))
#ifdef NotifyDetailNone
	    return NotifyDetailNone;
#else
	    goto not_there;
#endif
	if (strEQ(name, "NotifyGrab"))
#ifdef NotifyGrab
	    return NotifyGrab;
#else
	    goto not_there;
#endif
	if (strEQ(name, "NotifyHint"))
#ifdef NotifyHint
	    return NotifyHint;
#else
	    goto not_there;
#endif
	if (strEQ(name, "NotifyInferior"))
#ifdef NotifyInferior
	    return NotifyInferior;
#else
	    goto not_there;
#endif
	if (strEQ(name, "NotifyNonlinear"))
#ifdef NotifyNonlinear
	    return NotifyNonlinear;
#else
	    goto not_there;
#endif
	if (strEQ(name, "NotifyNonlinearVirtual"))
#ifdef NotifyNonlinearVirtual
	    return NotifyNonlinearVirtual;
#else
	    goto not_there;
#endif
	if (strEQ(name, "NotifyNormal"))
#ifdef NotifyNormal
	    return NotifyNormal;
#else
	    goto not_there;
#endif
	if (strEQ(name, "NotifyPointer"))
#ifdef NotifyPointer
	    return NotifyPointer;
#else
	    goto not_there;
#endif
	if (strEQ(name, "NotifyPointerRoot"))
#ifdef NotifyPointerRoot
	    return NotifyPointerRoot;
#else
	    goto not_there;
#endif
	if (strEQ(name, "NotifyUngrab"))
#ifdef NotifyUngrab
	    return NotifyUngrab;
#else
	    goto not_there;
#endif
	if (strEQ(name, "NotifyVirtual"))
#ifdef NotifyVirtual
	    return NotifyVirtual;
#else
	    goto not_there;
#endif
	if (strEQ(name, "NotifyWhileGrabbed"))
#ifdef NotifyWhileGrabbed
	    return NotifyWhileGrabbed;
#else
	    goto not_there;
#endif
	break;
    case 'O':
	if (strEQ(name, "Opposite"))
#ifdef Opposite
	    return Opposite;
#else
	    goto not_there;
#endif
	if (strEQ(name, "OwnerGrabButtonMask"))
#ifdef OwnerGrabButtonMask
	    return OwnerGrabButtonMask;
#else
	    goto not_there;
#endif
	break;
    case 'P':
	if (strEQ(name, "ParentRelative"))
#ifdef ParentRelative
	    return ParentRelative;
#else
	    goto not_there;
#endif
	if (strEQ(name, "PlaceOnBottom"))
#ifdef PlaceOnBottom
	    return PlaceOnBottom;
#else
	    goto not_there;
#endif
	if (strEQ(name, "PlaceOnTop"))
#ifdef PlaceOnTop
	    return PlaceOnTop;
#else
	    goto not_there;
#endif
	if (strEQ(name, "PointerMotionHintMask"))
#ifdef PointerMotionHintMask
	    return PointerMotionHintMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "PointerMotionMask"))
#ifdef PointerMotionMask
	    return PointerMotionMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "PointerRoot"))
#ifdef PointerRoot
	    return PointerRoot;
#else
	    goto not_there;
#endif
	if (strEQ(name, "PointerWindow"))
#ifdef PointerWindow
	    return PointerWindow;
#else
	    goto not_there;
#endif
	if (strEQ(name, "PreferBlanking"))
#ifdef PreferBlanking
	    return PreferBlanking;
#else
	    goto not_there;
#endif
	if (strEQ(name, "PropModeAppend"))
#ifdef PropModeAppend
	    return PropModeAppend;
#else
	    goto not_there;
#endif
	if (strEQ(name, "PropModePrepend"))
#ifdef PropModePrepend
	    return PropModePrepend;
#else
	    goto not_there;
#endif
	if (strEQ(name, "PropModeReplace"))
#ifdef PropModeReplace
	    return PropModeReplace;
#else
	    goto not_there;
#endif
	if (strEQ(name, "PropertyChangeMask"))
#ifdef PropertyChangeMask
	    return PropertyChangeMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "PropertyDelete"))
#ifdef PropertyDelete
	    return PropertyDelete;
#else
	    goto not_there;
#endif
	if (strEQ(name, "PropertyNewValue"))
#ifdef PropertyNewValue
	    return PropertyNewValue;
#else
	    goto not_there;
#endif
	if (strEQ(name, "PropertyNotify"))
#ifdef PropertyNotify
	    return PropertyNotify;
#else
	    goto not_there;
#endif
	if (strEQ(name, "PseudoColor"))
#ifdef PseudoColor
	    return PseudoColor;
#else
	    goto not_there;
#endif
	break;
    case 'Q':
	break;
    case 'R':
	if (strEQ(name, "RaiseLowest"))
#ifdef RaiseLowest
	    return RaiseLowest;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ReparentNotify"))
#ifdef ReparentNotify
	    return ReparentNotify;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ReplayKeyboard"))
#ifdef ReplayKeyboard
	    return ReplayKeyboard;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ReplayPointer"))
#ifdef ReplayPointer
	    return ReplayPointer;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ResizeRedirectMask"))
#ifdef ResizeRedirectMask
	    return ResizeRedirectMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ResizeRequest"))
#ifdef ResizeRequest
	    return ResizeRequest;
#else
	    goto not_there;
#endif
	if (strEQ(name, "RetainPermanent"))
#ifdef RetainPermanent
	    return RetainPermanent;
#else
	    goto not_there;
#endif
	if (strEQ(name, "RetainTemporary"))
#ifdef RetainTemporary
	    return RetainTemporary;
#else
	    goto not_there;
#endif
	if (strEQ(name, "RevertToNone"))
#ifdef RevertToNone
	    return RevertToNone;
#else
	    goto not_there;
#endif
	if (strEQ(name, "RevertToParent"))
#ifdef RevertToParent
	    return RevertToParent;
#else
	    goto not_there;
#endif
	if (strEQ(name, "RevertToPointerRoot"))
#ifdef RevertToPointerRoot
	    return RevertToPointerRoot;
#else
	    goto not_there;
#endif
	break;
    case 'S':
	if (strEQ(name, "ScreenSaverActive"))
#ifdef ScreenSaverActive
	    return ScreenSaverActive;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ScreenSaverReset"))
#ifdef ScreenSaverReset
	    return ScreenSaverReset;
#else
	    goto not_there;
#endif
	if (strEQ(name, "SelectionClear"))
#ifdef SelectionClear
	    return SelectionClear;
#else
	    goto not_there;
#endif
	if (strEQ(name, "SelectionNotify"))
#ifdef SelectionNotify
	    return SelectionNotify;
#else
	    goto not_there;
#endif
	if (strEQ(name, "SelectionRequest"))
#ifdef SelectionRequest
	    return SelectionRequest;
#else
	    goto not_there;
#endif
	if (strEQ(name, "SetModeDelete"))
#ifdef SetModeDelete
	    return SetModeDelete;
#else
	    goto not_there;
#endif
	if (strEQ(name, "SetModeInsert"))
#ifdef SetModeInsert
	    return SetModeInsert;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ShiftMapIndex"))
#ifdef ShiftMapIndex
	    return ShiftMapIndex;
#else
	    goto not_there;
#endif
	if (strEQ(name, "ShiftMask"))
#ifdef ShiftMask
	    return ShiftMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "SouthEastGravity"))
#ifdef SouthEastGravity
	    return SouthEastGravity;
#else
	    goto not_there;
#endif
	if (strEQ(name, "SouthGravity"))
#ifdef SouthGravity
	    return SouthGravity;
#else
	    goto not_there;
#endif
	if (strEQ(name, "SouthWestGravity"))
#ifdef SouthWestGravity
	    return SouthWestGravity;
#else
	    goto not_there;
#endif
	if (strEQ(name, "StaticColor"))
#ifdef StaticColor
	    return StaticColor;
#else
	    goto not_there;
#endif
	if (strEQ(name, "StaticGravity"))
#ifdef StaticGravity
	    return StaticGravity;
#else
	    goto not_there;
#endif
	if (strEQ(name, "StaticGray"))
#ifdef StaticGray
	    return StaticGray;
#else
	    goto not_there;
#endif
	if (strEQ(name, "StippleShape"))
#ifdef StippleShape
	    return StippleShape;
#else
	    goto not_there;
#endif
	if (strEQ(name, "StructureNotifyMask"))
#ifdef StructureNotifyMask
	    return StructureNotifyMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "SubstructureNotifyMask"))
#ifdef SubstructureNotifyMask
	    return SubstructureNotifyMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "SubstructureRedirectMask"))
#ifdef SubstructureRedirectMask
	    return SubstructureRedirectMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Success"))
#ifdef Success
	    return Success;
#else
	    goto not_there;
#endif
	if (strEQ(name, "SyncBoth"))
#ifdef SyncBoth
	    return SyncBoth;
#else
	    goto not_there;
#endif
	if (strEQ(name, "SyncKeyboard"))
#ifdef SyncKeyboard
	    return SyncKeyboard;
#else
	    goto not_there;
#endif
	if (strEQ(name, "SyncPointer"))
#ifdef SyncPointer
	    return SyncPointer;
#else
	    goto not_there;
#endif
	break;
    case 'T':
	if (strEQ(name, "TileShape"))
#ifdef TileShape
	    return TileShape;
#else
	    goto not_there;
#endif
	if (strEQ(name, "TopIf"))
#ifdef TopIf
	    return TopIf;
#else
	    goto not_there;
#endif
	if (strEQ(name, "TrueColor"))
#ifdef TrueColor
	    return TrueColor;
#else
	    goto not_there;
#endif
	break;
    case 'U':
	if (strEQ(name, "UnmapGravity"))
#ifdef UnmapGravity
	    return UnmapGravity;
#else
	    goto not_there;
#endif
	if (strEQ(name, "UnmapNotify"))
#ifdef UnmapNotify
	    return UnmapNotify;
#else
	    goto not_there;
#endif
	if (strEQ(name, "Unsorted"))
#ifdef Unsorted
	    return Unsorted;
#else
	    goto not_there;
#endif
	break;
    case 'V':
	if (strEQ(name, "VisibilityChangeMask"))
#ifdef VisibilityChangeMask
	    return VisibilityChangeMask;
#else
	    goto not_there;
#endif
	if (strEQ(name, "VisibilityFullyObscured"))
#ifdef VisibilityFullyObscured
	    return VisibilityFullyObscured;
#else
	    goto not_there;
#endif
	if (strEQ(name, "VisibilityNotify"))
#ifdef VisibilityNotify
	    return VisibilityNotify;
#else
	    goto not_there;
#endif
	if (strEQ(name, "VisibilityPartiallyObscured"))
#ifdef VisibilityPartiallyObscured
	    return VisibilityPartiallyObscured;
#else
	    goto not_there;
#endif
	if (strEQ(name, "VisibilityUnobscured"))
#ifdef VisibilityUnobscured
	    return VisibilityUnobscured;
#else
	    goto not_there;
#endif
	break;
    case 'W':
	if (strEQ(name, "WestGravity"))
#ifdef WestGravity
	    return WestGravity;
#else
	    goto not_there;
#endif
	if (strEQ(name, "WhenMapped"))
#ifdef WhenMapped
	    return WhenMapped;
#else
	    goto not_there;
#endif
	if (strEQ(name, "WindingRule"))
#ifdef WindingRule
	    return WindingRule;
#else
	    goto not_there;
#endif
	break;
    case 'X':
	if (strEQ(name, "XYBitmap"))
#ifdef XYBitmap
	    return XYBitmap;
#else
	    goto not_there;
#endif
	if (strEQ(name, "XYPixmap"))
#ifdef XYPixmap
	    return XYPixmap;
#else
	    goto not_there;
#endif
	if (strEQ(name, "X_PROTOCOL"))
#ifdef X_PROTOCOL
	    return X_PROTOCOL;
#else
	    goto not_there;
#endif
	if (strEQ(name, "X_PROTOCOL_REVISION"))
#ifdef X_PROTOCOL_REVISION
	    return X_PROTOCOL_REVISION;
#else
	    goto not_there;
#endif
	break;
    case 'Y':
	if (strEQ(name, "YSorted"))
#ifdef YSorted
	    return YSorted;
#else
	    goto not_there;
#endif
	if (strEQ(name, "YXBanded"))
#ifdef YXBanded
	    return YXBanded;
#else
	    goto not_there;
#endif
	if (strEQ(name, "YXSorted"))
#ifdef YXSorted
	    return YXSorted;
#else
	    goto not_there;
#endif
	break;
    case 'Z':
	if (strEQ(name, "ZPixmap"))
#ifdef ZPixmap
	    return ZPixmap;
#else
	    goto not_there;
#endif
	break;
    }
    errno = EINVAL;
    return 0;

not_there:
    errno = ENOENT;
    return 0;
}


MODULE = Tk::X		PACKAGE = Tk::X

PROTOTYPES: ENABLE

double
constant(name,arg)
	char *		name
	int		arg

