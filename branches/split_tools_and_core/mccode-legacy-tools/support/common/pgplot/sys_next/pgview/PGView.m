// PGView is the custom view object for the pgview program.  PGView
// receives 132 byte character arrays filled with PostScript code.  PGView
// saves these buffers in a Storage object.  When the PGPLOT program
// executes a flush operation, this stored data is then sent to the
// display.
//
// 1992-Mar-9 - [AFT]
//---
#import "PGView.h"
#import <appkit/Application.h>
#import <appkit/NXCursor.h>
#import <appkit/Window.h>
#import <objc/Storage.h>
#import <dpsclient/wraps.h>    // for PSxxxx routines

@implementation PGView

- initFrame:(const NXRect *) frameRect
{
      id       myWindow;

      [super initFrame:frameRect];

// Create an enclosing window and bring it upfront
    
      myWindow = [[Window alloc] initContent:frameRect
            style:NX_RESIZEBARSTYLE
            backing:NX_BUFFERED
            buttonMask:(NX_MINIATURIZEBUTTONMASK | NX_CLOSEBUTTONMASK)
            defer:NO];
      [myWindow setContentView:self];
      [myWindow setBackgroundGray:NX_WHITE];
      [myWindow setFreeWhenClosed:YES];
      [[[window setTitle:"PGPLOT Viewer"] display] orderFront:self];

// Save portrait/landscape flag
      if( frameRect->size.width > frameRect->size.height) {
         lwtype=0;
      } else {
         lwtype=1;
      }

// Needed to do PostScript scaling
      [self scale:0.1 :0.1];
      prevw = bounds.size.width;
      prevh = bounds.size.height;

// Buffer is used to store PostScript data
      psdata = [Storage newCount:1
                elementSize:132
                description:"[132c]"];
      iflush=0;
      nbuf=0;
      nplot=0;

// Allocate private graphics state, so things like the current color
// setting will be preserved between different calls to drawSelf.
      [self notifyToInitGState:YES];
      [self allocateGState];

      return self;
}

- drawSelf:(const NXRect *) rects :(int)rectCount
{
      char  *ctmp;
      int   i, ibeg;

      ctxt= DPSGetCurrentContext();
      if ([psdata count] > 0) {

         if( prevw != bounds.size.width || prevh != bounds.size.height) {
            [self scale:bounds.size.width/prevw
                       :bounds.size.height/prevh];
            prevw = bounds.size.width;
            prevh = bounds.size.height;
         }

// If PGPLOT sent a 'flush' message, then only send new data to sceen.
         if( iflush>0 ) {
            ibeg=nplot;
         } else {
            ibeg=0;
         }

         nplot = nbuf;
         for(i=ibeg; i<nplot; i++) {
            ctmp = (char *) [psdata elementAt:i];
//printf("drawSelf--%s\n", ctmp);
            DPSPrintf(ctxt, "%s\n", ctmp);
         }
// Preserve current graphic state for next call.
         if( iflush>0 ) {
            PScurrentgstate([self gState]);
            PSpop();
         }
      }
      return self;
}

- free
{
      [psdata free];
      return [super free];
}

- endPrologue
// This routine sends the PGPLOT defines to the print job.
{
      ctxt= DPSGetCurrentContext();
      [self pgplotDefs];
      return [super endPrologue];
}

- pgplotDefs
{
      DPSPrintf(ctxt,
      "/l {moveto rlineto currentpoint stroke moveto} bind def\n");
      DPSPrintf(ctxt,
      "/c {rlineto currentpoint stroke moveto} bind def\n");
      DPSPrintf(ctxt,
      "/d {moveto 0 0 rlineto currentpoint stroke moveto} bind def\n");
      DPSPrintf(ctxt,"/SLW {5 mul setlinewidth} bind def\n");
      DPSPrintf(ctxt,"/BP {newpath moveto} bind def\n");
      DPSPrintf(ctxt,"/LP /rlineto load def\n");
      DPSPrintf(ctxt,"/EP {rlineto closepath eofill} bind def\n");
      return self;
}

- beginp
{
      [self lockFocus];
      ctxt= DPSGetCurrentContext();
      DPSPrintf(ctxt,"cleardictstack\n");
      [self pgplotDefs];
      [self unlockFocus];

      [psdata empty];
      nbuf=0;
      nplot=0;

      return self;
}

- flush
{
      if ([psdata count] > nplot) {
         iflush=1;
         nbuf=[psdata count];
         [self display];
         iflush=0;
      }
      return self;
}

- gettype: (int *) iwtype;
{
      *iwtype=lwtype;
      return self;
}

- getwind: (int *) ixdim  by: (int *) iydim
      color: (int *) icol  scale: (double *) dmag
{
      *ixdim= (int) (bounds.size.width+0.5);
      *iydim= (int) (bounds.size.height+0.5);
      *icol = [self shouldDrawColor];
      *dmag=10.0;
      return self;
}

- pscode: (char *) cbuf
{
      [psdata addElement:(void *) cbuf];
      return self;
}

- readcursor: (NXPoint *) aPoint char: (int *) ichar
      cursor: (NXCursor *) crossCursor
{
      int      lasact, iwindnum;
      NXEvent  *nextEvent;
      NXModalSession session;
      NXPoint  curPoint;
      NXRect   hitRect;
      BOOL     qinside, qloop;

// Since pgview is not the active application, it cannot read the
// the keyboard.  The following code forces pgview to become the
// active application.
      lasact= [NXApp activateSelf:YES];

// Calculate the coordinates of the view in the window coordinate system.
      [self getFrame:&hitRect];
      [superview convertRect:&hitRect toView:nil];

      [window makeKeyAndOrderFront:self];
      [window getMouseLocation:&curPoint];
      qinside=[self mouse:&curPoint  inRect:&hitRect];
      [window setTrackingRect:&hitRect  inside:qinside  owner:self
         tag:1  left:NO  right:NO];
      iwindnum=[window windowNum];

      [self lockFocus];

// If I don't use a Modal loop, then sometimes a busy cursor occurs.
// The modal loop is the only way I know how to prevent this, sorry.
      [NXApp beginModalSession: &session for:window];
      [NXApp runModalSession: &session];

      PSshowcursor();
      if(qinside) {
         [crossCursor set];
      }
// Following should work, but doesn't
//      [self addCursorRect:&hitRect cursor:crossCursor];

      qloop=YES;
      do {
         nextEvent=[NXApp getNextEvent:
            NX_MOUSEDOWNMASK    | NX_MOUSEUPMASK |
            NX_KEYDOWNMASK      | NX_KEYUPMASK   |
            NX_MOUSEENTEREDMASK | NX_MOUSEEXITEDMASK ];

         switch (nextEvent->type) {
         case NX_MOUSEDOWN:
         case NX_KEYDOWN:
// Ignore down events.
            break;
         case NX_MOUSEUP:
         case NX_KEYUP:
// Only process events if mouse is inside the current view.
            if(qinside) {
               if(nextEvent->type==NX_KEYUP) {
                  *ichar= (int) nextEvent->data.key.charCode;
                  [window getMouseLocation:&curPoint];
               } else {
                  *ichar= 65;
                  curPoint=nextEvent->location;
               }
// First convert from pixel coordinate in the window system to view
               [superview convertPointFromSuperview:&curPoint];
// Now convert from pixel to scaled coordinate
               [self convertPointFromSuperview:&curPoint];
               *aPoint = curPoint;
               qloop=NO;
            }
            break;
         case NX_MOUSEENTERED:
            qinside = YES;
            [crossCursor set];
            break;
         case NX_MOUSEEXITED:
            qinside = NO;
            [NXArrow set];
            break;
         default:
printf("PGView--mystery event type=%d  flags=%d  window=%u\n",
         nextEvent->type, nextEvent->flags, nextEvent->window);
            break;
         }

      } while (qloop);

      [NXArrow set];
      [NXApp endModalSession: &session];
      [self unlockFocus];

// Sending an active:lstact message to NXApp does not always work.  The
// problem occurs if pgview was launched via an 'open -a' statement and
// for the second (and following) cursor reads in pgex15.  The following
// is less 'user friendly' but safer.
      [NXApp deactivateSelf];
      return self;
}

@end
