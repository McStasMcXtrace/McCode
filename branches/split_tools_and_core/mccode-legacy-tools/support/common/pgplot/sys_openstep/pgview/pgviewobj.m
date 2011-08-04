// pgviewobj.m is the custom view object for the pgview program.  PGView
// receives 132 byte character arrays filled with PostScript code.  PGView
// saves these buffers in a Storage object.  When the PGPLOT program
// executes a flushpg operation, this stored data is then sent to the
// display.
//
// 1999-Feb-20 - Update for OpenStep - [AFT]
// 1992-Mar-09 - [AFT]
//---
#import "pgviewobj.h"

@implementation pgviewobj

- initWithFrame:(NSRect)frameRect
{
      [super initWithFrame:frameRect];

// Create an enclosing window and bring it upfront
      myWindow = [[NSWindow alloc] initWithContentRect:frameRect
         styleMask:NSResizableWindowMask|
               (NSMiniaturizableWindowMask | NSClosableWindowMask)
         backing:NSBackingStoreBuffered
         defer:NO];
      [myWindow setContentView:self];
      [myWindow setBackgroundColor:[NSColor whiteColor]];
      [myWindow setReleasedWhenClosed:YES];
      [myWindow setTitle:@"PGPLOT Viewer"];
      [myWindow display];
      [myWindow orderFront:self];

// Save portrait/landscape flag
      if( frameRect.size.width > frameRect.size.height) {
         lwtype=0;
      } else {
         lwtype=1;
      }

// Needed to do PostScript scaling
      [self scaleUnitSquareToSize:NSMakeSize(0.1, 0.1)];
      prevw = [self bounds].size.width;
      prevh = [self bounds].size.height;

// PostScript data sent from PGPLOT will be stored in psdata.
      psdata = [Storage newCount:0
                elementSize:132
                description:"[132c]"];
      nplot=0;

// Allocate private graphics state, so things like the current color
// setting will be preserved between different calls to drawSelf.
      [self allocateGState];

      return self;
}

- (void)drawRect:(NSRect)rect
{
      DPSContext ctxt;
      NSRect bounds;
      int    i;

      if ( nplot > 0) {
         bounds=[self bounds];
         if( prevw != bounds.size.width || prevh != bounds.size.height) {
            [self scaleUnitSquareToSize:NSMakeSize(bounds.size.width/prevw,
                                                   bounds.size.height/prevh)];
            prevw = bounds.size.width;
            prevh = bounds.size.height;
         }

         ctxt= DPSGetCurrentContext();
         for(i=0; i<nplot; i++) {
            DPSPrintf(ctxt, "%s\n", (char *) [psdata elementAt:i]);
         }
      }
      return;
}

- (void)dealloc
{
printf("in dealloc\n");
      [psdata free];
      [super dealloc];
      return;
}

- (void)endPrologue
// This routine sends the PGPLOT defines to the print job.
{
      DPSContext ctxt;

      ctxt= DPSGetCurrentContext();
      [self pgplotDefs:ctxt];
      [super endPrologue];
      return;
}

- (void) pgplotDefs: (DPSContext) ctxt
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
      return;
}

- (void) beginp
{
      DPSContext ctxt;

      if([NSApp isHidden]) {
         [NSApp unhideWithoutActivation];
      }
      [myWindow setDocumentEdited:YES];
      [self lockFocus];
      ctxt= DPSGetCurrentContext();
      DPSPrintf(ctxt,"cleardictstack\n");
      [self pgplotDefs:ctxt];
      [self unlockFocus];
      [myWindow orderFrontRegardless];
      [psdata empty];
      nplot=0;

      return;
}

- (void) endp
{
      [myWindow setDocumentEdited:NO];
      return;
}

- (void) flushpg
{
      DPSContext ctxt;
      int   i, ibeg;

      if ([psdata count] > nplot) {
         [self lockFocus];
         ibeg=nplot;
         nplot = [psdata count];
         ctxt= DPSGetCurrentContext();
         for(i=ibeg; i<nplot; i++) {
            DPSPrintf(ctxt, "%s\n", (char *) [psdata elementAt:i]);
         }
         PScurrentgstate([self gState]);
         PSpop();
         [myWindow flushWindow];
         [self unlockFocus];
      }
      return;
}

- (void) gettype: (int *) iwtype;
{
      *iwtype=lwtype;
      return;
}

- (void) getwind: (int *) ixdim  by: (int *) iydim
      color: (int *) icol  scale: (int *) imag
{
      *ixdim= (int) ([self bounds].size.width+0.5);
      *iydim= (int) ([self bounds].size.height+0.5);
      *icol = [self shouldDrawColor];
      *imag=10;
      return;
}

- (void) pscode: (char *) cbuf
{
      [psdata addElement:(void *) cbuf];
      return;
}

- (void) readcursor: (NSPoint *) aPoint char: (int *) ichar
         cursor: (NSCursor *) crossCursor
{
      char     cbuf[8];
      NSEvent  *nextEvent;
      NSModalSession session;
      NSPoint  curPoint;
      NSRect   hitRect;
      NSView   *mysuper;
      BOOL     qinside, qloop;

// Since pgview is not the active application, it cannot read the
// the keyboard.  The following code forces pgview to become the
// active application.
      [NSApp activateIgnoringOtherApps:YES];

// Calculate the coordinates of the view in the window coordinate system.
      mysuper = [self superview];
      hitRect = [self convertRect:[self frame] fromView:mysuper];

      [myWindow makeKeyAndOrderFront:self];
      curPoint = [myWindow mouseLocationOutsideOfEventStream];
      qinside=[self mouse:curPoint inRect:hitRect];
      [self addTrackingRect:hitRect
         owner:self
         userData:nil
         assumeInside:qinside];

      [self lockFocus];
      if(qinside) [crossCursor set];

// If I don't use a Modal loop, then sometimes a busy cursor occurs.
// The modal loop is the only way I know how to prevent this, sorry.
      session = [NSApp beginModalSessionForWindow:myWindow];
      [NSApp runModalSession:session];

      qloop=YES;
      do {
         nextEvent=[myWindow nextEventMatchingMask:
            NSLeftMouseDownMask| NSLeftMouseUpMask |
            NSKeyDownMask      | NSKeyUpMask   |
            NSMouseEnteredMask | NSMouseExitedMask];

         switch ([nextEvent type]) {
         case NSLeftMouseDown:
         case NSKeyDown:
// Ignore down events.
            break;
         case NSLeftMouseUp:
         case NSKeyUp:
// Only process events if mouse is inside the current view.
            if(qinside) {
               if([nextEvent type]==NSKeyUp) {
                  [[nextEvent characters] getCString:cbuf maxLength:8];
                  *ichar= (int) cbuf[0];
                  curPoint = [myWindow mouseLocationOutsideOfEventStream];
               } else {
                  *ichar= 65;
                  curPoint=[nextEvent locationInWindow];
               }
// First convert from pixel coordinate in the window system to view
               curPoint = [mysuper convertPoint:curPoint fromView:[mysuper superview]];
// Now convert from pixel to scaled coordinate
               curPoint = [self convertPoint:curPoint fromView:mysuper];
               *aPoint = curPoint;
               qloop=NO;
            }
            break;
         case NSMouseEntered:
            qinside = YES;
            [crossCursor set];
            break;
         case NSMouseExited:
            qinside = NO;
            [[NSCursor arrowCursor] set];
            break;
         default:
printf("PGView--mystery event type=%d  flags=%d  window=%u\n",
         [nextEvent type], [nextEvent modifierFlags], [nextEvent windowNumber]);
            break;
         }

      } while (qloop);

      [NSApp endModalSession:session];
      [[NSCursor arrowCursor] set];
      [self unlockFocus];

// I have not found a nice way to return control the last active application.
// This kludge does the job, but flashes the window on and off the screen.
//      [NSApp hide:self];
//      [NSApp unhideWithoutActivation];
      [NSApp deactivate];
      return;
}

@end
