// dispatch.m--The purpose is to keep track of the currently active plot
// view and to route messages from the PGPLOT program there.  To do this,
// dispatchobj needs to be the target for the Menu items that create
// windows.  It also needs to be the window delegate. And finally it
// needs to receive messages from the Port object (i.e., from the
// socket interface to the PGPLOT program).
//    If PGPLOT is drawing to a window then dispatchobj doe not allow
// the user to delete the window.
//
// 1999-Feb-20 - Update for OpenStep - [AFT]
// 1992-Mar-9 - [AFT]
//---
#import "dispatchobj.h"
#import "ipanelobj.h"

@implementation dispatchobj

//
//--- Class methods -----------------------------------------------------
//

- init
{
      NSPoint  spot;

      [super init];

// Scale print jobs to print on one page.
      [[NSPrintInfo sharedPrintInfo] setHorizontalPagination:NSFitPagination];
      [[NSPrintInfo sharedPrintInfo] setVerticalPagination:NSFitPagination];

// Prepare the cross cursor
      spot.x = 7.0; spot.y = 7.0;
      crossCursor = [[NSCursor alloc]
         initWithImage:[NSImage imageNamed:@"cross.tiff"]
         hotSpot:spot];

// 0=Landscape, 1=portrait
      iwtype=0;
      curView=NULL;
      qdrawing=NO;

      return self;
}

//
//--- Window delegate ---------------------------------------------------
//

- (void)windowDidBecomeMain:(NSNotification *)notification
// If PGPLOT is actively drawing, then we try to prevent the key window
// from changing.  This is done so that the key window (i.e., the one
// with the back title bar) will denote the currently active plot window.
{
      NSWindow *theWindow = [notification object];
      if( qdrawing ) {
// If we are drawing, then force the curView view to be the key window.
         if( [theWindow contentView] != curView) {
             [[curView window] makeKeyWindow];
         }
      } else {
         curView=[theWindow contentView];
         [curView gettype: &iwtype];
         if(iwtype==0) {
            [[NSPrintInfo sharedPrintInfo] setOrientation:NSLandscapeOrientation];
         } else {
            [[NSPrintInfo sharedPrintInfo] setOrientation:NSPortraitOrientation];
         }
      }
      return;
}

- (BOOL)windowShouldClose:(id)sender
// Prevent window manager from closing a window in which PGPLOT is
// still drawing.
{
      if( !qdrawing || curView != [sender contentView] ) {
         if ( !qdrawing ) curView=NULL;
         return YES;
      }
      return NO;
}


//
//--- Targets for menu items --------------------------------------------
//

- (void)newLand
{
static NSRect wRect = {{330.0, 230.0},{720.0,535.0}};
      pgviewobj *newView;

      newView = [[pgviewobj alloc] initWithFrame:wRect];
      [[newView window] setDelegate:self];
      if( !qdrawing ) {
         curView=newView;
         iwtype=0;
         [[NSPrintInfo sharedPrintInfo] setOrientation:NSLandscapeOrientation];
      }
      return;
}

- (void)newPort
{
static NSRect wRect = {{500.0, 70.0},{535.0,720.0}};
      pgviewobj *newView;

      newView = [[pgviewobj alloc] initWithFrame:wRect];
      [[newView window] setDelegate:self];
      if( !qdrawing ) {
         curView=newView;
         iwtype=1;
         [[NSPrintInfo sharedPrintInfo] setOrientation:NSPortraitOrientation];
      }
      return;
}

- (void)pgprint
{
      [curView print:self];
      return;
}

- (void)showInfo
{
      [[[ipanelobj alloc] init] showit];
      return;
}

- (void)deactive
{
      [NSApp hide:self];
      [NSApp unhideWithoutActivation];
      return;
}

//
//--- methods called by the Port object ---------------------------------
//

- (void)beginp
{
      qdrawing=YES;
      [curView beginp];
      return;
}

- (void)cursorat: (float *) xpos and: (float *) ypos  char: (int *) ichar
{
      NSPoint aPoint;

      aPoint.x= *xpos;
      aPoint.y= *ypos;
      [curView readcursor: &aPoint char: ichar cursor:crossCursor];
      *xpos= aPoint.x;
      *ypos= aPoint.y;
      return;
}

- (void)flushpg
{
      [curView flushpg];
      return;
}

- (void)getwind: (int *) ixdim  by: (int *) iydim
         color: (int *) icol scale: (int *) imag
{
      if(curView == NULL) {
         if(iwtype==0) {
            [self newLand];
         } else {
            [self newPort];
         }
      }
      [curView getwind:ixdim  by:iydim  color:icol  scale:imag];
      return;
}

- (void)pscode: (char *) cbuf
{
      [curView pscode:cbuf];
      return;
}

- (void)endp
{
      qdrawing=NO;
      [curView endp];
      return;
}

@end
