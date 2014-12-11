// Dispatch is the central clearinghouse for the pgview program.  Dispatch
// creates a Listener object to listen to messages from PGPLOT programs.
// Most of these messages are routed to the active PGView object.
// Dispatch also allows the user to create new windows and to print
// windows using the main menu.  Since changing windows in the middle of
// a PostScript stream may cause problems, Dispatch does now allow the
// active window to change if PGPLOT is in the midst of a plot, i.e.,
// between the Begin Picture and End Picture commands.  Since most PGPLOT
// programs only send an End Picture immdediately prior to the next Begin
// Picture command, this means that you will not be able to change windows
// while most PGPLOT programs are running.
//
// 1992-Mar-9 - [AFT]
//---
#import "Dispatch.h"
#import <appkit/Application.h>
#import <appkit/PrintInfo.h>
#import "PGView.h"
#import "pgvListener.h"
#import "sername.h"

@implementation Dispatch

//
//--- Class methods -----------------------------------------------------
//

- init
{
      NXPoint  spot;

      [super init];

// Listen for PGPLOT programs
      myListener = [[pgvListener alloc] init];
      [myListener checkInAs: PGV_SERVER_NAME];
      [myListener addPort];
      [myListener setDelegate:self];

// Scale print jobs to print on one page.  Note setxxxPagination does not
// return self.
      [[NXApp printInfo] setHorizPagination:NX_FITPAGINATION];
      [[NXApp printInfo]  setVertPagination:NX_FITPAGINATION];

// Prepare the cross cursor
      crossCursor=[[NXCursor alloc] 
         initFromImage:[NXImage newFromSection:"cross.tiff"]];
      spot.x = spot.y = 7.0;
      [crossCursor setHotSpot:&spot];

// 0=Landscape, 1=portrait
      iwtype=0;
      curView=NULL;
      qdrawing=NO;

      return self;
}

//
//--- Window delegate ---------------------------------------------------
//

- windowDidBecomeMain:sender
// If PGPLOT is actively drawing, then we try to prevent the key window
// from changing.  This is done so that the main window (i.e., the one
// with the back title bar) will denote the currently active plot window.
{
      if( qdrawing ) {
         if( [sender contentView] != curView) {
            [[curView window] makeKeyWindow];
         }
      } else {
         curView=[sender contentView];
         [curView gettype: &iwtype];
         if(iwtype==0) {
            [[NXApp printInfo] setOrientation:NX_LANDSCAPE andAdjust:YES];
         } else {
            [[NXApp printInfo] setOrientation:NX_PORTRAIT  andAdjust:YES];
         }
      }
      return self;
}

- windowWillClose:sender
// Prevent PGPLOT from trying to draw to a window that was closed.
{
      if( curView == [sender contentView] ) curView=NULL;
      return self;
}

//
//--- Targets for menu items --------------------------------------------
//

- showInfo:sender
{
      if(!infoPanel) {
         [NXApp loadNibSection:"info.nib" owner:self];
      }
      [infoPanel makeKeyAndOrderFront:self];
      return self;
}

- newLand:sender
{
static NXRect wRect = {{330.0, 230.0},{720.0,535.0}};
      PGView   *newView;

      newView = [[PGView alloc] initFrame:&wRect];
      [[newView window] setDelegate:self];     
      if( !qdrawing ) {
         curView=newView;
         iwtype=0;
         [[NXApp printInfo] setOrientation:NX_LANDSCAPE andAdjust:YES];
      }
      return self;
}

- newPort:sender
{
static NXRect wRect = {{500.0, 70.0},{535.0,720.0}};
      PGView   *newView;

      newView = [[PGView alloc] initFrame:&wRect];
      [[newView window] setDelegate:self];
      if( !qdrawing ) {
         curView=newView;
         iwtype=1;
         [[NXApp printInfo] setOrientation:NX_PORTRAIT andAdjust:YES];
      }
      return self;
}

- print:sender
{
      [curView printPSCode:self];
      return self;
}

//
//--- Listener methods --------------------------------------------------
//

- beginp
{
      qdrawing=YES;
      [curView beginp];
      if([NXApp isHidden]) {
         [NXApp unhideWithoutActivation:self];
      }
      if(![[curView window] isVisible]) {
         [[curView window] orderFront:self];
      }
      return 0;
}

- cursorat: (double *) xpos and: (double *) ypos  char: (int *) ichar
{
      NXPoint aPoint;

      aPoint.x=(float) *xpos;
      aPoint.y=(float) *ypos;
      [curView readcursor: &aPoint char: ichar cursor:crossCursor];
      *xpos=(double) aPoint.x;
      *ypos=(double) aPoint.y;
      return 0;
}

- flush
{
      [curView flush];
      return 0;
}

- getwind: (int *) ixdim  by: (int *) iydim
         scale: (double *) dmag  color: (int *) icol
{
      if(curView == NULL) {
         if(iwtype==0) {
            [self newLand:self];
         } else {
            [self newPort:self];
         }
      }
      [curView getwind:ixdim  by:iydim  color:icol  scale:dmag];
      return 0;
}

- pscode: (char *) cbuf
{
      [curView pscode:cbuf];
      return 0;
}

- endp
{
      qdrawing=NO;
      return 0;
}

@end
