// ipanel creates and displays the pgview info panel.
//
// 1999-Feb-20 - [AFT]
//---
#import "ipanelobj.h"

@implementation ipanelobj

- showit
{
      static NSRect aRect = {{160.0, 500.0},{330.0,120.0}};
      NSWindow *aPanel;

      aPanel = [[NSWindow alloc] initWithContentRect:aRect
         styleMask:NSTitledWindowMask|NSClosableWindowMask
         backing:NSBackingStoreBuffered
         defer:NO];
      [aPanel setContentView:self];
      [aPanel setBackgroundColor:[NSColor whiteColor]];
      [aPanel setReleasedWhenClosed:YES];
      [aPanel setTitle:@"Info"];
      [aPanel display];
      [aPanel orderFront:self];
      return self;
}

- (void)drawRect:(NSRect)rects
{
      NSImage *myicon;
      static NSPoint iloc = {20.0, 45.0};

      PSmoveto(100.0, 80.0);
      PSselectfont("Times-Roman",24.0);
      PSshow("PGPLOT Viewer");
      PSmoveto(120.0,  50.0);
      PSselectfont("Times-Roman",16.0);
      PSshow("by Allyn Tennant");
      PSsetgray(0.25);
      PSmoveto(20.0,  9.0);
      PSselectfont("Times-Roman",10.0);
      PSshow("1999-Feb-20   X-ray Astronomy Group, Marshall Space Flight Center");
      PSmoveto(0.0, 25.0);
      PSlineto(350.0, 25.0);
      PSstroke();
      myicon=[NSApp applicationIconImage];
      [myicon compositeToPoint:iloc operation:NSCompositeSourceOver];
      return;
}

@end
