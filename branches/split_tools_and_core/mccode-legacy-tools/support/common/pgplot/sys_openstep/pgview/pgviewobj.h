#import <AppKit/AppKit.h>
#import <objc/Storage.h>

@interface pgviewobj : NSView
{
      Storage     *psdata;
      NSWindow    *myWindow;
      float       prevw, prevh;
      int         lwtype;
      int         nplot;
}

- initWithFrame:(NSRect)frameRect;
- (void) drawRect:(NSRect)rects;

- (void) pgplotDefs: (DPSContext) ctxt;

- (void) beginp;
- (void) endp;
- (void) flushpg;
- (void) gettype: (int *) iptype;
- (void) getwind: (int *) ixdim  by: (int *) iydim
         color: (int *) icol  scale: (int *) imag;
- (void) pscode: (char *) cbuf;
- (void) readcursor: (NSPoint *) aPoint char: (int *) ichar
         cursor: (NSCursor *) crossCursor;
@end
