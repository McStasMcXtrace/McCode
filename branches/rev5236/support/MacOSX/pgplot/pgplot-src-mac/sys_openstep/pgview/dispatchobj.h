#import <AppKit/AppKit.h>
#import "pgviewobj.h"

@interface dispatchobj : NSObject
{
      NSCursor *crossCursor;
      pgviewobj *curView;
      BOOL      qdrawing;
      int       iwtype;
}

- (void)newLand;
- (void)newPort;
- (void)pgprint;
- (void)deactive;

- (void)beginp;
- (void)cursorat: (float *) xpos and: (float *) ypos  char: (int *) ichar;
- (void)flushpg;
- (void)getwind: (int *) ixdim  by: (int *) iydim
         color: (int *) icol  scale: (int *) imag;
- (void)pscode: (char *) cbuf;
- (void)endp;

@end
