#import <AppKit/AppKit.h>
#import <objc/Storage.h>

@interface hostobj : Object
{
      Storage     *hostlist;
      NSMenu      *hostmenu;
      id          mydispatch;
}

- initwithdispatch:(id)adispatch;
- (void) addhost:(int)iaddr;
- (BOOL) queryhost:(int)isuspect;
- (NSMenu *) gethostmenu;
- (void) removehost:(NSMenuItem *) sender;

@end
