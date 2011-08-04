/* Based on TCPPort.h from /NextDeveloper/Examples */

#import <Foundation/Foundation.h>
#import "dispatchobj.h"
#import "hostobj.h"

@interface portobj : NSPort
{
      id       portFile;      // the connect()ed NSFileHandle
      dispatchobj *mydispatch;
      hostobj  *hostlist;
      int      mypeer;
      int      istate;
      int      ifunc;
      int      ilen;
      int      nread;
      char     cbuf[256];
}

- (portobj *)initport:(unsigned short)ipn target:(dispatchobj *)adispatch;
- (NSMenu *) gethostmenu;

@end
