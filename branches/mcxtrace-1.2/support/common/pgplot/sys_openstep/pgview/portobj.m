// portobj.m--This creates a NSPort object that can be used in a
// NSConnection.  Since we are using Unix/Internet stream sockets
// and not Mach sockets, most methods need to be overwritten.
// based on /NextDeveloper/Examples/Foundation/TCPTransport/TCPPort.m
// 1999-Feb-22 - [AFT]

#import "portobj.h"

#import <sys/types.h>
#import <sys/socket.h>
#import <netinet/in.h>
#import <netdb.h>

#define INVALID_SOCKET -1

#if !defined(__svr4__) && !defined(WIN32)
#import <libc.h>
#endif

@implementation portobj

//
//--- Useful function
//

void grreply(int peeraddr, short iport, void *reply, int lreply)
{
      struct sockaddr_in repadd;
      int repsock;

// Create reply socket descriptor
      repsock = socket(AF_INET, SOCK_STREAM, 0);
// Create the reply address structure
      memset((char *)&repadd, 0, sizeof(struct sockaddr_in));
      repadd.sin_family = AF_INET;
      repadd.sin_port=iport;
      repadd.sin_addr.s_addr = peeraddr;

      if (connect(repsock, (struct sockaddr *)&repadd, sizeof(repadd)) < 0) {
         perror("connecting reply socket");
      } else {
         if (write(repsock, reply, lreply) < 0)
            perror("writing on stream socket");
      }
      close(repsock);
      return;
}

//
//--- Class methods -----------------------------------------------------
//

- (portobj *)initport:(unsigned short)ipn target:(dispatchobj *)adispatch
{
      NSHost *myhost;
      struct sockaddr_in addr;
      struct hostent *ahost;
      char *ctmp;
      int  fd;
      int  itmp, ltmp;

// Create socket descriptor
      fd = socket(PF_INET, SOCK_STREAM, 0);
      if (fd == INVALID_SOCKET) {
         NSLog(@"socket call failed");
         return nil;
      }

// Create the address structure
      ltmp = sizeof(struct sockaddr_in);
      memset((char *)&addr, 0, ltmp);
      addr.sin_family = AF_INET;
      addr.sin_addr.s_addr = INADDR_ANY;
      addr.sin_port = htons(ipn);

// Bind address structure to socket descriptor
      if (bind(fd, (struct sockaddr *)&addr, ltmp) < 0) {
         NSLog(@"bind failed");
         close(fd);
         return nil;
      }

// Passively listen for connections.
      listen(fd, 5);

// Create an NSFileHandle object to do the waiting
      portFile = [[NSFileHandle alloc] initWithFileDescriptor:fd];
      mydispatch = adispatch;
// This tells portFile to alert us for connection attempts.
      [[NSNotificationCenter defaultCenter]
         addObserver:self
         selector:@selector(acceptNotification:)
         name:NSFileHandleConnectionAcceptedNotification
         object:portFile];
// Create the hostobj to manage the list of hosts that are allowed
// to connect to us.
      hostlist=[hostobj new];
      [hostlist initwithdispatch:self];

// Add "localhost" to list.
      ctmp="localhost";
      ahost = gethostbyname(ctmp);
      if (ahost == 0) {
         fprintf(stderr, "%s: unknown host", ctmp);
      } else {
         memcpy(&itmp, ahost->h_addr, 4);
         [hostlist addhost:itmp];
      }

// Get the actual IP address of local machine and add.
      myhost = [NSHost currentHost];
      if ( myhost != nil ) {
         itmp = inet_addr([[myhost address] cString]);
         [hostlist addhost:itmp];
      }

      return self;
}

- (NSMenu *) gethostmenu
{
      return [hostlist gethostmenu];
}

- (void) removehost:(id) sender
{
      [hostlist removehost:sender];
      return;
}

//
//--- methods needed by runloop -----------------------------------------
//

- (void)addConnection:(NSConnection *)conn
      toRunLoop:(NSRunLoop *)runLoop
      forMode:(NSString *)mode
{
      portobj *listener = [conn receivePort];
      portobj *sender = [conn sendPort];

      [listener->portFile acceptConnectionInBackgroundAndNotify];

      if (listener != sender) {
         [sender->portFile readInBackgroundAndNotify];
      }
      return;
}

- (void)removeConnection:(NSConnection *)conn
      fromRunLoop:(NSRunLoop *)runLoop
      forMode:(NSString *)mode {
// Since addConnection allocates no objects, we have none to release.
}

- (void)acceptNotification:(NSNotification *)note {
      NSFileHandle *msgFile;
      NSConnection *connection;
      portobj  *sp;
      struct   sockaddr_in peeradd;
      int      msgsock, itmp;

// Get the file handle info for the incoming messages
      msgFile = [[note userInfo]
         objectForKey:NSFileHandleNotificationFileHandleItem];
// We now have all data from the Notification.  Allow another connection.
      [portFile acceptConnectionInBackgroundAndNotify];
      if (!msgFile) {
         NSLog(@"** no socket in notification info %@", [note userInfo]);
         return;
      }
// Find out who is trying to connect with us.
      msgsock=[msgFile fileDescriptor];
      itmp = sizeof(peeradd);
      if (getpeername(msgsock, (struct sockaddr *)&peeradd, &itmp)) {
         perror("getting socket name");
      }
      if ( ![hostlist queryhost:peeradd.sin_addr.s_addr] ) {
// User rejected the connection, close the file handle.
         [msgFile closeFile];
         return;
      }

// Create a port to receive the messages.  This will be a "sendPort".
      sp = [isa alloc];
      if (!sp) {
         return;
      }
// This tells the msgFile handle to stick around and saves the pointer
// in our local portFile variable.
      sp->portFile = [msgFile retain];
      sp->mydispatch = mydispatch;
      sp->mypeer=peeradd.sin_addr.s_addr;
      sp->istate = 0;
      [sp autorelease];
      [[NSNotificationCenter defaultCenter]
         addObserver:sp
         selector:@selector(readNotification:)
         name:NSFileHandleReadCompletionNotification
         object:msgFile];
      connection = [[NSConnection alloc] initWithReceivePort:self sendPort:sp];

      return;
}

- (void) sendoff {
// We have now read the function code, length, and data.  Dispatch.
      short iport;
      float r1, r2;
      int   ibuf[4], itmp[4];
      int   i;

      switch (ifunc) {
      case 1:
         memcpy(&iport, cbuf, 2);
         [mydispatch  getwind:&itmp[0]  by:&itmp[1]
               color:&itmp[3]  scale:&itmp[2]];
         for (i=0; i<4; i++) {
            ibuf[i]=htonl(itmp[i]);
         }
         grreply(mypeer, iport, &ibuf, 16);
         break;
      case 2:
         [mydispatch beginp];
         break;
      case 3:
         [mydispatch pscode:cbuf];
         break;
      case 4:
         memcpy(&iport, cbuf, 2);
         [mydispatch  cursorat:&r1  and:&r2  char:&itmp[2]];
         ibuf[0] = htonl( (int) r1 );
         ibuf[1] = htonl( (int) r2 );
         ibuf[2] = htonl( itmp[2] );
         grreply(mypeer, iport, &ibuf, 12);
         break;
      case 5:
         [mydispatch flushpg];
         break;
      case 6:
         [mydispatch endp];
         break;
      default:
         printf("portobj--Unknown function code= %d\n",ifunc);
         break;
      } /* end switch (ifunc) */
}

- (void)readNotification:(NSNotification *)note {
      NSData   *data;
      NSRange  substring;
      int      lentot, ioff;
      unsigned char c1buf;

      data = [[note userInfo] objectForKey:NSFileHandleNotificationDataItem];
      lentot=[data length];
      if ( lentot<0 ) {
         perror("reading stream message");
         return;
      } else if (lentot == 0) {
// Nothing more to read.  Close file handle and exit.
         [portFile closeFile];
         return;
      }
// Set the next read going.
      [portFile readInBackgroundAndNotify];
      ioff=0;
      do {
         switch (istate) {
         case 0:
// Read function code
            nread = 0;
            substring.location=ioff;
            substring.length=1;
            [data getBytes:&c1buf range:substring];
            ioff=ioff+substring.length;
            ifunc=c1buf;
            istate = 1;
            break;
         case 1:
// Read length
            substring.location=ioff;
            substring.length=1;
            [data getBytes:&c1buf range:substring];
            ioff=ioff+substring.length;
            ilen=c1buf;
            if ( ilen==0 ) {
// No data package to read, dispatch now.
               istate = 0;
//printf("ifunc=%i, ilen=%i\n",ifunc,ilen);
               [self sendoff];
            } else {
// Next read the data.
               istate = 2;
            }
            break;
         case 2:
// Read data.  There are two input cases, either cbuf is empty or
// it contains part of a previous read.  Both cases are handled
// by starting cbuf at position nread.  Likewise there are two output
// cases, either we finish the read or we don't.
            substring.location=ioff;
            if (  ilen-nread <= lentot-ioff ) {
// Can finish the read.  State will change.
               substring.length = ilen-nread;
               istate = 0;
            } else {
// Incomplete read, so no state change.
               substring.length = lentot-ioff;
            }
            [data getBytes:&cbuf[nread] range:substring];
            ioff=ioff+substring.length;
            nread = nread+substring.length;
            if ( istate==0 ) {
//printf("ifunc=%i, ilen=%i\n",ifunc,ilen);
               [self sendoff];
            }
         } /* end switch (state) */
      } while ( ioff<lentot );
      return;

}

//- (NSString *)description {
//      return [NSString stringWithFormat:@"<portobj portFile %d>", [portFile fileDescriptor]];
//}
@end
