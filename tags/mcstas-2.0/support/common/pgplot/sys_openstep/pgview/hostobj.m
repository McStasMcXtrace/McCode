// hostobj.m--This object manages the list of hosts that are allowed
// to connect.  Use addhost to add an address without prompting.  Typically
// this is only done for the localhost.  When you get a new peer name
// off the network, use queryhost to see if it is already in the list
// and if not, prompt the user to see if a connection should be allowed.
//
// 1999-Feb-21 - [AFT]
//---
#import "hostobj.h"
#import <sys/types.h>
#import <sys/socket.h>
#import <netinet/in.h>
#import <netdb.h>

@implementation hostobj

- initwithdispatch:(id)adispatch
{
      mydispatch = adispatch;
      hostlist = [Storage newCount:0
                  elementSize:4
                  description:"[i]"];

      hostmenu = [[NSMenu alloc]
         initWithTitle:[NSString stringWithCString:"Hosts"]];
      [hostmenu sizeToFit];
      return self;
}

- (NSMenu *) gethostmenu
{
      return hostmenu;;
}

- (void) addhost:(int)iaddr
{
      struct hostent *host;
      struct in_addr addr;

// Add to list of approved addresses
      [hostlist addElement:(void *)&iaddr];

// Get actual machine name and add to hosts submenu.
      addr.s_addr = iaddr;
      host = gethostbyaddr((char *)&addr, sizeof(addr), AF_INET);
      [[hostmenu addItemWithTitle:[NSString stringWithCString:host->h_name]
         action:@selector(removehost:)
         keyEquivalent:@""]
         setTarget:mydispatch];
      [hostmenu sizeToFit];
      return;
}

- (BOOL) queryhost:(int)isuspect
{
      struct hostent *host;
      struct in_addr addr;
      char     cbuf[128];
      int      *ihost;
      int      i, iresult, ltmp, nhost;

// If the suspect IP address is in the approved list, then return true.
      nhost=[hostlist count];
      for(i=0; i<nhost; i++) {
         ihost=[hostlist elementAt:i];
         if ( isuspect == *ihost ) {
            return TRUE;
         }
      }

// Not in approved list.  Get actual machine name.
      addr.s_addr = isuspect;
      host = gethostbyaddr((char *)&addr, sizeof(addr), AF_INET);
      ltmp = strlen(host->h_name);
      memcpy(&cbuf, host->h_name, ltmp);
      cbuf[ltmp]='\0';

// And prompt user to see if this is OK.
      iresult = NSRunAlertPanel(@"pgview:  Accept connection from",
         [NSString stringWithCString:cbuf],
         @"Accept",@"Reject",nil);

// If it is OK, then add to list of approved hosts.
      if ( iresult ) [self addhost:isuspect];

      return iresult;
}

- (void) removehost:(NSMenuItem *) sender
{
      struct hostent *ahost;
      char  cbuf[128];
      int   *ihost;
      int   i, iresult, isuspect, nhost;

// Come here if the user has selected a host from the hosts submenu.
// The menuItem title conveniently contains the host name.
      [[sender title] getCString:cbuf maxLength:128];
      ahost = gethostbyname(cbuf);
      if (ahost == 0) {
         fprintf(stderr, "%s: unknown host", cbuf);
      } else {
// We now have the host IP address.  Find entry in hostlist.
         memcpy(&isuspect, ahost->h_addr, 4);
         nhost=[hostlist count];
         for(i=0; i<nhost; i++) {
            ihost=[hostlist elementAt:i];
            if ( isuspect == *ihost ) {
// We now have the menuItem and the index into the hostlist.  Give the
// user a chance to change mind before deleting.
               iresult = NSRunAlertPanel(@"pgview:  Remove system?",
                  [NSString stringWithCString:cbuf],
                  @"Yes",@"No",nil);
               if ( iresult ) {
                  [hostlist removeElementAt:i];
                  [hostmenu removeItem:sender];
               }
               return;
            }
         }
         printf("pgview internal error, no IP address for=%s\n",cbuf);
      }
      return;
}

@end
