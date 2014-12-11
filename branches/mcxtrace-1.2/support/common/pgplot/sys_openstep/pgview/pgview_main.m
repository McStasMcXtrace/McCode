#import <AppKit/AppKit.h>
#import <Foundation/NSAutoreleasePool.h>
#import "portobj.h"
#import "dispatchobj.h"

#include <stdio.h>

void main(int argc, char *argv[]) {

      portobj        *myport;
      dispatchobj    *mydispatch;
      NSAutoreleasePool *pool;
      NSConnection   *conn;
      NSMenu         *mymenu, *newmenu;
      NSMenuItem     *hitem, *nitem;
      unsigned short iport=7974;

/* Black magic to make everything work */
      [NSApplication sharedApplication];
      pool = [[NSAutoreleasePool alloc] init];

/* Create a dispatch object */
      mydispatch = [[dispatchobj alloc] init];

/* Create the port object */
      myport = [[portobj alloc] initport:iport target:mydispatch];

/* Black magic to get sockets to work. */
      conn = [NSConnection connectionWithReceivePort:myport sendPort:myport];
      if (conn == nil) {
         NSLog(@"couldn't create a connection!");
         [pool release];
         exit(1);
      }
      [conn setRootObject:mydispatch];

/* Create the "new" submenu */
      newmenu = [[NSMenu alloc]
         initWithTitle:[NSString stringWithCString:"New"]];
      [[newmenu addItemWithTitle:[NSString stringWithCString:"Landscape"]
         action:@selector(newLand)
         keyEquivalent:[NSString stringWithCString:"L"]]
         setTarget:mydispatch];
      [[newmenu addItemWithTitle:[NSString stringWithCString:"Portrait"]
         action:@selector(newPort)
         keyEquivalent:[NSString stringWithCString:"O"]]
         setTarget:mydispatch];

/* Create the menu */
      mymenu = [[NSMenu alloc]
         initWithTitle:[NSString stringWithCString:"pgview"]];
      [[mymenu addItemWithTitle:[NSString stringWithCString:"Info..."]
         action:@selector(showInfo)
         keyEquivalent:@""]
         setTarget:mydispatch];
      nitem=[mymenu addItemWithTitle:[NSString stringWithCString:"New"]
         action:NULL
         keyEquivalent:@""];
      [mymenu setSubmenu:newmenu forItem:nitem];
      hitem=[mymenu addItemWithTitle:[NSString stringWithCString:"Hosts"]
         action:NULL
         keyEquivalent:@""];
      [mymenu setSubmenu:[myport gethostmenu] forItem:hitem];
      [[mymenu addItemWithTitle:[NSString stringWithCString:"Print"]
         action:@selector(pgprint)
         keyEquivalent:[NSString stringWithCString:"p"]]
         setTarget:mydispatch];
      [[mymenu addItemWithTitle:[NSString stringWithCString:"Deactivate"]
         action:@selector(deactive)
         keyEquivalent:[NSString stringWithCString:"d"]]
         setTarget:mydispatch];
      [mymenu addItemWithTitle:[NSString stringWithCString:"Hide"]
         action:@selector(hide:)
         keyEquivalent:[NSString stringWithCString:"h"]];
      [mymenu addItemWithTitle:[NSString stringWithCString:"Quit"]
         action:@selector(terminate:)
         keyEquivalent:[NSString stringWithCString:"q"]];
      [mymenu sizeToFit];
      [NSApp setMainMenu:mymenu];

      [NSApp run];

      [pool release];

      exit(0);
}
