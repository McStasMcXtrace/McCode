#import "pgvSpeaker.h"
#import <stdio.h>
// POSIX defines sleep in <unistd.h> which does not exist on the NeXT, sigh.
unsigned int sleep(unsigned int seconds);

void mkspeak( id *retSpeaker)
//
// This routine creates a Speaker object for communicating with the
// PGPLOT viewer.  If there is no Listener then the pgview program is
// launched, and waits up to 11 secs to connect to the port.  Of course,
// this means the pgview must lie in your current path.
//
{
#include "sername.h"
//#include <stdlib.h>

    port_t thePort;
    id     mySpeaker;
    int    icnt;

    mySpeaker = [[pgvSpeaker alloc] init];
    thePort = NXPortFromName(PGV_SERVER_NAME, NULL);
    if (thePort==PORT_NULL) {
        printf("Launching pgview...\n");
//        system("open -a pgview &");
        system("open /LocalApps/pgview.app/pgview");
        icnt=1;
        while (thePort==PORT_NULL && icnt<21) {
            sleep(1);
            if((icnt/5)*5 == icnt) printf("waiting...\n");
            thePort = NXPortFromName(PGV_SERVER_NAME, NULL);
            icnt=icnt+1;
        } /* end while */
        if (thePort==PORT_NULL) {
            printf("Could not find port connected to pgview.\n");
            exit(1);
        } /* end if */
    } /* end if */
    [mySpeaker setSendPort:thePort];

    *retSpeaker=mySpeaker;
}
