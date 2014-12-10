/*
  Copyright (c) 1998-2003 Nick Ing-Simmons. All rights reserved.
  This program is free software; you can redistribute it and/or
  modify it under the same terms as Perl itself.
*/

#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>

#include "tkGlue.def"

#include "pTk/tkPort.h"
#include "pTk/tkInt.h"
#include "tkGlue.h"

typedef struct PerlEvent {
    Tcl_Event header;		/* Information that is standard for
				 * all events. */
    SV *infoPtr;		/* Pointer to file info structure.  Note
				 * that we still have to verify that the
				 * file exists before dereferencing this
				 * pointer. */
} PerlEvent;


static int initialized;

static void PerlEventInit(void);

static void
PerlSetupProc(ClientData data, int flags)
{
 static Tcl_Time blockTime = { 0, 0 };

    if (!(flags & TCL_FILE_EVENTS)) {
	return;
    }

    /*
     * Check to see if there is a ready file.  If so, poll.
     */
#if 0
    for (infoPtr = firstPerlPtr; infoPtr != NULL; infoPtr = infoPtr->nextPtr) {
	if (infoPtr->watchMask) {
	    Tcl_SetMaxBlockTime(&blockTime);
	    break;
	}
    }
#endif
}

static void
PerlCheckProc(data, flags)
    ClientData data;		/* Not used. */
    int flags;			/* Event flags as passed to Tcl_DoOneEvent. */
{
    PerlEvent *evPtr;
    SV *infoPtr;

    if (!(flags & TCL_FILE_EVENTS)) {
	return;
    }

    /*
     * Queue events for any ready files that don't already have events
     * queued (caused by persistent states that won't generate WinSock
     * events).
     */

#if 0
    for (infoPtr = firstPerlPtr; infoPtr != NULL; infoPtr = infoPtr->nextPtr) {
	if (infoPtr->watchMask && !(infoPtr->flags & FILE_PENDING)) {
	    infoPtr->flags |= FILE_PENDING;
	    evPtr = (PerlEvent *) ckalloc(sizeof(PerlEvent));
	    evPtr->header.proc = PerlEventProc;
	    evPtr->infoPtr = infoPtr;
	    Tcl_QueueEvent((Tcl_Event *) evPtr, TCL_QUEUE_TAIL);
	}
    }
#endif
}

static int
PerlEventProc(Tcl_Event *evPtr, int flags)
{
    PerlEvent *perlEvPtr = (PerlEvent *)evPtr;
    SV *infoPtr;

    if (!(flags & TCL_FILE_EVENTS)) {
	return 0;
    }

    /*
     * Search through the list of watched files for the one whose handle
     * matches the event.  We do this rather than simply dereferencing
     * the handle in the event so that files can be deleted while the
     * event is in the queue.
     */
#if 0
    for (infoPtr = firstFilePtr; infoPtr != NULL; infoPtr = infoPtr->nextPtr) {
	if (fileEvPtr->infoPtr == infoPtr) {
	    infoPtr->flags &= ~(FILE_PENDING);
	    Tcl_NotifyChannel(infoPtr->channel, infoPtr->watchMask);
	    break;
	}
    }
#endif

    return 1;
}

static void
PerlExitHandler(ClientData clientData)
{
 Tcl_DeleteEventSource(PerlSetupProc, PerlCheckProc, NULL);
 initialized = 0;
}

static void
PerlEventInit(void)
{
 initialized = 1;
 Tcl_CreateEventSource(PerlSetupProc, PerlCheckProc, NULL);
 Tcl_CreateExitHandler(PerlExitHandler, NULL);
}


