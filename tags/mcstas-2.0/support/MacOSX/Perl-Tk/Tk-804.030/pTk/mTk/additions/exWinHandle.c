/*
  Copyright (c) 1998 Nick Ing-Simmons. All rights reserved.
  This program is free software; you can redistribute it and/or
  modify it under the same terms as FileHandle itself.
*/
#ifdef TCL_EVENT_IMPLEMENT

#include "tkPort.h"
#include "Lang.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <winsock.h>
#include "tkWinInt.h"

typedef struct FileHandler {
    int fd;
    int mask;			/* Mask of desired events: TCL_READABLE,
				 * etc. */
    int readyMask;		/* Mask of events that have been seen since the
				 * last time file handlers were invoked for
				 * this file. */
    int pending;
	HANDLE auxEvent;
    Tcl_FileProc *proc;		/* Procedure to call, in the style of
				 * Tcl_CreateFileHandler. */
    ClientData clientData;	/* Argument to pass to proc. */
    struct FileHandler *nextPtr;/* Next in list of all files we care about. */
} FileHandler;

typedef struct FileHandlerEvent {
    Tcl_Event header;		/* Information that is standard for
				 * all events. */
    int fd;			/* File descriptor that is ready.  Used
				 * to find the FileHandler structure for
				 * the file (can't point directly to the
				 * FileHandler structure because it could
				 * go away while the event is queued). */
} FileHandlerEvent;


static int initialized;

static FileHandler *firstFileHandlerPtr;

static void FileHandleEventInit(void);

static void
FileReadyProc(ClientData clientData, HANDLE handle)
{
    FileHandler *filePtr = (FileHandler *) clientData;
    filePtr->readyMask |= TCL_READABLE;
}


static void
FileHandleSetupProc(ClientData data, int flags)
{
    static Tcl_Time blockTime = { 0, 0 };
    FileHandler *filePtr;

    if (!(flags & TCL_FILE_EVENTS)) {
	return;
    }

    for (filePtr = firstFileHandlerPtr; filePtr != NULL; filePtr = filePtr->nextPtr) {
	if (!filePtr->readyMask && filePtr->mask & TCL_READABLE) {
	    HANDLE fh = (HANDLE) Lang_OSHandle(filePtr->fd);
#if 0
		/* Now see if the handle is a socket */
        char sockbuf[256];
        int optlen = sizeof(sockbuf);
    	int retval = getsockopt((SOCKET)fh, SOL_SOCKET, SO_TYPE, sockbuf, &optlen);
        if(retval == SOCKET_ERROR && WSAGetLastError() == WSAENOTSOCK) {
			/* Not a socket - use the raw handle */
#endif
		    Tcl_WatchHandle(fh, FileReadyProc, (ClientData) filePtr);
#if 0
		} else {
			/* Socket handle */
			if (!filePtr->auxEvent) {
 			  filePtr->auxEvent =  WSACreateEvent();
            }
			WSAEventSelect((SOCKET) fh, filePtr->auxEvent, FD_READ|FD_ACCEPT);
			Tcl_WatchHandle(filePtr->auxEvent, FileReadyProc, (ClientData) filePtr);
		}
#endif
	    break;
	}
    }

}

static int
FileHandlerEventProc(evPtr, flags)
    Tcl_Event *evPtr;		/* Event to service. */
    int flags;			/* Flags that indicate what events to
				 * handle, such as TCL_FILE_EVENTS. */
{
    FileHandler *filePtr;
    FileHandlerEvent *fileEvPtr = (FileHandlerEvent *) evPtr;
    int mask;

    if (!(flags & TCL_FILE_EVENTS)) {
	return 0;
    }

    /*
     * Search through the file handlers to find the one whose handle matches
     * the event.  We do this rather than keeping a pointer to the file
     * handler directly in the event, so that the handler can be deleted
     * while the event is queued without leaving a dangling pointer.
     */

    for (filePtr = firstFileHandlerPtr; filePtr != NULL;
	    filePtr = filePtr->nextPtr) {
	if (filePtr->fd != fileEvPtr->fd) {
	    continue;
	}

	/*
	 * The code is tricky for two reasons:
	 * 1. The file handler's desired events could have changed
	 *    since the time when the event was queued, so AND the
	 *    ready mask with the desired mask.
	 * 2. The file could have been closed and re-opened since
	 *    the time when the event was queued.  This is why the
	 *    ready mask is stored in the file handler rather than
	 *    the queued event:  it will be zeroed when a new
	 *    file handler is created for the newly opened file.
	 */

	mask = filePtr->readyMask & filePtr->mask;
	filePtr->readyMask = 0;
	filePtr->pending = 0;
	if (mask != 0) {
	    (*filePtr->proc)(filePtr->clientData, mask);
	}
	break;
    }
    return 1;
}

static void
FileHandleCheckProc(data, flags)
    ClientData data;		/* Not used. */
    int flags;			/* Event flags as passed to Tcl_DoOneEvent. */
{

    FileHandlerEvent *fileEvPtr;
    FileHandler *filePtr = firstFileHandlerPtr;

    if (!(flags & TCL_FILE_EVENTS)) {
	return;
    }

    while (filePtr)
     {
      if (filePtr->readyMask && !filePtr->pending)
       {
    	fileEvPtr = (FileHandlerEvent *) ckalloc(sizeof(FileHandlerEvent));
	    fileEvPtr->fd = filePtr->fd;
	    Tcl_QueueProcEvent(FileHandlerEventProc, (Tcl_Event *) fileEvPtr, TCL_QUEUE_TAIL);
    	filePtr->pending = 1;
       }
      filePtr = filePtr->nextPtr;
     }

}

static void
FileHandleExitHandler(ClientData clientData)
{
 Tcl_DeleteEventSource(FileHandleSetupProc, FileHandleCheckProc, NULL);
 initialized = 0;
}

static void
FileHandleEventInit(void)
{
 initialized = 1;
 firstFileHandlerPtr = NULL;
 Tcl_CreateEventSource(FileHandleSetupProc, FileHandleCheckProc, NULL);
 Tcl_CreateExitHandler(FileHandleExitHandler, NULL);
}


void
Tcl_CreateFileHandler(fd, mask, proc, clientData)
    int fd;			/* UNIX-like "FileHandle" of stream to watch. */
    int mask;			/* OR'ed combination of TCL_READABLE,
				 * TCL_WRITABLE, and TCL_EXCEPTION:
				 * indicates conditions under which
				 * proc should be called. */
    Tcl_FileProc *proc;		/* Procedure to call for each
				 * selected event. */
    ClientData clientData;	/* Arbitrary data to pass to proc. */
{
    FileHandler *filePtr;

    if (!initialized) {
	FileHandleEventInit();
    }

    for (filePtr = firstFileHandlerPtr; filePtr != NULL;
	    filePtr = filePtr->nextPtr) {
	if (filePtr->fd == fd) {
	    break;
	}
    }

    if (filePtr == NULL) {
	filePtr = (FileHandler*) ckalloc(sizeof(FileHandler)); /* MLK */
	filePtr->fd = fd;
	filePtr->readyMask = 0;
	filePtr->pending   = 0;
	filePtr->auxEvent  = 0;
	filePtr->nextPtr = firstFileHandlerPtr;
	firstFileHandlerPtr = filePtr;
    }
    filePtr->proc = proc;
    filePtr->clientData = clientData;
    filePtr->mask = mask;
}

void
Tcl_DeleteFileHandler(fd)
    int fd;		/* Stream id for which to remove callback procedure. */
{
    FileHandler *filePtr, *prevPtr;
    int index, bit, i;
    unsigned long flags;

    if (!initialized) {
	FileHandleEventInit();
    }

    /*
     * Find the entry for the given file (and return if there
     * isn't one).
     */

    for (prevPtr = NULL, filePtr = firstFileHandlerPtr; ;
	    prevPtr = filePtr, filePtr = filePtr->nextPtr) {
	if (filePtr == NULL) {
	    return;
	}
	if (filePtr->fd == fd) {
	    break;
	}
    }
    /*
     * Clean up information in the callback record.
     */

    if (prevPtr == NULL) {
	firstFileHandlerPtr = filePtr->nextPtr;
    } else {
	prevPtr->nextPtr = filePtr->nextPtr;
    }
	if (filePtr->auxEvent) {
		CloseHandle(filePtr->auxEvent);
    }
    ckfree((char *) filePtr);
}

#endif
