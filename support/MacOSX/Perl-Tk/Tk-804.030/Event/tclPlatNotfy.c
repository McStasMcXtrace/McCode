#define TCL_EVENT_IMPLEMENT
#ifdef WIN32
#include "../pTk/tclWinNotify.c"

void
Tcl_WatchHandle(HANDLE h, Tcl_HandleProc *proc, ClientData clientData)
{
#if 0
 int i = 0;
 while (i < notifier.hCount)
  {
   if (notifier.hArray[i] == h)
    break;
   i++;
  }
 if (i == notifier.hCount)
  {
   if (notifier.hCount < MAXIMUM_WAIT_OBJECTS)
    {
     notifier.hArray[i] = h;
     notifier.hCount++;
    }
  }
 if (i < notifier.hCount)
  {
   notifier.pArray[i].proc = proc;
   notifier.pArray[i].clientData = clientData;
  }
#endif
}


#else
#include "../pTk/tclUnixNotfy.c"
#endif


/*
 * Keep a record of the original Notifier procedures, created in the
 * same compilation unit as the stub tables so we can later do reliable,
 * portable comparisons to see whether a Tcl_SetNotifier() call swapped
 * new routines into the stub table.
 */

Tcl_NotifierProcs tclOriginalNotifier = {
    Tcl_SetTimer,
    Tcl_WaitForEvent,
#if !defined(__WIN32__) && !defined(MAC_TCL) /* UNIX */
    Tcl_CreateFileHandler,
    Tcl_DeleteFileHandler,
#else
    NULL,
    NULL,
#endif
    NULL,
    NULL,
    NULL,
    NULL
};


