#define TCL_EVENT_IMPLEMENT
#ifdef WIN32
#include "../pTk/exWinHandle.c"
#else
#include "../pTk/tclUnixEvent.c"
#endif
