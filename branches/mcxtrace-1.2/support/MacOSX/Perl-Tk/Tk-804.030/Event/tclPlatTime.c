#define TCL_EVENT_IMPLEMENT
#ifdef WIN32
#include "../pTk/tclWinTime.c"
#else
#include "../pTk/tclUnixTime.c"
#endif
