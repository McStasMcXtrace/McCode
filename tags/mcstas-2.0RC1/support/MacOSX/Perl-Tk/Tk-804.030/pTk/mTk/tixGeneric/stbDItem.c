#include "tkInt.h"
#include "tixPort.h"
#include "tix.h"
#include "tkVMacro.h"

static int   DItemParseProc _ANSI_ARGS_((ClientData clientData,
		Tcl_Interp *interp, Tk_Window tkwin, Tcl_Obj * value,
		char *widRec, int offset));

static Tcl_Obj *   DItemPrintProc _ANSI_ARGS_((
		ClientData clientData, Tk_Window tkwin, char *widRec,
		int offset, Tcl_FreeProc **freeProcPtr));

static int
DItemParseProc(clientData, interp, tkwin, value, widRec,offset)
    ClientData clientData;
    Tcl_Interp *interp;
    Tk_Window tkwin;
    Tcl_Obj * value;
    char *widRec;
    int offset;
{
 return TixDItemParseProc(clientData, interp, tkwin, value, widRec,offset);
}

static Tcl_Obj *
DItemPrintProc(clientData, tkwin, widRec,offset, freeProcPtr)
    ClientData clientData;
    Tk_Window tkwin;
    char *widRec;
    int offset;
    Tcl_FreeProc **freeProcPtr;
{
 return TixDItemPrintProc(clientData, tkwin, widRec,offset, freeProcPtr);
}


Tk_CustomOption tixConfigItemType = {
    DItemParseProc, DItemPrintProc, 0,
};

