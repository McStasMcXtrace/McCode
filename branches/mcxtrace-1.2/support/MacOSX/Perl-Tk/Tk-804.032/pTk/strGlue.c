#include "Lang.h"

/* Various routines which map legacy string forms of calls
 * onto Tcl_Obj * equivalents.
 */

Tcl_Obj *
Tcl_GetVar2Ex(interp, varName, part2, flags)
Tcl_Interp *interp;
CONST char *varName;
CONST char *part2;
int flags;
{
 Tcl_Obj *temp = Tcl_NewStringObj(varName,-1);
 Tcl_Obj *temp2 = NULL;
 Tcl_Obj *ret;
 if (part2)
  {
   temp2 = Tcl_NewStringObj(part2,-1);
  }
 ret  = Tcl_ObjGetVar2(interp, temp, temp2, flags);
 Tcl_DecrRefCount(temp);
 if (temp2)
   Tcl_DecrRefCount(temp2);
 return ret;
}

CONST char *
Tcl_GetVar2(interp, varName, part2, flags)
Tcl_Interp *interp;
CONST char *varName;
CONST char *part2;
int flags;
{
 return Tcl_GetString(Tcl_GetVar2Ex(interp, varName, part2, flags));
}

CONST char *
Tcl_GetVar(interp, varName, flags)
Tcl_Interp *interp;
CONST char *varName;
int flags;
{
 return Tcl_GetVar2(interp,varName,NULL,flags);
}

CONST char *
Tcl_SetVar(interp, varName, newValue, flags)
Tcl_Interp *interp;
CONST char *varName;
CONST char *newValue;
int flags;
{
 Tcl_Obj *temp = Tcl_NewStringObj(varName,-1);
 Tcl_Obj *ret = Tcl_ObjSetVar2(interp, temp, NULL, Tcl_NewStringObj(newValue,-1), flags);
 Tcl_DecrRefCount(temp);
 return Tcl_GetString(ret);
}

int
Tcl_GetBoolean(interp, value, boolPtr)
Tcl_Interp *interp;
CONST char *value;
int *boolPtr;
{
 Tcl_Obj *temp = Tcl_NewStringObj(value,-1);
 int ret = Tcl_GetBooleanFromObj(interp, temp, boolPtr);
 Tcl_DecrRefCount(temp);
 return ret;
}

int
Tcl_GetDouble(interp, value, doublePtr)
Tcl_Interp *interp;
CONST char *value;
double *doublePtr;
{
 Tcl_Obj *temp = Tcl_NewStringObj(value,-1);
 int ret = Tcl_GetDoubleFromObj(interp, temp, doublePtr);
 Tcl_DecrRefCount(temp);
 return ret;
}

int
Tcl_GetInt(interp, value, intPtr)
Tcl_Interp *interp;
CONST char *value;
int *intPtr;
{
 Tcl_Obj *temp = Tcl_NewStringObj(value,-1);
 int ret = Tcl_GetIntFromObj(interp, temp, intPtr);
 Tcl_DecrRefCount(temp);
 return ret;
}

CONST char *
Tcl_GetStringResult(Tcl_Interp * interp)
{
 return Tcl_GetString(Tcl_GetObjResult(interp));
}




