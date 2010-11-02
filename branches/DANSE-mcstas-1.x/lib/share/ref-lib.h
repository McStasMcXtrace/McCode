/*****************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2006, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/ref-lib.h
*
* %Identification
* Written by: Peter Christiansen
* Date: August, 2006
* Origin: RISOE
* Release: McStas 1.10
* Version: $Revision: 1.1 $
*
* Commonly used reflection functions are declared in this file which
* are used by some guide and mirror components.
*
* Depends on read_table-lib
*
* Usage: within SHARE
* %include "ref-lib"
*
*
* $Id: ref-lib.h,v 1.1 2006-08-10 13:10:49 pchr Exp $
*
* $Log: ref-lib.h,v $
* Revision 1.1  2006-08-10 13:10:49  pchr
* Added new library with reflection functions.
*
*
*
****************************************************************************/
%include "read_table-lib"

#ifndef REF_LIB_H
#define REF_LIB_H "$Revision: 1.1 $"

void StdReflecFunc(double, double*, double*);
void TableReflecFunc(double, t_Table*, double*);

#endif

/* end of ref-lib.h */
