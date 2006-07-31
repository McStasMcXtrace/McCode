/*****************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2006, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/pol-lib.h
*
* %Identification
* Written by: Peter Christiansen
* Date: August, 2006
* Origin: RISOE
* Release: McStas 1.10
* Version: $Revision: 1.1 $
*
* This file is to be imported by polarisation components.
* It handles some shared functions.
*
* This library may be used directly as an external library. 
* It has no dependency.
*
* Usage: within SHARE
* %include "pol-lib"
*
*
* $Id: pol-lib.h,v 1.1 2006-07-31 13:17:10 pchr Exp $
*
* $Log: not supported by cvs2svn $
*
****************************************************************************/

#ifndef POL_LIB_H
#define POL_LIB_H "$Revision: 1.1 $"

// Routines used for Monochromator and guides/mirrors 
// in the special (usual) case where
// the up direction is parallel to the y-axis and 
// the down direction is anti-parallel to the y-axis 
void GetMonoPolFNFM(double, double, double*, double*);
void GetMonoPolRef(double, double, double, double*);
void SetMonoPolRefOut(double, double, double, double*, double*, double*);
void SetMonoPolTransOut(double, double, double, double*, double*, double*);

#endif

/* end of pol-lib.h */
