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
* Version: $Revision: 1.3 $
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
* $Id: pol-lib.h,v 1.3 2006-08-31 12:56:59 pchr Exp $
*
* $Log: pol-lib.h,v $
* Revision 1.3  2006-08-31 12:56:59  pchr
* Removed the two components Pol_filter and Pol_arm and have instead made
* a special Set_pol component which can do this kind of unphysical but very
* useful trick to hardcode the polarisation.
* Updated examples using Pol_filter and Pol_arm.
*
* Made the larmor frequency into a constant (define) in pol-lib and
* made a small function that calculates the constant field required for
* a certain precession angle given magnetLength and lambda.
* This is now used in Test_Magnetic_Constant.
*
* Revision 1.2  2006/08/28 10:12:25  pchr
* Basic infrastructure for spin propagation in magnetic fields.
*
* Revision 1.1  2006/07/31 13:17:10  pchr
* Made a library with some polarisation routines.
*
*
****************************************************************************/

#ifndef POL_LIB_H
#define POL_LIB_H "$Revision: 1.3 $"

// Constant used 
#define mc_pol_omegaL (-2 * PI * 29.16e6) /* MHz*rad/Tesla */

// Routines used for Monochromator and guides/mirrors 
// in the special (usual) case where
// the up direction is parallel to the y-axis and 
// the down direction is anti-parallel to the y-axis 
void GetMonoPolFNFM(double, double, double*, double*);
void GetMonoPolRef(double, double, double, double*);
void SetMonoPolRefOut(double, double, double, double*, double*, double*);
void SetMonoPolTransOut(double, double, double, double*, double*, double*);

// Routines for spin precession in magnetic fields
void SimpleNumMagnetPrecession(double, double, double, double, double, double, 
			       double, double*, double*, double*, double, 
			       Coords, Rotation);

// Routines to help calculate the rquired magnetic field
double GetConstantField(double, double, double);

#endif

/* end of pol-lib.h */
