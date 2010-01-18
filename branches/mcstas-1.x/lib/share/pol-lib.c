/****************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2006, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/pol-lib.c
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
* Embedded within instrument in runtime mode.
* Variable names have prefix 'mc_pol_' for 'McStas Polarisation' 
* to avoid conflicts
*
* Usage: within SHARE
* %include "pol-lib"
*
* $Id: pol-lib.c,v 1.3 2006-08-31 12:56:59 pchr Exp $
*
* $Log: pol-lib.c,v $
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
#include "pol-lib.h"
#endif

/****************************************************************************
* void GetMonoPolFNFM(double Rup, double Rdown, double *FN, double *FM)
* 
* ACTION: Calculate FN and FM from reflectivities Rup and Rdown
*
* For a monochromator (nuclear and magnetic scattering), the
* polarisation is done by defining the reflectivity for spin up (Rup)
* and spin down (Rdown) (which can be negative, see now!) and based on
* this the nuclear and magnetic structure factors are calculated:
* FM = sign(Rup)*sqrt(|Rup|) + sign(Rdown)*sqrt(|Rdown|)
* FN = sign(Rup)*sqrt(|Rup|) - sign(Rdown)*sqrt(|Rdown|)
*****************************************************************************/
void GetMonoPolFNFM(double mc_pol_Rup, double mc_pol_Rdown, 
		    double *mc_pol_FN, double *mc_pol_FM){
  if (mc_pol_Rup>0)
    mc_pol_Rup   = sqrt(fabs(mc_pol_Rup));
  else
    mc_pol_Rup   = -sqrt(fabs(mc_pol_Rup));
  
  if (mc_pol_Rdown>0)
    mc_pol_Rdown = sqrt(fabs(mc_pol_Rdown));
  else
    mc_pol_Rdown = -sqrt(fabs(mc_pol_Rdown));
  
  *mc_pol_FN = 0.5*(mc_pol_Rup + mc_pol_Rdown);
  *mc_pol_FM = 0.5*(mc_pol_Rup - mc_pol_Rdown);
  return;
}

/****************************************************************************
* void GetMonoPolRefProb(double FN, double FM, double sy, double *prob)
* 
* ACTION: Calculate reflection probability from sy, FN and FM
*
* For a monochromator with up direction along y the reflection
* probability is given as:
* prob = FN*FN + 2*FN*FM*sy_in + FM*FM 
*     (= |Rup| + |Rdown| (for sy_in=0))
* where FN and FM are calculated from Rup and Rdown by GetMonoPolFNFM
*****************************************************************************/
void GetMonoPolRefProb(double mc_pol_FN, double mc_pol_FM, 
		       double mc_pol_sy, double *mc_pol_prob) {
  *mc_pol_prob = mc_pol_FN*mc_pol_FN + mc_pol_FM*mc_pol_FM
    + 2*mc_pol_FN*mc_pol_FM*mc_pol_sy;
  return;
}

/****************************************************************************
* void SetMonoPolRefOut(double FN, double FM, double refProb,
*		     double* sx, double* sy, double* sz) {
* 
* ACTION: Set the outgoing polarisation vector of the reflected neutrons
* given FN, FM and the reflection probability.
*
* For a monochromator with up direction along y the outgoing polarisation
* is given as:
*	sx = (FN*FN - FM*FM)*sx_in/R0;
*	sy = ((FN*FN - FM*FM)*sy_in + 2*FN*FM + FM*FM*sy_in)/R0;
*	sz = (FN*FN - FM*FM)*sz_in/R0;
* where sx_in, sy_in, and sz_in is the incoming polarisation, and
* FN and FM are calculated from Rup and Rdown by GetMonoPolFNFM
*****************************************************************************/
void SetMonoPolRefOut(double mc_pol_FN, double mc_pol_FM, 
		      double mc_pol_refProb, double* mc_pol_sx, 
		      double* mc_pol_sy, double* mc_pol_sz) {
  *mc_pol_sx = (mc_pol_FN*mc_pol_FN - mc_pol_FM*mc_pol_FM)*(*mc_pol_sx)
    /mc_pol_refProb;
  *mc_pol_sy = ((mc_pol_FN*mc_pol_FN - mc_pol_FM*mc_pol_FM)*(*mc_pol_sy) 
		+ 2*mc_pol_FN*mc_pol_FM + 2*mc_pol_FM*mc_pol_FM*(*mc_pol_sy))
    /mc_pol_refProb;
  *mc_pol_sz = (mc_pol_FN*mc_pol_FN - mc_pol_FM*mc_pol_FM)*(*mc_pol_sz)
    /mc_pol_refProb;
  return;
}

/****************************************************************************
* void SetMonoPolTransOut(double FN, double FM, double refProb, 
*			  double* sx, double* sy, double* sz) {
* 
* ACTION: Set the outgoing polarisation vector of the transmitted neutrons
* given FN, FM and the REFLECTION probability.
*
* We use that the polarization is conserved so:
* s_in = refProb*s_ref+(1-refProb)*s_trans, and then
* s_trans = (s_in-refProb*s_ref)/(1-refProb)
* where refProb is calculated using the routine GetMonoPolRefProb
* and s_ref is calculated by SetMonoPolRefOut
*****************************************************************************/
void SetMonoPolTransOut(double mc_pol_FN, double mc_pol_FM, 
			double mc_pol_refProb, double* mc_pol_sx, 
			double* mc_pol_sy, double* mc_pol_sz) {
  double mc_pol_sx_ref = *mc_pol_sx, mc_pol_sy_ref = *mc_pol_sy;
  double mc_pol_sz_ref = *mc_pol_sz;
  
  // By passing 1 as probability we get mc_pol_refProb*s_out_ref
  SetMonoPolRefOut(mc_pol_FN, mc_pol_FM, 1, 
		   &mc_pol_sx_ref, &mc_pol_sy_ref, &mc_pol_sz_ref);
  *mc_pol_sx = (*mc_pol_sx - mc_pol_sx_ref)/(1 - mc_pol_refProb);
  *mc_pol_sy = (*mc_pol_sy - mc_pol_sy_ref)/(1 - mc_pol_refProb);
  *mc_pol_sz = (*mc_pol_sz - mc_pol_sz_ref)/(1 - mc_pol_refProb);
  return;
}

/****************************************************************************
* void SimpleNumMagnetPrecession(double x, double y, double z, double t,
*			         double vx, double vy, double vz,
*			         double* sx, double* sy, double* sz, double dt)
* 
*****************************************************************************/
void SimpleNumMagnetPrecession(double mc_pol_x, double mc_pol_y, 
			       double mc_pol_z, double mc_pol_time,
			       double mc_pol_vx, double mc_pol_vy, 
			       double mc_pol_vz,
			       double* mc_pol_sx, double* mc_pol_sy, 
			       double* mc_pol_sz, double mc_pol_deltaT,
			       Coords mc_pol_posLM, Rotation mc_pol_rotLM) {
  
  double mc_pol_Bx, mc_pol_By, mc_pol_Bz, mc_pol_phiz;
  double mc_pol_BxStart, mc_pol_ByStart, mc_pol_BzStart, mc_pol_Bstart;
  double mc_pol_BxTemp, mc_pol_ByTemp, mc_pol_BzTemp, mc_pol_Btemp;
  double mc_pol_Bstep, mc_pol_timeStep, mc_pol_sp;
  const double mc_pol_spThreshold  = cos(1.0*DEG2RAD);
  const double mc_pol_startTimeStep = 1e-5; // s
  double dummy1, dummy2;
  Rotation mc_pol_rotBack;
  
  // change coordinates from local system to magnet system
  mccoordschange(mc_pol_posLM, mc_pol_rotLM,
		 &mc_pol_x, &mc_pol_y, &mc_pol_z, 
		 &mc_pol_vx, &mc_pol_vy, &mc_pol_vz, &mc_pol_time,
		 &dummy1, &dummy2);
  mccoordschange_polarisation(mc_pol_rotLM, mc_pol_sx, mc_pol_sy, mc_pol_sz);
  
  // get initial B-field value
  mcMagneticField(mc_pol_x, mc_pol_y, mc_pol_z, mc_pol_time, 
		  &mc_pol_BxTemp, &mc_pol_ByTemp, &mc_pol_BzTemp);
  
  do {
    
    mc_pol_Bx = 0; mc_pol_By = 0; mc_pol_Bz = 0; mc_pol_phiz = 0; 
    mc_pol_BxStart = mc_pol_BxTemp; mc_pol_ByStart = mc_pol_ByTemp; 
    mc_pol_BzStart = mc_pol_BzTemp;
    mc_pol_Bstart = 
      sqrt(mc_pol_BxStart*mc_pol_BxStart + mc_pol_ByStart*mc_pol_ByStart 
	   + mc_pol_BzStart*mc_pol_BzStart);
    mc_pol_timeStep = mc_pol_startTimeStep;
    
    if(mc_pol_deltaT<mc_pol_timeStep)
      mc_pol_timeStep = mc_pol_deltaT;
    
    do {
      
      mcMagneticField(mc_pol_x+mc_pol_vx*mc_pol_timeStep, 
		      mc_pol_y+mc_pol_vy*mc_pol_timeStep, 
		      mc_pol_z+mc_pol_vz*mc_pol_timeStep, 
		      mc_pol_time+mc_pol_timeStep,
		      &mc_pol_BxTemp, &mc_pol_ByTemp, &mc_pol_BzTemp);
      // not so elegant, but this is how we kame sure that the steps decrease
      // when the WHILE condition is not met
      mc_pol_timeStep *= 0.5;
      
      mc_pol_Btemp = 
	sqrt(mc_pol_BxTemp*mc_pol_BxTemp + mc_pol_ByTemp*mc_pol_ByTemp 
	     + mc_pol_BzTemp*mc_pol_BzTemp);
      
      mc_pol_sp =
	scalar_prod(mc_pol_BxStart, mc_pol_ByStart, mc_pol_BzStart, 
		    mc_pol_BxTemp, mc_pol_ByTemp, mc_pol_BzTemp);
      mc_pol_sp /= mc_pol_Bstart*mc_pol_Btemp;
      
    } while (mc_pol_sp<mc_pol_spThreshold);
    
    mc_pol_timeStep*=2;
    
    // update coordinate values
    mc_pol_x += mc_pol_vx*mc_pol_timeStep;
    mc_pol_y += mc_pol_vy*mc_pol_timeStep; 
    mc_pol_z += mc_pol_vz*mc_pol_timeStep; 
    mc_pol_time += mc_pol_timeStep;
    mc_pol_deltaT -= mc_pol_timeStep;
    
    mc_pol_Bx = 0.5 * (mc_pol_BxStart + mc_pol_BxTemp);
    mc_pol_By = 0.5 * (mc_pol_ByStart + mc_pol_ByTemp);
    mc_pol_Bz = 0.5 * (mc_pol_BzStart + mc_pol_BzTemp);
    mc_pol_phiz = fmod(sqrt(mc_pol_Bx*mc_pol_Bx+
			    mc_pol_By*mc_pol_By+
			    mc_pol_Bz*mc_pol_Bz)
		       *mc_pol_timeStep*mc_pol_omegaL, 2*PI);

    // Do the neutron spin precession
    
    if(!(mc_pol_Bx==0 && mc_pol_By==0 && mc_pol_Bz==0)) {
      
      double mc_pol_sx_in = *mc_pol_sx;
      double mc_pol_sy_in = *mc_pol_sy;
      double mc_pol_sz_in = *mc_pol_sz;
      
      rotate(*mc_pol_sx, *mc_pol_sy, *mc_pol_sz, 
	     mc_pol_sx_in, mc_pol_sy_in, mc_pol_sz_in, 
	     mc_pol_phiz, mc_pol_Bx, mc_pol_By, mc_pol_Bz);
    }
    
  } while (mc_pol_deltaT>0);
  
  // change back spin coordinates from magnet system to local system
  rot_transpose(mc_pol_rotLM, mc_pol_rotBack); 
  mccoordschange_polarisation(mc_pol_rotBack, mc_pol_sx, mc_pol_sy, mc_pol_sz);
}

/****************************************************************************
* double GetConstantField(double length, double lambda, double angle)
* 
* Return the magnetic field in Tesla required to flip a neutron with
* wavelength lambda(1/velocity), angle degrees, over the specified 
* length(=time*velocity).
* 
*****************************************************************************/
double GetConstantField(double mc_pol_length, double mc_pol_lambda, 
			double mc_pol_angle)
{
  const double mc_pol_velocity = K2V*2*PI/mc_pol_lambda;
  const double mc_pol_time = mc_pol_length/mc_pol_velocity;
  
  // B*omegaL*time = angle
  return mc_pol_angle*DEG2RAD/mc_pol_omegaL/mc_pol_time; // T
}

/* end of pol-lib.c */
