/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2013, All rights reserved
*         DTU Physics, Lyngby, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/ess_source-lib.c
*
* %Identification
* Written by: PW
* Date: Nov 7, 2013
* Origin: DTU Physics
* Release: McStas 2.1
* Version: 0.1
*
* This file is to be imported by the ESS_moderator_long component
* It defines a set of brilliance definitions (used via function pointer) for
* easier use of the component.
*
* Usage: within SHARE
* %include "ess_source-lib"
*
*******************************************************************************/

#ifndef ESS_SOURCE_LIB_H
#error McStas : please import this library with %include "ess_source-lib"
#endif

/* This is the cold Mezei moderator from 2001 */
double ESS_Mezei_cold(double wavelength, double geometry) 
{
  //spectrum related constants - ESS 2001 Cold moderator
  //T=50, tau=287e-6, tau1=0, tau2=20e-6, chi2=0.9, I0=6.9e11, I2=27.6e10, branch1=0, branch2=0.5;

} /* end of ESS_Mezei_cold */


/* This is the thermal Mezei moderator from 2001 */
double ESS_Mezei_thermal(double wavelength, double geometry)
{
  //spectrum related constants - ESS 2001 Thermal moderator       
  //T_t=325, tau_t=80e-6, tau1_t=400e-6, tau2_t=12e-6, chi2_t=2.5, I0_t=13.5e11, I2_t=27.6e10, branch1_t=0.5, branch2_t=0.5;

} /* end of ESS_Mezei_thermal */


/* This is the Mezei moderator with a correction term from Klaus Lieutenant */
double ESS_2012_Lieutenant_cold(double wavelength, double geometry)
{
  /* spectrum related constants - ESS 2001 Cold moderator */
  /* T=50, tau=287e-6, tau1=0, tau2=20e-6, chi2=0.9, I0=6.9e11, I2=27.6e10, branch1=0, branch2=0.5; */

  /* /\* Correction factors to converts 'predicted' spectrum from cold moderator to the one observed in MCNPX *\/ */
  /* if (lambda<=2.5) cor=log(1.402+0.898*lambda)*(2.0776-4.1093*lambda+4.8836*pow(lambda,2)-2.4715*pow(lambda,3)+0.4521*pow(lambda,4)); */
  /* else if (lambda <= 3.5) cor = log(1.402 + 0.898*lambda)*(4.3369 - 1.8367*lambda + 0.2524*pow(lambda,2) ); */
  /* else if (lambda  > 3.5) cor = log(1.402 + 0.898*lambda); */

} /* end of ESS_2012_Lieutenant_cold */


/* This is the cold moderator with 2013 updates, fits from Troels Schoenfeldt */
/* Parametrization including moderator height for the "pancake" moderator */
double ESS_2013_Schoenfeldt_cold(double wavelength, double geometry)
{
} /* end of ESS_2013_Schoenfeldt_cold */


/* This is the thermal moderator with 2013 updates, fits from Troels Schoenfeldt */
double ESS_2013_Schoenfeldt_thermal(double wavelength, double geometry)
{
} /* end of ESS_2013_Schoenfeldt_thermal */

/* Display of geometry - flat and TDR-like */
void ESS_mcdisplay_flat(double geometry)
{
}/* end of ESS_mcdisplay_flat */

void ESS_mcdisplay_TDRlike(double geometry)
{
}/* end of ESS_mcdisplay_TDRlike */


/* end of ess_source-lib.c */
