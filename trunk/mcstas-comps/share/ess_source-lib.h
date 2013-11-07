/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2013, All rights reserved
*         DTU Physics, Lyngby, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/ess_source-lib.h
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
#define ESS_SOURCE_LIB_H 0.1
#endif

/* List of brilliance definitions */
double ESS_Mezei_cold(double wavelength, double geometry);
double ESS_Mezei_thermal(double wavelength, double geometry);
double ESS_2012_Lieutenant_cold(double wavelength, double geometry);
double ESS_2013_Schoenfeldt_cold(double wavelength, double geometry);
double ESS_2013_Schoenfeldt_thermal(double wavelength, double geometry);

/* List of geometry definitions - mainly for mcdisplay... */
void ESS_mcdisplay_flat(double geometry);
void ESS_mcdisplay_TDRlike(double geometry);

/* end of ess_source-lib.h */
