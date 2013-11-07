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

double ESS_Mezei_cold(double wavelength, string geometry) 
{
} /* end of ESS_Mezei_cold */

double ESS_Mezei_thermal(double wavelength, string geometry)
{
} /* end of ESS_Mezei_thermal */

double ESS_2012_Lieutenant_cold(double wavelength, string geometry)
{
} /* end of ESS_2012_Lieutenant_cold */

double ESS_2013_Schoenfeldt_cold(double wavelength, string geometry)
{
} /* end of ESS_2013_Schoenfeldt_cold */

double ESS_2013_Schoenfeldt_thermal(double wavelength, string geometry)
{
} /* end of ESS_2013_Schoenfeldt_thermal */

/* end of ess_source-lib.c */
