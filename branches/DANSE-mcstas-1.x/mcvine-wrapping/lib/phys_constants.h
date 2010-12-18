// -*- C++ -*-
//
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
//                                   Jiao Lin
//                      California Institute of Technology
//                        (C) 2005 All Rights Reserved
//
// {LicenseText}
//
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//


// striped off from mcstas-r.h


#ifndef H_MCSATS2_PHYS_CONSTANTS
#define H_MCSATS2_PHYS_CONSTANTS

#define RAD2MIN  ((180*60)/PI)
#define MIN2RAD  (PI/(180*60))
#define DEG2RAD  (PI/180)
#define RAD2DEG  (180/PI)
#define AA2MS    629.719	   /* Convert k[1/AA] to v[m/s] */
#define MS2AA    1.58801E-3	   /* Convert v[m/s] to k[1/AA] */
#define K2V	 AA2MS
#define V2K	 MS2AA
#define Q2V	 AA2MS
#define V2Q	 MS2AA
#define SE2V	 437.3949	   /* Convert sqrt(E)[meV] to v[m/s] */
#define VS2E	 5.227e-6	   /* Convert (v[m/s])**2 to E[meV] */
#define FWHM2RMS 0.424660900144    /* Convert between full-width-half-max and */
#define RMS2FWHM 2.35482004503     /* root-mean-square (standard deviation) */
#define HBAR     1.05459E-34
#define MNEUTRON 1.67492E-27

#ifndef PI
# ifdef M_PI
#  define PI M_PI
# else
#  define PI 3.14159265358979323846
# endif
#endif

#endif // H_MCSATS2_PHYS_CONSTANTS

// version
// $Id$

// Generated automatically by CxxMill on Wed Jun 28 08:32:32 2006

// End of file 
