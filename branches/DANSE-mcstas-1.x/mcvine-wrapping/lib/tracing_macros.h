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



#ifndef H_MCSTAS2_TRACING_MACROS
#define H_MCSTAS2_TRACING_MACROS

/*
  macros used by mcstas coomponents.
 */


// stripped off from mcstas-r.h
// all macros are supposed by run inside the trace() method of class "Componnt"


// in original mcstas, there is a flag mcScattered to be used by Group
// and it is updated by this macro
#define SCATTER do{} while (0)

// 
#define ABSORB do {p=-1; return;} while (0) 
// should it be:
// #define ABSORB do {p=-1;} while (0) 

#include "propagator_macros.h"


#ifndef FLT_MAX
#define       FLT_MAX         3.40282347E+38F /* max decimal value of a "float" */
#endif


#define SCATTERED mcScattered


#endif //H_MCSTAS2_TRACING_MACROS

// version
// $Id$

// Generated automatically by CxxMill on Wed Jun 28 07:37:11 2006

// End of file 
