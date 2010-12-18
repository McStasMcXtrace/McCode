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



#ifndef H_MCSTAS2_MISC_MACROS
#define H_MCSTAS2_MISC_MACROS


// stripped off from mcstas-r.h
// all macros are supposed by run inside the trace() method of class "Componnt"


#define NAME_CURRENT_COMP const_cast<char *>(m_name.c_str())


//count normalization should be done in the application (instrument) level
//not component level
inline void mcset_ncount(double count) {throw;}
inline double mcget_ncount(void) {return 1.0;}
inline double mcget_run_num(void) {return 1.0;}
// int mcstas_main(int argc, char *argv[]);

double mcestimate_error(double N, double p1, double p2);

#ifndef FLT_MAX
#define       FLT_MAX         3.40282347E+38F /* max decimal value of a "float" */
#endif


#define SCATTERED mcScattered


#endif //H_MCSTAS2_MISC_MACROS

// version
// $Id$

// Generated automatically by CxxMill on Wed Jun 28 07:37:11 2006

// End of file 
