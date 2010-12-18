// -*- C++ -*-

#ifndef H_mcstas2_numeric_abs
#define H_mcstas2_numeric_abs


#include <math.h>
#include <stdlib.h>

namespace mcstas2 {

  /* wrap function abs. There should be better ways to do that.
   */

  inline int abs( int x ) { return ::abs(x); }
  inline long abs( long x ) { return labs(x); }
  inline double abs( double x ) { return fabs(x); }
  inline float abs( float x ) { return fabsf(x); }

} // mcstas2


#endif // H_mcstas2_numeric_abs
