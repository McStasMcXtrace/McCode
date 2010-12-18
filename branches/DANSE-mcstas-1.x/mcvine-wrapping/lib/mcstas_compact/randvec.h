// -*- C++ -*-

#ifndef H_McStas_compact_RANDVEC
#define H_McStas_compact_RANDVEC

#include "coords.h"

namespace McStas{
  void randvec_target_circle(double *xo, double *yo, double *zo, 
			     double *solid_angle, double xi, double yi, double zi, double radius);
#define randvec_target_sphere randvec_target_circle
  void randvec_target_rect_angular(double *xo, double *yo, double *zo, 
				   double *solid_angle,
				   double xi, double yi, double zi, double height, double width, Rotation A);        
  void randvec_target_rect(double *xo, double *yo, double *zo, 
			   double *solid_angle,
			   double xi, double yi, double zi, double height, double width, Rotation A); 
}//McStas

#endif // H_McStas_compact_RANDVEC
