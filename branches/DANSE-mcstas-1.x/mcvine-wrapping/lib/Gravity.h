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


#ifndef H_MCSTAS2_GRAVITY
#define H_MCSTAS2_GRAVITY

namespace mcstas2{

  struct Gravity{

    Gravity( double in_x=0, double in_y=-1, double in_z=0 ) :
      dir_x(in_x), dir_y(in_y), dir_z(in_z),
      x(dir_x*amplitude), y(dir_y*amplitude), z(dir_z*amplitude)
    { }

    double dir_x, dir_y, dir_z;  // direction
    double x, y, z; 
    const static double amplitude; // m/s**2
      
  }; // Gravity

} // namespace mcstas2

// 
#define GRAVITY (mcstas2::Gravity::amplitude)  /* [m/s^2] gravitational acceleration */
extern int    mcgravitation;

#define PROP_GRAV_DT(dt, Ax, Ay, Az) \
  do { \
    prop_dt_withgravity(dt, Ax, Ay, Az, x,y,z, vx,vy,vz, t);	\
  } while(0)


#endif // H_MCSTAS2_GRAVITY

// version
// $Id$

// End of file 
