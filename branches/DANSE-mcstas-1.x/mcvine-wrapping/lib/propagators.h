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


#ifndef H_MCSTAS2_PROPAGATORS
#define H_MCSTAS2_PROPAGATORS


//strip off from mcstas-r.h

#include "geometry.h"
#include "Component.h"
#include "tracing_macros.h"


inline void prop_dt_nogravity
(const double dt, 
 double &x, double &y, double &z, 
 const double &vx, const double &vy, const double &vz,
 double & t)
{
  x += vx*(dt); 
  y += vy*(dt); 
  z += vz*(dt); 
  t += (dt); 
}
  

inline void prop_dt_withgravity
(const double & dt, const double &Ax, const double &Ay, const double &Az,
 double &x, double &y, double &z, 
 double &vx, double &vy, double &vz,
 double & t)
{
  x  += vx*dt + Ax*dt*dt/2;   
  y  += vy*dt + Ay*dt*dt/2; 
  z  += vz*dt + Az*dt*dt/2; 
  vx += Ax*dt; 
  vy += Ay*dt; 
  vz += Az*dt; 
  t  += dt; 
}


inline void prop_dt
(const double & dt, const mcstas2::Component & comp,
 double &x, double &y, double &z, 
 double &vx, double &vy, double &vz,
 double &p,
 double &t)
{
  if (dt<0) {ABSORB; return;}
  const mcstas2::Gravity & g = comp.gravity();

  if (comp.gravityIsOn()) prop_dt_withgravity( dt, g.x, g.y, g.z, x,y,z, vx,vy,vz, t);
  else prop_dt_nogravity( dt, x,y,z, vx,vy,vz, t );
}


inline void prop_z0_nogravity
(double &x, double &y, double &z, 
 const double &vx, const double &vy, const double &vz,
 double &p,
 double & t)
{
  double dt;
  if(vz == 0) { ABSORB; return; }
  dt = -z/vz;
  prop_dt_nogravity( dt, x,y,z, vx,vy,vz, t );
}



inline void prop_z0_withgravity
(const double &Ax, const double &Ay, const double &Az,
 double &x, double &y, double &z, 
 double &vx, double &vy, double &vz,
 double &p,
 double & t)
{
  double dt;
  if (plane_intersect_Gfast(&dt, -Az/2, -vz, -z) && dt>0) 
    prop_dt_withgravity(dt, Ax, Ay, Az, x,y,z, vx,vy,vz, t); 
  else {ABSORB;return;}
}


inline void prop_z0
(const mcstas2::Component & comp,
 double &x, double &y, double &z, 
 double &vx, double &vy, double &vz,
 double &p,
 double &t)
{
  const mcstas2::Gravity & g = comp.gravity();

  if (comp.gravityIsOn()) prop_z0_withgravity( g.x, g.y, g.z, x,y,z, vx,vy,vz, p, t);
  else prop_z0_nogravity( x,y,z, vx,vy,vz, p, t ); 
}


#define vec_prod(x, y, z, x1, y1, z1, x2, y2, z2) \
  do { \
    double mcvp_tmpx, mcvp_tmpy, mcvp_tmpz; \
    mcvp_tmpx = (y1)*(z2) - (y2)*(z1); \
    mcvp_tmpy = (z1)*(x2) - (z2)*(x1); \
    mcvp_tmpz = (x1)*(y2) - (x2)*(y1); \
    (x) = mcvp_tmpx; (y) = mcvp_tmpy; (z) = mcvp_tmpz; \
  } while(0)

#define scalar_prod(x1, y1, z1, x2, y2, z2) \
  ((x1)*(x2) + (y1)*(y2) + (z1)*(z2))

#define NORM(x,y,z) \
  do { \
    double mcnm_tmp = sqrt((x)*(x) + (y)*(y) + (z)*(z)); \
    if(mcnm_tmp != 0.0) \
    { \
      (x) /= mcnm_tmp; \
      (y) /= mcnm_tmp; \
      (z) /= mcnm_tmp; \
    } \
  } while(0)

#define rotate(x, y, z, vx, vy, vz, phi, ax, ay, az) \
  do { \
    double mcrt_tmpx = (ax), mcrt_tmpy = (ay), mcrt_tmpz = (az); \
    double mcrt_vp, mcrt_vpx, mcrt_vpy, mcrt_vpz; \
    double mcrt_vnx, mcrt_vny, mcrt_vnz, mcrt_vn1x, mcrt_vn1y, mcrt_vn1z; \
    double mcrt_bx, mcrt_by, mcrt_bz; \
    double mcrt_cos, mcrt_sin; \
    NORM(mcrt_tmpx, mcrt_tmpy, mcrt_tmpz); \
    mcrt_vp = scalar_prod((vx), (vy), (vz), mcrt_tmpx, mcrt_tmpy, mcrt_tmpz); \
    mcrt_vpx = mcrt_vp*mcrt_tmpx; \
    mcrt_vpy = mcrt_vp*mcrt_tmpy; \
    mcrt_vpz = mcrt_vp*mcrt_tmpz; \
    mcrt_vnx = (vx) - mcrt_vpx; \
    mcrt_vny = (vy) - mcrt_vpy; \
    mcrt_vnz = (vz) - mcrt_vpz; \
    vec_prod(mcrt_bx, mcrt_by, mcrt_bz, \
	     mcrt_tmpx, mcrt_tmpy, mcrt_tmpz, mcrt_vnx, mcrt_vny, mcrt_vnz); \
    mcrt_cos = cos((phi)); mcrt_sin = sin((phi)); \
    mcrt_vn1x = mcrt_vnx*mcrt_cos + mcrt_bx*mcrt_sin; \
    mcrt_vn1y = mcrt_vny*mcrt_cos + mcrt_by*mcrt_sin; \
    mcrt_vn1z = mcrt_vnz*mcrt_cos + mcrt_bz*mcrt_sin; \
    (x) = mcrt_vpx + mcrt_vn1x; \
    (y) = mcrt_vpy + mcrt_vn1y; \
    (z) = mcrt_vpz + mcrt_vn1z; \
  } while(0)


#endif //H_MCSTAS2_PROPAGATORS



// version
// $Id$

// Generated automatically by CxxMill on Wed Jun 28 10:28:16 2006

// End of file 
