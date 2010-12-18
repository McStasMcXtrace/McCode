// -*- C++ -*-

#ifndef H_McStas_compact_COORDS
#define H_McStas_compact_COORDS
#include <numeric>
#include <math.h>

namespace McStas{
  typedef double MCNUM;
  typedef struct {MCNUM x, y, z;} Coords;
  typedef MCNUM Rotation[3][3];

  template <typename T>
  void vec_prod(T &x, T &y, T &z, 
		const T & x1, const T & y1, const T & z1, const T & x2, const T & y2, const T & z2) ;

  template <typename T>
  T scalar_prod(const T & x1, const T &y1, const T &z1, const T &x2, const T &y2, const T &z2) ;

  template <typename T>
  void NORM( T & x, T & y, T & z);

  template <typename T>
  void rotate(T &x, T &y, T &z, 
	      const T &vx, const T &vy, const T &vz, 
	      const T &phi, 
	      const T &ax, const T &ay, const T &az) ;

  Coords coords_set(MCNUM x, MCNUM y, MCNUM z);
  Coords coords_get(Coords a, MCNUM *x, MCNUM *y, MCNUM *z);
  Coords coords_add(Coords a, Coords b);
  Coords coords_sub(Coords a, Coords b);
  Coords coords_neg(Coords a);
  
  void rot_set_rotation(Rotation t, double phx, double phy, double phz);
  void rot_mul(Rotation t1, Rotation t2, Rotation t3);
  void rot_copy(Rotation dest, Rotation src);
  void rot_transpose(Rotation src, Rotation dst);
  Coords rot_apply(Rotation t, Coords a);



  // implementation  
  template <typename T>
  void vec_prod(T &x, T &y, T &z, 
		const T & x1, const T & y1, const T & z1, const T & x2, const T & y2, const T & z2)   
  { 
    T  mcvp_tmpx, mcvp_tmpy, mcvp_tmpz;
    mcvp_tmpx = (y1)*(z2) - (y2)*(z1);	
    mcvp_tmpy = (z1)*(x2) - (z2)*(x1); 
    mcvp_tmpz = (x1)*(y2) - (x2)*(y1); 
    (x) = mcvp_tmpx; (y) = mcvp_tmpy; (z) = mcvp_tmpz; 
  } 

  
  template <typename T>
  T scalar_prod(const T & x1, const T &y1, const T &z1, const T &x2, const T &y2, const T &z2) 
  {
    return ((x1)*(x2) + (y1)*(y2) + (z1)*(z2));
  }

  template <typename T>
  void NORM( T & x, T & y, T & z)
  {
    T mcnm_tmp = sqrt((x)*(x) + (y)*(y) + (z)*(z)); 
    if(mcnm_tmp != 0.0) 
    { 
      (x) /= mcnm_tmp; 
      (y) /= mcnm_tmp; 
      (z) /= mcnm_tmp; 
    } 
  }

  template <typename T>
  void rotate(T &x, T &y, T &z, 
	      const T &vx, const T &vy, const T &vz, 
	      const T &phi, 
	      const T &ax, const T &ay, const T &az) 
  {
    T mcrt_tmpx = (ax), mcrt_tmpy = (ay), mcrt_tmpz = (az); 
    T mcrt_vp, mcrt_vpx, mcrt_vpy, mcrt_vpz; 
    T mcrt_vnx, mcrt_vny, mcrt_vnz, mcrt_vn1x, mcrt_vn1y, mcrt_vn1z; 
    T mcrt_bx, mcrt_by, mcrt_bz; 
    T mcrt_cos, mcrt_sin; 
    NORM(mcrt_tmpx, mcrt_tmpy, mcrt_tmpz); 
    mcrt_vp = scalar_prod((vx), (vy), (vz), mcrt_tmpx, mcrt_tmpy, mcrt_tmpz); 
    mcrt_vpx = mcrt_vp*mcrt_tmpx; 
    mcrt_vpy = mcrt_vp*mcrt_tmpy; 
    mcrt_vpz = mcrt_vp*mcrt_tmpz; 
    mcrt_vnx = (vx) - mcrt_vpx; 
    mcrt_vny = (vy) - mcrt_vpy; 
    mcrt_vnz = (vz) - mcrt_vpz; 
    vec_prod(mcrt_bx, mcrt_by, mcrt_bz, 
	     mcrt_tmpx, mcrt_tmpy, mcrt_tmpz, mcrt_vnx, mcrt_vny, mcrt_vnz); 
    mcrt_cos = cos((phi)); mcrt_sin = sin((phi)); 
    mcrt_vn1x = mcrt_vnx*mcrt_cos + mcrt_bx*mcrt_sin; 
    mcrt_vn1y = mcrt_vny*mcrt_cos + mcrt_by*mcrt_sin; 
    mcrt_vn1z = mcrt_vnz*mcrt_cos + mcrt_bz*mcrt_sin; 
    (x) = mcrt_vpx + mcrt_vn1x; 
    (y) = mcrt_vpy + mcrt_vn1y; 
    (z) = mcrt_vpz + mcrt_vn1z; 
  }

} // McStas

#endif // H_McStas_compact_COORDS
