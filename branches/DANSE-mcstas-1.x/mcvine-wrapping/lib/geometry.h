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


#ifndef H_MCSTAS2_GEOMETRY
#define H_MCSTAS2_GEOMETRY


double randnorm(void);
void normal_vec(double *nx, double *ny, double *nz, 
    double x, double y, double z);
int box_intersect(double *dt_in, double *dt_out, double x, double y, double z,
    double vx, double vy, double vz, double dx, double dy, double dz);
int cylinder_intersect(double *t0, double *t1, double x, double y, double z,
    double vx, double vy, double vz, double r, double h);
int sphere_intersect(double *t0, double *t1, double x, double y, double z,
		 double vx, double vy, double vz, double r);
/* ADD: E. Farhi, Aug 6th, 2001 plane_intersect_Gfast */   
int plane_intersect_Gfast(double *Idt, 
    double A,  double B,  double C);
void randvec_target_circle(double *xo, double *yo, double *zo, 
    double *solid_angle, double xi, double yi, double zi, double radius);
#define randvec_target_sphere randvec_target_circle



typedef double MCNUM;
typedef struct {MCNUM x, y, z;} Coords;
typedef MCNUM Rotation[3][3];

extern MCNUM norotation[][3];
#define ROT_A_CURRENT_COMP  norotation


void randvec_target_rect_angular
(double *xo, double *yo, double *zo, 
 double *solid_angle,
 double xi, double yi, double zi, double height, double width, Rotation A);        
void randvec_target_rect
(double *xo, double *yo, double *zo, 
 double *solid_angle,
 double xi, double yi, double zi, double height, double width, Rotation A); 


Coords coords_set(MCNUM x, MCNUM y, MCNUM z);
Coords coords_get(Coords a, MCNUM *x, MCNUM *y, MCNUM *z);
Coords coords_add(Coords a, Coords b);
Coords coords_sub(Coords a, Coords b);
Coords coords_neg(Coords a);
Coords coords_scale(Coords b, double scale);
double coords_sp(Coords a, Coords b);
Coords coords_xp(Coords b, Coords c);
void   coords_print(Coords a);

void rot_set_rotation(Rotation t, double phx, double phy, double phz);
int  rot_test_identity(Rotation t);
void rot_mul(Rotation t1, Rotation t2, Rotation t3);
void rot_copy(Rotation dest, Rotation src);
void rot_transpose(Rotation src, Rotation dst);
Coords rot_apply(Rotation t, Coords a);
void mccoordschange(Coords a, Rotation t, double *x, double *y, double *z,
    double *vx, double *vy, double *vz, double *time,
    double *s1, double *s2);
void mccoordschange_polarisation(Rotation t,
    double *sx, double *sy, double *sz);


#endif //H_MCSTAS2_GEOMETRY



// version
// $Id$

// End of file 
