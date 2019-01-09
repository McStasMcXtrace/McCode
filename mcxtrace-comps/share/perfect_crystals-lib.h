#ifndef BRAGG_CRYSTALS_H
#define BRAGG_CRYSTALS_H
    
#include <complex.h>

enum {Bragg_crystal_explicit=0, Bragg_crystal_diamond, Bragg_crystal_fcc,Bragg_crystal_bcc};

/* make proper function declaration to be standards-compliant */

int MxBraggDiffractionDispersion(double complex kqvals[4], double complex xi0[4], double complex xih[4],
        const double k0[3], const double nhat[3],
        const double H[3],
        double complex chi0, double complex chih_chihbar, double C, int nroots);

int MxBraggDarwinReflectivityBC(double *Rsig, double *Rpi, double kh[3],
        const double k0hat[3], const double nhat[3],
        const double alpha[3],
        double f00, double f0h, double fp, double fpp, double V, int h, int k, int l,
        double debye_waller_B, double E,
        int crystal_type, double fscaler, double fscalei);

/*This is the old Darwin function*/
void MxBraggDarwinReflectivity(double *R, double *Thetah, double *Theta0, double *DeltaTheta0,
        double f00, double f0h, double fp, double fpp, double V, double alpha, int h, int k, int l,
        double debye_waller_B, double E, double Thetain, int pol,
        int crystal_type, double fscaler, double fscalei);

void cross(double res[3], const double v1[3], const double v2[3], int unitize);

%include "read_table-lib"

#define vdot(a,b) (a[0]*b[0]+a[1]*b[1]+a[2]*b[2])
#define vplus(res, a, b) { res[0]=a[0]+b[0]; res[1]=a[1]+b[1]; res[2]=a[2]+b[2]; }
// solve a quadratic a x^2 + b x + c safely, a la Numerical Recipes.  r1 always has the larger magnitude
#define qsolve(r1, r2, a, b, c, sqrtfn) { \
    if(creal(b) > 0) { r1=(-b-sqrtfn(b*b-4*a*c))/(2*a); } \
    else { r1=(-b+sqrtfn(b*b-4*a*c))/(2*a); } \
    r2=(c/a)/r1; }

#endif /* BRAGG_CRYSTALS_H SHARE section */
