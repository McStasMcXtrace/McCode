#ifndef MX_CRYSTALS_H
#define MX_CRYSTALS_H

#include <complex.h>

enum {Mx_crystal_explicit=0, Mx_crystal_diamond, Mx_crystal_fcc,Mx_crystal_bcc};

/* make proper function declaration to be standards-compliant */
void Mx_CubicCrystalChi(double complex *chi0, double complex *chih, double *k0mag, double *hscale, double *thetaB,
                         double f00, double f0h, double fp, double fpp, double V, int h, int k, int l,
                         double debye_waller_B, double E,
                         int crystal_type, double fscaler, double fscalei);

int Mx_DiffractionDispersion(double complex kqvals[4], double complex xi0[4], double complex xih[4],
        const double k0[3], const double nhat[3],
        const double H[3],
        double complex chi0, double complex chih_chihbar, double C, int nroots);

int Mx_DarwinReflectivityBC(double *Rsig, double *Rpi, double kh[3],
	const double k0hat[3], const double nhat[3],
	const double alpha[3],
	double complex chi0, double complex chih, double complex chihbar,
	double k0mag, double hscale, double thetaB);

int Mx_LaueReflectivityBC(double *Rsig, double *Rpi, double *Tsig, double *Tpi,
	double *Asig, double *Api, // primary attenuation
	double kh[3],
	const double k0hat[3], const double nhat[3],
	const double alpha[3],
	double complex chi0, double complex chih, double complex chihbar,
	double k0mag, double hscale, double thetaB, double thickness);

/*This is the old Darwin function*/
void Mx_DarwinReflectivity(double *R, double *Thetah, double *Theta0, double *DeltaTheta0,
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

#endif /* MX_CRYSTALS_H SHARE section */
