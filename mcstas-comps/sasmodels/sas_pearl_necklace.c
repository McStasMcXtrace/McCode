#define HAS_Iq
#define FORM_VOL
#line 1 "../kernel_header.c"
#define FLOAT_SIZE 8
#ifdef __OPENCL_VERSION__
# define USE_OPENCL
#elif defined(__CUDACC__)
# define USE_CUDA
#elif defined(_OPENMP)
# define USE_OPENMP
#endif

// Use SAS_DOUBLE to force the use of double even for float kernels
#define SAS_DOUBLE double

// If opencl is not available, then we are compiling a C function
// Note: if using a C++ compiler, then define kernel as extern "C"
#ifdef USE_OPENCL

   #define USE_GPU
   #define pglobal global
   #define pconstant constant

   typedef int int32_t;

   #if defined(USE_SINCOS)
   #  define SINCOS(angle,svar,cvar) svar=sincos(angle,&cvar)
   #else
   #  define SINCOS(angle,svar,cvar) do {const double _t_=angle; svar=sin(_t_);cvar=cos(_t_);} while (0)
   #endif
   // Intel CPU on Mac gives strange values for erf(); on the verified
   // platforms (intel, nvidia, amd), the cephes erf() is significantly
   // faster than that available in the native OpenCL.
   #define NEED_ERF
   // OpenCL only has type generic math
   #define expf exp
   #ifndef NEED_ERF
   #  define erff erf
   #  define erfcf erfc
   #endif

#elif defined(USE_CUDA)

   #define USE_GPU
   #define local __shared__
   #define pglobal
   #define constant __constant__
   #define pconstant const
   #define kernel extern "C" __global__

   // OpenCL powr(a,b) = C99 pow(a,b), b >= 0
   // OpenCL pown(a,b) = C99 pow(a,b), b integer
   #define powr(a,b) pow(a,b)
   #define pown(a,b) pow(a,b)
   //typedef int int32_t;
   #if defined(USE_SINCOS)
   #  define SINCOS(angle,svar,cvar) sincos(angle,&svar,&cvar)
   #else
   #  define SINCOS(angle,svar,cvar) do {const double _t_=angle; svar=sin(_t_);cvar=cos(_t_);} while (0)
   #endif

#else // !USE_OPENCL && !USE_CUDA

   #define local
   #define pglobal
   #define constant const
   #define pconstant const

   #ifdef __cplusplus
      #include <cstdio>
      #include <cmath>
      using namespace std;
      #if defined(_MSC_VER)
         #include <limits>
         #include <float.h>
         #define kernel extern "C" __declspec( dllexport )
         inline double trunc(double x) { return x>=0?floor(x):-floor(-x); }
         inline double fmin(double x, double y) { return x>y ? y : x; }
         inline double fmax(double x, double y) { return x<y ? y : x; }
         #define isnan(x) _isnan(x)
         #define isinf(x) (!_finite(x))
         #define isfinite(x) _finite(x)
         #define NAN (std::numeric_limits<double>::quiet_NaN()) // non-signalling NaN
         #define INFINITY (std::numeric_limits<double>::infinity())
         #define NEED_ERF
         #define NEED_EXPM1
         #define NEED_TGAMMA
     #else
         #define kernel extern "C"
         #include <cstdint>
     #endif
     inline void SINCOS(double angle, double &svar, double &cvar) { svar=sin(angle); cvar=cos(angle); }
   #else // !__cplusplus
     #include <inttypes.h>  // C99 guarantees that int32_t types is here
     #include <stdio.h>
     #if defined(__TINYC__)
         typedef int int32_t;
         #include <math.h>
         // TODO: check isnan is correct
         inline double _isnan(double x) { return x != x; } // hope this doesn't optimize away!
         #undef isnan
         #define isnan(x) _isnan(x)
         // Defeat the double->float conversion since we don't have tgmath
         inline SAS_DOUBLE trunc(SAS_DOUBLE x) { return x>=0?floor(x):-floor(-x); }
         inline SAS_DOUBLE fmin(SAS_DOUBLE x, SAS_DOUBLE y) { return x>y ? y : x; }
         inline SAS_DOUBLE fmax(SAS_DOUBLE x, SAS_DOUBLE y) { return x<y ? y : x; }
         #define NEED_ERF
         #define NEED_EXPM1
         #define NEED_TGAMMA
         #define NEED_CBRT
         // expf missing from windows?
         #define expf exp
     #else
         #include <tgmath.h> // C99 type-generic math, so sin(float) => sinf
     #endif
     // MSVC doesn't support C99, so no need for dllexport on C99 branch
     #define kernel
     #define SINCOS(angle,svar,cvar) do {const double _t_=angle; svar=sin(_t_);cvar=cos(_t_);} while (0)
   #endif  // !__cplusplus
   // OpenCL powr(a,b) = C99 pow(a,b), b >= 0
   // OpenCL pown(a,b) = C99 pow(a,b), b integer
   #define powr(a,b) pow(a,b)
   #define pown(a,b) pow(a,b)

#endif // !USE_OPENCL

#if defined(NEED_CBRT)
   #define cbrt(_x) pow(_x, 0.33333333333333333333333)
#endif

#if defined(NEED_EXPM1)
   // TODO: precision is a half digit lower than numpy on mac in [1e-7, 0.5]
   // Run "explore/precision.py sas_expm1" to see this (may have to fiddle
   // the xrange for log to see the complete range).
   static SAS_DOUBLE expm1(SAS_DOUBLE x_in) {
      double x = (double)x_in;  // go back to float for single precision kernels
      // Adapted from the cephes math library.
      // Copyright 1984 - 1992 by Stephen L. Moshier
      if (x != x || x == 0.0) {
         return x; // NaN and +/- 0
      } else if (x < -0.5 || x > 0.5) {
         return exp(x) - 1.0;
      } else {
         const double xsq = x*x;
         const double p = (((
            +1.2617719307481059087798E-4)*xsq
            +3.0299440770744196129956E-2)*xsq
            +9.9999999999999999991025E-1);
         const double q = ((((
            +3.0019850513866445504159E-6)*xsq
            +2.5244834034968410419224E-3)*xsq
            +2.2726554820815502876593E-1)*xsq
            +2.0000000000000000000897E0);
         double r = x * p;
         r =  r / (q - r);
         return r+r;
       }
   }
#endif

// Standard mathematical constants:
//   M_E, M_LOG2E, M_LOG10E, M_LN2, M_LN10, M_PI, M_PI_2=pi/2, M_PI_4=pi/4,
//   M_1_PI=1/pi, M_2_PI=2/pi, M_2_SQRTPI=2/sqrt(pi), SQRT2, SQRT1_2=sqrt(1/2)
// OpenCL defines M_constant_F for float constants, and nothing if double
// is not enabled on the card, which is why these constants may be missing
#ifndef M_PI
#  define M_PI 3.141592653589793
#endif
#ifndef M_PI_2
#  define M_PI_2 1.570796326794897
#endif
#ifndef M_PI_4
#  define M_PI_4 0.7853981633974483
#endif
#ifndef M_E
#  define M_E 2.718281828459045091
#endif
#ifndef M_SQRT1_2
#  define M_SQRT1_2 0.70710678118654746
#endif

// Non-standard function library
// pi/180, used for converting between degrees and radians
// 4/3 pi for computing sphere volumes
// square and cube for computing squares and cubes
#ifndef M_PI_180
#  define M_PI_180 0.017453292519943295
#endif
#ifndef M_4PI_3
#  define M_4PI_3 4.18879020478639
#endif
double square(double x) { return x*x; }
double cube(double x) { return x*x*x; }
double sas_sinx_x(double x) { return x==0 ? 1.0 : sin(x)/x; }

// CRUFT: support old style models with orientation received qx, qy and angles

// To rotate from the canonical position to theta, phi, psi, first rotate by
// psi about the major axis, oriented along z, which is a rotation in the
// detector plane xy. Next rotate by theta about the y axis, aligning the major
// axis in the xz plane. Finally, rotate by phi in the detector plane xy.
// To compute the scattering, undo these rotations in reverse order:
//     rotate in xy by -phi, rotate in xz by -theta, rotate in xy by -psi
// The returned q is the length of the q vector and (xhat, yhat, zhat) is a unit
// vector in the q direction.
// To change between counterclockwise and clockwise rotation, change the
// sign of phi and psi.

#if 1
//think cos(theta) should be sin(theta) in new coords, RKH 11Jan2017
#define ORIENT_SYMMETRIC(qx, qy, theta, phi, q, sn, cn) do { \
    SINCOS(phi*M_PI_180, sn, cn); \
    q = sqrt(qx*qx + qy*qy); \
    cn  = (q==0. ? 1.0 : (cn*qx + sn*qy)/q * sin(theta*M_PI_180));  \
    sn = sqrt(1 - cn*cn); \
    } while (0)
#else
// SasView 3.x definition of orientation
#define ORIENT_SYMMETRIC(qx, qy, theta, phi, q, sn, cn) do { \
    SINCOS(theta*M_PI_180, sn, cn); \
    q = sqrt(qx*qx + qy*qy);\
    cn = (q==0. ? 1.0 : (cn*cos(phi*M_PI_180)*qx + sn*qy)/q); \
    sn = sqrt(1 - cn*cn); \
    } while (0)
#endif

#if 1
#define ORIENT_ASYMMETRIC(qx, qy, theta, phi, psi, q, xhat, yhat, zhat) do { \
    q = sqrt(qx*qx + qy*qy); \
    const double qxhat = qx/q; \
    const double qyhat = qy/q; \
    double sin_theta, cos_theta; \
    double sin_phi, cos_phi; \
    double sin_psi, cos_psi; \
    SINCOS(theta*M_PI_180, sin_theta, cos_theta); \
    SINCOS(phi*M_PI_180, sin_phi, cos_phi); \
    SINCOS(psi*M_PI_180, sin_psi, cos_psi); \
    xhat = qxhat*(-sin_phi*sin_psi + cos_theta*cos_phi*cos_psi) \
         + qyhat*( cos_phi*sin_psi + cos_theta*sin_phi*cos_psi); \
    yhat = qxhat*(-sin_phi*cos_psi - cos_theta*cos_phi*sin_psi) \
         + qyhat*( cos_phi*cos_psi - cos_theta*sin_phi*sin_psi); \
    zhat = qxhat*(-sin_theta*cos_phi) \
         + qyhat*(-sin_theta*sin_phi); \
    } while (0)
#else
// SasView 3.x definition of orientation
#define ORIENT_ASYMMETRIC(qx, qy, theta, phi, psi, q, cos_alpha, cos_mu, cos_nu) do { \
    q = sqrt(qx*qx + qy*qy); \
    const double qxhat = qx/q; \
    const double qyhat = qy/q; \
    double sin_theta, cos_theta; \
    double sin_phi, cos_phi; \
    double sin_psi, cos_psi; \
    SINCOS(theta*M_PI_180, sin_theta, cos_theta); \
    SINCOS(phi*M_PI_180, sin_phi, cos_phi); \
    SINCOS(psi*M_PI_180, sin_psi, cos_psi); \
    cos_alpha = cos_theta*cos_phi*qxhat + sin_theta*qyhat; \
    cos_mu = (-sin_theta*cos_psi*cos_phi - sin_psi*sin_phi)*qxhat + cos_theta*cos_psi*qyhat; \
    cos_nu = (-cos_phi*sin_psi*sin_theta + sin_phi*cos_psi)*qxhat + sin_psi*cos_theta*qyhat; \
    } while (0)
#endif

//# Beginning of rotational operation definitions

typedef struct {
          double R31, R32;
      } QACRotation;

typedef struct {
    double R11, R12;
    double R21, R22;
    double R31, R32;
} QABCRotation;

// Fill in the rotation matrix R from the view angles (theta, phi) and the
// jitter angles (dtheta, dphi).  This matrix can be applied to all of the
// (qx, qy) points in the image to produce R*[qx,qy]' = [qa,qc]'
static void
qac_rotation(
    QACRotation *rotation,
    double theta, double phi,
    double dtheta, double dphi)
{
    double sin_theta, cos_theta;
    double sin_phi, cos_phi;

    // reverse view matrix
    SINCOS(theta*M_PI_180, sin_theta, cos_theta);
    SINCOS(phi*M_PI_180, sin_phi, cos_phi);
    const double V11 = cos_phi*cos_theta;
    const double V12 = sin_phi*cos_theta;
    const double V21 = -sin_phi;
    const double V22 = cos_phi;
    const double V31 = sin_theta*cos_phi;
    const double V32 = sin_phi*sin_theta;

    // reverse jitter matrix
    SINCOS(dtheta*M_PI_180, sin_theta, cos_theta);
    SINCOS(dphi*M_PI_180, sin_phi, cos_phi);
    const double J31 = sin_theta;
    const double J32 = -sin_phi*cos_theta;
    const double J33 = cos_phi*cos_theta;

    // reverse matrix
    rotation->R31 = J31*V11 + J32*V21 + J33*V31;
    rotation->R32 = J31*V12 + J32*V22 + J33*V32;
}

// Apply the rotation matrix returned from qac_rotation to the point (qx,qy),
// returning R*[qx,qy]' = [qa,qc]'
static void
qac_apply(
    QACRotation *rotation,
    double qx, double qy,
    double *qab_out, double *qc_out)
{
    // Indirect calculation of qab, from qab^2 = |q|^2 - qc^2
    const double dqc = rotation->R31*qx + rotation->R32*qy;
    const double dqab_sq = -dqc*dqc + qx*qx + qy*qy;
    //*qab_out = sqrt(fabs(dqab_sq));
    *qab_out = dqab_sq > 0.0 ? sqrt(dqab_sq) : 0.0;
    *qc_out = dqc;
}

// Fill in the rotation matrix R from the view angles (theta, phi, psi) and the
// jitter angles (dtheta, dphi, dpsi).  This matrix can be applied to all of the
// (qx, qy) points in the image to produce R*[qx,qy]' = [qa,qb,qc]'
static void
qabc_rotation(
    QABCRotation *rotation,
    double theta, double phi, double psi,
    double dtheta, double dphi, double dpsi)
{
    double sin_theta, cos_theta;
    double sin_phi, cos_phi;
    double sin_psi, cos_psi;

    // reverse view matrix
    SINCOS(theta*M_PI_180, sin_theta, cos_theta);
    SINCOS(phi*M_PI_180, sin_phi, cos_phi);
    SINCOS(psi*M_PI_180, sin_psi, cos_psi);
    const double V11 = -sin_phi*sin_psi + cos_phi*cos_psi*cos_theta;
    const double V12 = sin_phi*cos_psi*cos_theta + sin_psi*cos_phi;
    const double V21 = -sin_phi*cos_psi - sin_psi*cos_phi*cos_theta;
    const double V22 = -sin_phi*sin_psi*cos_theta + cos_phi*cos_psi;
    const double V31 = sin_theta*cos_phi;
    const double V32 = sin_phi*sin_theta;

    // reverse jitter matrix
    SINCOS(dtheta*M_PI_180, sin_theta, cos_theta);
    SINCOS(dphi*M_PI_180, sin_phi, cos_phi);
    SINCOS(dpsi*M_PI_180, sin_psi, cos_psi);
    const double J11 = cos_psi*cos_theta;
    const double J12 = sin_phi*sin_theta*cos_psi + sin_psi*cos_phi;
    const double J13 = sin_phi*sin_psi - sin_theta*cos_phi*cos_psi;
    const double J21 = -sin_psi*cos_theta;
    const double J22 = -sin_phi*sin_psi*sin_theta + cos_phi*cos_psi;
    const double J23 = sin_phi*cos_psi + sin_psi*sin_theta*cos_phi;
    const double J31 = sin_theta;
    const double J32 = -sin_phi*cos_theta;
    const double J33 = cos_phi*cos_theta;

    // reverse matrix
    rotation->R11 = J11*V11 + J12*V21 + J13*V31;
    rotation->R12 = J11*V12 + J12*V22 + J13*V32;
    rotation->R21 = J21*V11 + J22*V21 + J23*V31;
    rotation->R22 = J21*V12 + J22*V22 + J23*V32;
    rotation->R31 = J31*V11 + J32*V21 + J33*V31;
    rotation->R32 = J31*V12 + J32*V22 + J33*V32;
}

// Apply the rotation matrix returned from qabc_rotation to the point (qx,qy),
// returning R*[qx,qy]' = [qa,qb,qc]'
static void
qabc_apply(
    QABCRotation *rotation,
    double qx, double qy,
    double *qa_out, double *qb_out, double *qc_out)
{
    *qa_out = rotation->R11*qx + rotation->R12*qy;
    *qb_out = rotation->R21*qx + rotation->R22*qy;
    *qc_out = rotation->R31*qx + rotation->R32*qy;
}

// ##### End of rotation operation definitions ######
#line 1 ".././models/lib/sas_Si.c"
// integral of sin(x)/x Taylor series approximated to w/i 0.1%
double sas_Si(double x);

    
#pragma acc routine seq
double sas_Si(double x)
{
    if (x >= M_PI*6.2/4.0) {
        const double xxinv = 1./(x*x);
        // Explicitly writing factorial values triples the speed of the calculation
        const double out_cos = (((-720.*xxinv + 24.)*xxinv - 2.)*xxinv + 1.)/x;
        const double out_sin = (((-5040.*xxinv + 120.)*xxinv - 6.)*xxinv + 1)*xxinv;

        double sin_x, cos_x;
        SINCOS(x, sin_x, cos_x);
        return M_PI_2 - cos_x*out_cos - sin_x*out_sin;
    } else {
        const double xx = x*x;
        // Explicitly writing factorial values triples the speed of the calculation
        return (((((-1./439084800.*xx
            + 1./3265920.)*xx
            - 1./35280.)*xx
            + 1./600.)*xx
            - 1./18.)*xx
            + 1.)*x;
    }
}

#line 1 ".././models/lib/sas_3j1x_x.c"
/**
* Spherical Bessel function 3*j1(x)/x
*
* Used for low q to avoid cancellation error.
* Note that the values differ from sasview ~ 5e-12 rather than 5e-14, but
* in this case it is likely cancellation errors in the original expression
* using double precision that are the source.
*/
double sas_3j1x_x(double q);

// The choice of the number of terms in the series and the cutoff value for
// switching between series and direct calculation depends on the numeric
// precision.
//
// Point where direct calculation reaches machine precision:
//
//   single machine precision eps 3e-8 at qr=1.1 **
//   double machine precision eps 4e-16 at qr=1.1
//
// Point where Taylor series reaches machine precision (eps), where taylor
// series matches direct calculation (cross) and the error at that point:
//
//   prec   n eps  cross  error
//   single 3 0.28  0.4   6.2e-7
//   single 4 0.68  0.7   2.3e-7
//   single 5 1.18  1.2   7.5e-8
//   double 3 0.01  0.03  2.3e-13
//   double 4 0.06  0.1   3.1e-14
//   double 5 0.16  0.2   5.0e-15
//
// ** Note: relative error on single precision starts increase on the direct
// method at qr=1.1, rising from 3e-8 to 5e-5 by qr=1e3.  This should be
// safe for the sans range, with objects of 100 nm supported to a q of 0.1
// while maintaining 5 digits of precision.  For usans/sesans, the objects
// are larger but the q is smaller, so again it should be fine.
//
// See explore/sph_j1c.py for code to explore these ranges.

// Use 4th order series
#if FLOAT_SIZE>4
#define SPH_J1C_CUTOFF 0.1
#else
#define SPH_J1C_CUTOFF 0.7
#endif
#pragma acc routine seq
double sas_3j1x_x(double q)
{
    // 2017-05-18 PAK - support negative q
    if (fabs(q) < SPH_J1C_CUTOFF) {
        const double q2 = q*q;
        return (1.0 + q2*(-3./30. + q2*(3./840. + q2*(-3./45360.))));// + q2*(3./3991680.)))));
    } else {
        double sin_q, cos_q;
        SINCOS(q, sin_q, cos_q);
        return 3.0*(sin_q/q - cos_q)/(q*q);
    }
}

#line 1 ".././models/pearl_necklace.c"
double form_volume(double radius, double edge_sep,
    double thick_string, double fp_num_pearls);
double Iq(double q, double radius, double edge_sep,
    double thick_string, double fp_num_pearls, double sld,
    double string_sld, double solvent_sld);

// From Igor library
static double
pearl_necklace_kernel(double q, double radius, double edge_sep, double thick_string,
    int num_pearls, double sld_pearl, double sld_string, double sld_solv)
{
    // number of string segments
    const int num_strings = num_pearls - 1;

    //each masses: contrast * volume
    const double contrast_pearl = sld_pearl - sld_solv;
    const double contrast_string = sld_string - sld_solv;
    const double string_vol = edge_sep * M_PI_4 * thick_string * thick_string;
    const double pearl_vol = M_4PI_3 * radius * radius * radius;
    const double m_string = contrast_string * string_vol;
    const double m_pearl = contrast_pearl * pearl_vol;

    // center to center distance between the neighboring pearls
    const double A_s = edge_sep + 2.0 * radius;

    //sine functions of a pearl
    // Note: lim_(q->0) Si(q*a)/(q*b) = a/b
    // So therefore:
    //    beta = q==0. ? 1.0 : (Si(q*(A_s-radius)) - Si(q*radius))/q_edge;
    //    gamma = q==0. ? 1.0 : Si(q_edge)/q_edge;
    // But there is a 1/(1-sinc) term below which blows up so don't bother
    const double q_edge = q * edge_sep;
    const double beta = (sas_Si(q*(A_s-radius)) - sas_Si(q*radius)) / q_edge;
    const double gamma = sas_Si(q_edge) / q_edge;
    const double psi = sas_3j1x_x(q*radius);

    // Precomputed sinc terms
    const double si = sas_sinx_x(q*A_s);
    const double omsi = 1.0 - si;
    const double pow_si = pown(si, num_pearls);

    // form factor for num_pearls
    const double sss = 2.0*square(m_pearl*psi) * (
        - si * (1.0 - pow_si) / (omsi*omsi)
        + num_pearls / omsi
        - 0.5 * num_pearls
        );

    // form factor for num_strings (like thin rods)
    const double srr = m_string * m_string * (
        - 2.0 * (1.0 - pow_si/si)*beta*beta / (omsi*omsi)
        + 2.0 * num_strings*beta*beta / omsi
        + num_strings * (2.0*gamma - square(sas_sinx_x(q_edge/2.0)))
        );

    // form factor for correlations
    const double srs = 4.0 * m_string * m_pearl * beta * psi * (
        - si * (1.0 - pow_si/si) / (omsi*omsi)
        + num_strings / omsi
        );

    const double form = sss + srr + srs;

    return 1.0e-4 * form;
}

double form_volume(double radius, double edge_sep, double thick_string, double fp_num_pearls)
{
    const int num_pearls = (int)(fp_num_pearls + 0.5); //Force integer number of pearls
    const int num_strings = num_pearls - 1;
    const double string_vol = edge_sep * M_PI_4 * thick_string * thick_string;
    const double pearl_vol = M_4PI_3 * radius * radius * radius;
    const double volume = num_strings*string_vol + num_pearls*pearl_vol;

    return volume;
}

static double
radius_from_volume(double radius, double edge_sep, double thick_string, double fp_num_pearls)
{
    const double vol_tot = form_volume(radius, edge_sep, thick_string, fp_num_pearls);
    return cbrt(vol_tot/M_4PI_3);
}

static double
radius_effective(int mode, double radius, double edge_sep, double thick_string, double fp_num_pearls)
{
    switch (mode) {
    default:
    case 1:
        return radius_from_volume(radius, edge_sep, thick_string, fp_num_pearls);
    }
}

double Iq(double q, double radius, double edge_sep,
    double thick_string, double fp_num_pearls, double sld,
    double string_sld, double solvent_sld)
{
    const int num_pearls = (int)(fp_num_pearls + 0.5); //Force integer number of pearls
    const double form = pearl_necklace_kernel(q, radius, edge_sep,
        thick_string, num_pearls, sld, string_sld, solvent_sld);

    return form;
}

