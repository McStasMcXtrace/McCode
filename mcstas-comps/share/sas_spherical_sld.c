#define HAS_Iq
#define FORM_VOL
#line 1 "../kernel_header.c"

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

#line 1 ".././models/lib/polevl.c"

/*							polevl.c
 *							p1evl.c
 *
 *	Evaluate polynomial
 *
 *
 *
 * SYNOPSIS:
 *
 * int N;
 * double x, y, coef[N+1], polevl[];
 *
 * y = polevl( x, coef, N );
 *
 *
 *
 * DESCRIPTION:
 *
 * Evaluates polynomial of degree N:
 *
 *                     2          N
 * y  =  C  + C x + C x  +...+ C x
 *        0    1     2          N
 *
 * Coefficients are stored in reverse order:
 *
 * coef[0] = C  , ..., coef[N] = C  .
 *            N                   0
 *
 * The function p1evl() assumes that C_N = 1.0 and is
 * omitted from the array.  Its calling arguments are
 * otherwise the same as polevl().
 *
 *
 * SPEED:
 *
 * In the interest of speed, there are no checks for out
 * of bounds arithmetic.  This routine is used by most of
 * the functions in the library.  Depending on available
 * equipment features, the user may wish to rewrite the
 * program in microcode or assembly language.
 *
 */


/*
Cephes Math Library Release 2.1:  December, 1988
Copyright 1984, 1987, 1988 by Stephen L. Moshier
Direct inquiries to 30 Frost Street, Cambridge, MA 02140
*/
#pragma acc routine seq
static
double polevl( double x, pconstant double *coef, int N )
{

    int i = 0;
    double ans = coef[i];

    while (i < N) {
        i++;
        ans = ans * x + coef[i];
    }

    return ans;
}

/*							p1evl()	*/
/*                                          N
 * Evaluate polynomial when coefficient of x  is 1.0.
 * Otherwise same as polevl.
 */
#pragma acc routine seq
static
double p1evl( double x, pconstant double *coef, int N )
{
    int i=0;
    double ans = x+coef[i];

    while (i < N-1) {
        i++;
        ans = ans*x + coef[i];
    }

    return ans;
}


#line 1 ".././models/lib/sas_erf.c"


/*
 * Cephes Math Library Release 2.2:  June, 1992
 * Copyright 1984, 1987, 1988, 1992 by Stephen L. Moshier
 * Direct inquiries to 30 Frost Street, Cambridge, MA 02140
 */
/*
 *
 *	Error function
 *
 *
 *
 * SYNOPSIS:
 *
 * double x, y, erf();
 *
 * y = erf( x );
 *
 *
 *
 * DESCRIPTION:
 *
 * The integral is
 *
 *                           x
 *                            -
 *                 2         | |          2
 *   erf(x)  =  --------     |    exp( - t  ) dt.
 *              sqrt(pi)   | |
 *                          -
 *                           0
 *
 * For 0 <= |x| < 1, erf(x) = x * P4(x**2)/Q5(x**2); otherwise
 * erf(x) = 1 - erfc(x).
 *
 *
 *
 * ACCURACY:
 *
 *                      Relative error:
 * arithmetic   domain     # trials      peak         rms
 *    IEEE      0,1         30000       3.7e-16     1.0e-16
 *
 */
/*
 *
 *	Complementary error function
 *
 *
 *
 * SYNOPSIS:
 *
 * double x, y, erfc();
 *
 * y = erfc( x );
 *
 *
 *
 * DESCRIPTION:
 *
 *
 *  1 - erf(x) =
 *
 *                           inf.
 *                             -
 *                  2         | |          2
 *   erfc(x)  =  --------     |    exp( - t  ) dt
 *               sqrt(pi)   | |
 *                           -
 *                            x
 *
 *
 * For small x, erfc(x) = 1 - erf(x); otherwise rational
 * approximations are computed.
 *
 *
 *
 * ACCURACY:
 *
 *                      Relative error:
 * arithmetic   domain     # trials      peak         rms
 *    IEEE      0,26.6417   30000       5.7e-14     1.5e-14
 */

#ifdef NEED_ERF

#if FLOAT_SIZE>4  // DOUBLE_PRECISION

double cephes_erf(double x);
double cephes_erfc(double a);

constant double PD[] = {
    2.46196981473530512524E-10,
    5.64189564831068821977E-1,
    7.46321056442269912687E0,
    4.86371970985681366614E1,
    1.96520832956077098242E2,
    5.26445194995477358631E2,
    9.34528527171957607540E2,
    1.02755188689515710272E3,
    5.57535335369399327526E2
};

constant double QD[] = {
    /* 1.00000000000000000000E0, */
    1.32281951154744992508E1,
    8.67072140885989742329E1,
    3.54937778887819891062E2,
    9.75708501743205489753E2,
    1.82390916687909736289E3,
    2.24633760818710981792E3,
    1.65666309194161350182E3,
    5.57535340817727675546E2
};

constant double RD[] = {
    5.64189583547755073984E-1,
    1.27536670759978104416E0,
    5.01905042251180477414E0,
    6.16021097993053585195E0,
    7.40974269950448939160E0,
    2.97886665372100240670E0
};

constant double SD[] = {
    /* 1.00000000000000000000E0, */
    2.26052863220117276590E0,
    9.39603524938001434673E0,
    1.20489539808096656605E1,
    1.70814450747565897222E1,
    9.60896809063285878198E0,
    3.36907645100081516050E0
};

constant double TD[] = {
    9.60497373987051638749E0,
    9.00260197203842689217E1,
    2.23200534594684319226E3,
    7.00332514112805075473E3,
    5.55923013010394962768E4
};

constant double UD[] = {
    /* 1.00000000000000000000E0, */
    3.35617141647503099647E1,
    5.21357949780152679795E2,
    4.59432382970980127987E3,
    2.26290000613890934246E4,
    4.92673942608635921086E4
};

#pragma acc declare copyin(PD[0:1], QD[0:1], RD[0:1], SD[0:1], TD[0:1], UD[0:1])

#pragma acc routine seq
double cephes_erfc(double a)
{
    double MAXLOG = 88.72283905206835;
    double p, q, x, y, z;


    x = fabs(a);

    if (x < 1.0) {
        // The line below causes problems on the GPU, so inline
        // the erf function instead and z < 1.0.
        //return (1.0 - cephes_erf(a));
        // 2017-05-18 PAK - use erf(a) rather than erf(|a|)
        z = a * a;
        y = a * polevl(z, TD, 4) / p1evl(z, UD, 5);

        return 1.0 - y;
    }

    z = -a * a;

    if (z < -MAXLOG) {
        if (a < 0)
            return (2.0);
        else
            return (0.0);
    }

    z = exp(z);


    if (x < 8.0) {
        p = polevl(x, PD, 8);
        q = p1evl(x, QD, 8);
    }
    else {
        p = polevl(x, RD, 5);
        q = p1evl(x, SD, 6);
    }
    y = (z * p) / q;

    if (a < 0)
        y = 2.0 - y;

    if (y == 0.0) {
        if (a < 0)
            return (2.0);
        else
            return (0.0);
    }
    return y;
}


double cephes_erf(double x)
{
    double y, z;

    if (fabs(x) > 1.0)
        return (1.0 - cephes_erfc(x));

    z = x * x;
    y = x * polevl(z, TD, 4) / p1evl(z, UD, 5);

    return y;
}

#else // SINGLE PRECISION

float cephes_erff(float x);
float cephes_erfcf(float a);

/* erfc(x) = exp(-x^2) P(1/x), 1 < x < 2 */
constant float PF[] = {
    2.326819970068386E-002,
    -1.387039388740657E-001,
    3.687424674597105E-001,
    -5.824733027278666E-001,
    6.210004621745983E-001,
    -4.944515323274145E-001,
    3.404879937665872E-001,
    -2.741127028184656E-001,
    5.638259427386472E-001
};

/* erfc(x) = exp(-x^2) 1/x P(1/x^2), 2 < x < 14 */
constant float RF[] = {
    -1.047766399936249E+001,
    1.297719955372516E+001,
    -7.495518717768503E+000,
    2.921019019210786E+000,
    -1.015265279202700E+000,
    4.218463358204948E-001,
    -2.820767439740514E-001,
    5.641895067754075E-001
};

/* erf(x) = x P(x^2), 0 < x < 1 */
 constant float TF[] = {
    7.853861353153693E-005,
    -8.010193625184903E-004,
    5.188327685732524E-003,
    -2.685381193529856E-002,
    1.128358514861418E-001,
    -3.761262582423300E-001,
    1.128379165726710E+000
};

#pragma acc declare copyin(PF[0:1], RF[0:1], TF[0:1])

#pragma acc routine seq
float cephes_erfcf(float a)
{
    float MAXLOG = 88.72283905206835;
    float p, q, x, y, z;


    /*if (a < 0.0)
        x = -a;
    else
        x = a;*/
    // TODO: tinycc does not support fabsf
    x = fabs(a);


    if (x < 1.0) {
        //The line below is a troublemaker for GPU, so sas_erf function
        //is explicit here for the case < 1.0
        //return (1.0 - sas_erf(a));
        // 2017-05-18 PAK - use erf(a) rather than erf(|a|)
        z = a * a;
        y = a * polevl( z, TF, 6 );

        return 1.0 - y;
    }

    z = -a * a;

    if (z < -MAXLOG) {
        if (a < 0)
            return (2.0);
        else
            return (0.0);
    }
    z = expf(z);


    q=1.0/x;
    y=q*q;
    if( x < 2.0 ) {
        p = polevl( y, PF, 8 );
    } else {
        p = polevl( y, RF, 7 );
    }
    y = z * q * p;

    if (a < 0)
        y = 2.0 - y;

    if (y == 0.0) {
        if (a < 0)
            return (2.0);
        else
            return (0.0);
    }
    return y;
}

#pragma acc routine seq
float cephes_erff(float x)
{
    float y, z;

    // TODO: tinycc does not support fabsf
    if (fabs(x) > 1.0)
        return (1.0 - cephes_erfcf(x));

    z = x * x;
    y = x * polevl( z, TF, 6 );

    return y;
}

#endif // SINGLE_PRECISION

#if FLOAT_SIZE>4
//static double sas_erf(double x) { return erf(x); }
//static double sas_erfc(double x) { return erfc(x); }
#define sas_erf cephes_erf
#define sas_erfc cephes_erfc
#else
#define sas_erf cephes_erff
#define sas_erfc cephes_erfcf
#endif

#else // !NEED_ERF

#if FLOAT_SIZE>4
//static double sas_erf(double x) { return erf(x); }
//static double sas_erfc(double x) { return erfc(x); }
#define sas_erf erf
#define sas_erfc erfc
#else
#define sas_erf erff
#define sas_erfc erfcf
#endif
#endif // !NEED_ERF


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


#line 1 ".././models/spherical_sld.c"

static double
outer_radius(double fp_n_shells, double thickness[], double interface[])
{
    int n_shells= (int)(fp_n_shells + 0.5);
    double r = 0.0;
    for (int i=0; i < n_shells; i++) {
        r += thickness[i] + interface[i];
    }
    return r;
}

static double form_volume(
    double fp_n_shells,
    double thickness[],
    double interface[])
{
    return M_4PI_3*cube(outer_radius(fp_n_shells, thickness, interface));
}

static double
radius_effective(int mode, double fp_n_shells, double thickness[], double interface[])
{
    // case 1: outer radius
    return outer_radius(fp_n_shells, thickness, interface);
}

static double blend(int shape, double nu, double z)
{
    if (shape==0) {
        const double num = sas_erf(nu * M_SQRT1_2 * (2.0*z - 1.0));
        const double denom = 2.0 * sas_erf(nu * M_SQRT1_2);
        return num/denom + 0.5;
    } else if (shape==1) {
        return pow(z, nu);
    } else if (shape==2) {
        return 1.0 - pow(1.0 - z, nu);
    } else if (shape==3) {
        return expm1(-nu*z)/expm1(-nu);
    } else if (shape==4) {
        return expm1(nu*z)/expm1(nu);
    } else if (shape==5) {
        return 1.0 - pow(1.0 - z*z, (0.5*nu-2.0));        
    } else {
        return NAN;
    }
}

static double f_linear(double q, double r, double contrast, double slope)
{
    const double qr = q * r;
    const double qrsq = qr * qr;
    const double bes = sas_3j1x_x(qr);
    double sinqr, cosqr;
    SINCOS(qr, sinqr, cosqr);
    const double fun = 3.0*r*(2.0*qr*sinqr - (qrsq-2.0)*cosqr)/(qrsq*qrsq);
    const double vol = M_4PI_3 * cube(r);
    return vol*(bes*contrast + fun*slope);
}

static double Iq(
    double q,
    double fp_n_shells,
    double sld_solvent,
    double sld[],
    double thickness[],
    double interface[],
    double shape[],
    double nu[],
    double fp_n_steps)
{
    // iteration for # of shells + core + solvent
    int n_shells = (int)(fp_n_shells + 0.5);
    int n_steps = (int)(fp_n_steps + 0.5);
    double f=0.0;
    double r=0.0;
    for (int shell=0; shell<n_shells; shell++){
        const double sld_l = sld[shell];

        // uniform shell; r=0 => r^3=0 => f=0, so works for core as well.
        f -= M_4PI_3 * cube(r) * sld_l * sas_3j1x_x(q*r);
        r += thickness[shell];
        f += M_4PI_3 * cube(r) * sld_l * sas_3j1x_x(q*r);

        // iterate over sub_shells in the interface
        const double dr = interface[shell]/n_steps;
        const double delta = (shell==n_shells-1 ? sld_solvent : sld[shell+1]) - sld_l;
        const double nu_shell = fmax(fabs(nu[shell]), 1.e-14);
        const int shape_shell = (int)(shape[shell]);

        // if there is no interface the equations don't work
        if (dr == 0.) continue;

        double sld_in = sld_l;
        for (int step=1; step <= n_steps; step++) {
            // find sld_i at the outer boundary of sub-shell step
            //const double z = (double)step/(double)n_steps;
            const double z = (double)step/(double)n_steps;
            const double fraction = blend(shape_shell, nu_shell, z);
            const double sld_out = fraction*delta + sld_l;
            // calculate slope
            const double slope = (sld_out - sld_in)/dr;
            const double contrast = sld_in - slope*r;

            // iteration for the left and right boundary of the shells
            f -= f_linear(q, r, contrast, slope);
            r += dr;
            f += f_linear(q, r, contrast, slope);
            sld_in = sld_out;
        }
    }
    // add in solvent effect
    f -= M_4PI_3 * cube(r) * sld_solvent * sas_3j1x_x(q*r);

    const double f2 = f * f * 1.0e-4;
    return f2;
}

static void Fq(
    double q,
    double *F1,
    double *F2,
    double fp_n_shells,
    double sld_solvent,
    double sld[],
    double thickness[],
    double interface[],
    double shape[],
    double nu[],
    double fp_n_steps)
{
    // iteration for # of shells + core + solvent
    int n_shells = (int)(fp_n_shells + 0.5);
    int n_steps = (int)(fp_n_steps + 0.5);
    double f=0.0;
    double r=0.0;
    for (int shell=0; shell<n_shells; shell++){
        const double sld_l = sld[shell];

        // uniform shell; r=0 => r^3=0 => f=0, so works for core as well.
        f -= M_4PI_3 * cube(r) * sld_l * sas_3j1x_x(q*r);
        r += thickness[shell];
        f += M_4PI_3 * cube(r) * sld_l * sas_3j1x_x(q*r);

        // iterate over sub_shells in the interface
        const double dr = interface[shell]/n_steps;
        const double delta = (shell==n_shells-1 ? sld_solvent : sld[shell+1]) - sld_l;
        const double nu_shell = fmax(fabs(nu[shell]), 1.e-14);
        const int shape_shell = (int)(shape[shell]);

        // if there is no interface the equations don't work
        if (dr == 0.) continue;

        double sld_in = sld_l;
        for (int step=1; step <= n_steps; step++) {
            // find sld_i at the outer boundary of sub-shell step
            //const double z = (double)step/(double)n_steps;
            const double z = (double)step/(double)n_steps;
            const double fraction = blend(shape_shell, nu_shell, z);
            const double sld_out = fraction*delta + sld_l;
            // calculate slope
            const double slope = (sld_out - sld_in)/dr;
            const double contrast = sld_in - slope*r;

            // iteration for the left and right boundary of the shells
            f -= f_linear(q, r, contrast, slope);
            r += dr;
            f += f_linear(q, r, contrast, slope);
            sld_in = sld_out;
        }
    }
    // add in solvent effect
    f -= M_4PI_3 * cube(r) * sld_solvent * sas_3j1x_x(q*r);

    *F1 = 1e-2*f;
    *F2 = 1e-4*f*f;
}


