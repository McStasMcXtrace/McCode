#define HAS_Iqabc
#define HAS_FQ
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
#line 1 ".././models/lib/gauss76.c"
// Created by Andrew Jackson on 4/23/07

 #ifdef GAUSS_N
 # undef GAUSS_N
 # undef GAUSS_Z
 # undef GAUSS_W
 #endif
 #define GAUSS_N 76
 #define GAUSS_Z Gauss76Z
 #define GAUSS_W Gauss76Wt

// Gaussians
constant double Gauss76Wt[76] = {
	.00126779163408536,		//0
	.00294910295364247,
	.00462793522803742,
	.00629918049732845,
	.00795984747723973,
	.00960710541471375,
	.0112381685696677,
	.0128502838475101,
	.0144407317482767,
	.0160068299122486,
	.0175459372914742,		//10
	.0190554584671906,
	.020532847967908,
	.0219756145344162,
	.0233813253070112,
	.0247476099206597,
	.026072164497986,
	.0273527555318275,
	.028587223650054,
	.029773487255905,
	.0309095460374916,		//20
	.0319934843404216,
	.0330234743977917,
	.0339977794120564,
	.0349147564835508,
	.0357728593807139,
	.0365706411473296,
	.0373067565423816,
	.0379799643084053,
	.0385891292645067,
	.0391332242205184,		//30
	.0396113317090621,
	.0400226455325968,
	.040366472122844,
	.0406422317102947,
	.0408494593018285,
	.040987805464794,
	.0410570369162294,
	.0410570369162294,
	.040987805464794,
	.0408494593018285,		//40
	.0406422317102947,
	.040366472122844,
	.0400226455325968,
	.0396113317090621,
	.0391332242205184,
	.0385891292645067,
	.0379799643084053,
	.0373067565423816,
	.0365706411473296,
	.0357728593807139,		//50
	.0349147564835508,
	.0339977794120564,
	.0330234743977917,
	.0319934843404216,
	.0309095460374916,
	.029773487255905,
	.028587223650054,
	.0273527555318275,
	.026072164497986,
	.0247476099206597,		//60
	.0233813253070112,
	.0219756145344162,
	.020532847967908,
	.0190554584671906,
	.0175459372914742,
	.0160068299122486,
	.0144407317482767,
	.0128502838475101,
	.0112381685696677,
	.00960710541471375,		//70
	.00795984747723973,
	.00629918049732845,
	.00462793522803742,
	.00294910295364247,
	.00126779163408536		//75 (indexed from 0)
};

constant double Gauss76Z[76] = {
	-.999505948362153,		//0
	-.997397786355355,
	-.993608772723527,
	-.988144453359837,
	-.981013938975656,
	-.972229228520377,
	-.961805126758768,
	-.949759207710896,
	-.936111781934811,
	-.92088586125215,
	-.904107119545567,		//10
	-.885803849292083,
	-.866006913771982,
	-.844749694983342,
	-.822068037328975,
	-.7980001871612,
	-.77258672828181,
	-.74587051350361,
	-.717896592387704,
	-.688712135277641,
	-.658366353758143,		//20
	-.626910417672267,
	-.594397368836793,
	-.560882031601237,
	-.526420920401243,
	-.491072144462194,
	-.454895309813726,
	-.417951418780327,
	-.380302767117504,
	-.342012838966962,
	-.303146199807908,		//30
	-.263768387584994,
	-.223945802196474,
	-.183745593528914,
	-.143235548227268,
	-.102483975391227,
	-.0615595913906112,
	-.0205314039939986,
	.0205314039939986,
	.0615595913906112,
	.102483975391227,			//40
	.143235548227268,
	.183745593528914,
	.223945802196474,
	.263768387584994,
	.303146199807908,
	.342012838966962,
	.380302767117504,
	.417951418780327,
	.454895309813726,
	.491072144462194,		//50
	.526420920401243,
	.560882031601237,
	.594397368836793,
	.626910417672267,
	.658366353758143,
	.688712135277641,
	.717896592387704,
	.74587051350361,
	.77258672828181,
	.7980001871612,	//60
	.822068037328975,
	.844749694983342,
	.866006913771982,
	.885803849292083,
	.904107119545567,
	.92088586125215,
	.936111781934811,
	.949759207710896,
	.961805126758768,
	.972229228520377,		//70
	.981013938975656,
	.988144453359837,
	.993608772723527,
	.997397786355355,
	.999505948362153		//75
};


#pragma acc declare copyin(Gauss76Wt[0:76], Gauss76Z[0:76])
#line 1 ".././models/hollow_rectangular_prism.c"
static double
shell_volume(double length_a, double b2a_ratio, double c2a_ratio, double thickness)
{
    const double length_b = length_a * b2a_ratio;
    const double length_c = length_a * c2a_ratio;
    const double form_volume = length_a * length_b * length_c;
    const double a_core = length_a - 2.0*thickness;
    const double b_core = length_b - 2.0*thickness;
    const double c_core = length_c - 2.0*thickness;
    const double core_volume = a_core * b_core * c_core;
    return form_volume - core_volume;
}

static double
form_volume(double length_a, double b2a_ratio, double c2a_ratio, double thickness)
{
    const double length_b = length_a * b2a_ratio;
    const double length_c = length_a * c2a_ratio;
    const double form_volume = length_a * length_b * length_c;
    return form_volume;
}

static double
radius_from_excluded_volume(double length_a, double b2a_ratio, double c2a_ratio)
{
    const double r_equiv = sqrt(length_a*length_a*b2a_ratio/M_PI);
    const double length_c = length_a*c2a_ratio;
    return 0.5*cbrt(0.75*r_equiv*(2.0*r_equiv*length_c + (r_equiv + length_c)*(M_PI*r_equiv + length_c)));
}

static double
radius_effective(int mode, double length_a, double b2a_ratio, double c2a_ratio, double thickness)
// NOTE length_a is external dimension!
{
    switch (mode) {
    default:
    case 1: // equivalent cylinder excluded volume
        return radius_from_excluded_volume(length_a, b2a_ratio, c2a_ratio);
    case 2: // equivalent outer volume sphere
        return cbrt(cube(length_a)*b2a_ratio*c2a_ratio/M_4PI_3);
    case 3: // half length_a
        return 0.5 * length_a;
    case 4: // half length_b
        return 0.5 * length_a*b2a_ratio;
    case 5: // half length_c
        return 0.5 * length_a*c2a_ratio;
    case 6: // equivalent outer circular cross-section
        return length_a*sqrt(b2a_ratio/M_PI);
    case 7: // half ab diagonal
        return 0.5*sqrt(square(length_a) * (1.0 + square(b2a_ratio)));
    case 8: // half diagonal
        return 0.5*sqrt(square(length_a) * (1.0 + square(b2a_ratio) + square(c2a_ratio)));
    }
}

static void
Fq(double q,
    double *F1,
    double *F2,
    double sld,
    double solvent_sld,
    double length_a,
    double b2a_ratio,
    double c2a_ratio,
    double thickness)
{
    const double length_b = length_a * b2a_ratio;
    const double length_c = length_a * c2a_ratio;
    const double a_half = 0.5 * length_a;
    const double b_half = 0.5 * length_b;
    const double c_half = 0.5 * length_c;
    const double vol_total = length_a * length_b * length_c;
    const double vol_core = 8.0 * (a_half-thickness) * (b_half-thickness) * (c_half-thickness);

    //Integration limits to use in Gaussian quadrature
    const double v1a = 0.0;
    const double v1b = M_PI_2;  //theta integration limits
    const double v2a = 0.0;
    const double v2b = M_PI_2;  //phi integration limits

    double outer_sum_F1 = 0.0;
    double outer_sum_F2 = 0.0;
    for(int i=0; i<GAUSS_N; i++) {

        const double theta = 0.5 * ( GAUSS_Z[i]*(v1b-v1a) + v1a + v1b );
        double sin_theta, cos_theta;
        SINCOS(theta, sin_theta, cos_theta);

        const double termC1 = sas_sinx_x(q * c_half * cos(theta));
        const double termC2 = sas_sinx_x(q * (c_half-thickness)*cos(theta));

        double inner_sum_F1 = 0.0;
        double inner_sum_F2 = 0.0;
        for(int j=0; j<GAUSS_N; j++) {

            const double phi = 0.5 * ( GAUSS_Z[j]*(v2b-v2a) + v2a + v2b );
            double sin_phi, cos_phi;
            SINCOS(phi, sin_phi, cos_phi);

            // Amplitude AP from eqn. (13), rewritten to avoid round-off effects when arg=0

            const double termA1 = sas_sinx_x(q * a_half * sin_theta * sin_phi);
            const double termA2 = sas_sinx_x(q * (a_half-thickness) * sin_theta * sin_phi);

            const double termB1 = sas_sinx_x(q * b_half * sin_theta * cos_phi);
            const double termB2 = sas_sinx_x(q * (b_half-thickness) * sin_theta * cos_phi);

            const double AP1 = vol_total * termA1 * termB1 * termC1;
            const double AP2 = vol_core * termA2 * termB2 * termC2;

            inner_sum_F1 += GAUSS_W[j] * (AP1-AP2);
            inner_sum_F2 += GAUSS_W[j] * square(AP1-AP2);
        }
        inner_sum_F1 *= 0.5 * (v2b-v2a);
        inner_sum_F2 *= 0.5 * (v2b-v2a);

        outer_sum_F1 += GAUSS_W[i] * inner_sum_F1 * sin(theta);
        outer_sum_F2 += GAUSS_W[i] * inner_sum_F2 * sin(theta);
    }
    outer_sum_F1 *= 0.5*(v1b-v1a);
    outer_sum_F2 *= 0.5*(v1b-v1a);

    // Normalize as in Eqn. (15) without the volume factor (as cancels with (V*DelRho)^2 normalization)
    // The factor 2 is due to the different theta integration limit (pi/2 instead of pi)
    const double form_avg = outer_sum_F1/M_PI_2;
    const double form_squared_avg = outer_sum_F2/M_PI_2;

    // Multiply by contrast^2. Factor corresponding to volume^2 cancels with previous normalization.
    const double contrast = sld - solvent_sld;

    // Convert from [1e-12 A-1] to [cm-1]
    *F1 = 1.0e-2 * contrast * form_avg;
    *F2 = 1.0e-4 * contrast * contrast * form_squared_avg;
}

static double
Iqabc(double qa, double qb, double qc,
    double sld,
    double solvent_sld,
    double length_a,
    double b2a_ratio,
    double c2a_ratio,
    double thickness)
{
    const double length_b = length_a * b2a_ratio;
    const double length_c = length_a * c2a_ratio;
    const double a_half = 0.5 * length_a;
    const double b_half = 0.5 * length_b;
    const double c_half = 0.5 * length_c;
    const double vol_total = length_a * length_b * length_c;
    const double vol_core = 8.0 * (a_half-thickness) * (b_half-thickness) * (c_half-thickness);

    // Amplitude AP from eqn. (13)

    const double termA1 = sas_sinx_x(qa * a_half);
    const double termA2 = sas_sinx_x(qa * (a_half-thickness));

    const double termB1 = sas_sinx_x(qb * b_half);
    const double termB2 = sas_sinx_x(qb * (b_half-thickness));

    const double termC1 = sas_sinx_x(qc * c_half);
    const double termC2 = sas_sinx_x(qc * (c_half-thickness));

    const double AP1 = vol_total * termA1 * termB1 * termC1;
    const double AP2 = vol_core * termA2 * termB2 * termC2;

    // Multiply by contrast^2. Factor corresponding to volume^2 cancels with previous normalization.
    const double delrho = sld - solvent_sld;

    // Convert from [1e-12 A-1] to [cm-1]
    return 1.0e-4 * square(delrho * (AP1-AP2));
}


