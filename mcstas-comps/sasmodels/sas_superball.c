#define HAS_Iqabc
#define HAS_FQ
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

#line 1 ".././models/lib/gauss20.c"

// Created by Andrew Jackson on 4/23/07

 #ifdef GAUSS_N
 # undef GAUSS_N
 # undef GAUSS_Z
 # undef GAUSS_W
 #endif
 #define GAUSS_N 20
 #define GAUSS_Z Gauss20Z
 #define GAUSS_W Gauss20Wt

// Gaussians
constant double Gauss20Wt[20]={
	.0176140071391521,
	.0406014298003869,
	.0626720483341091,
	.0832767415767047,
	.10193011981724,
	.118194531961518,
	.131688638449177,
	.142096109318382,
	.149172986472604,
	.152753387130726,
	.152753387130726,
	.149172986472604,
	.142096109318382,
	.131688638449177,
	.118194531961518,
	.10193011981724,
	.0832767415767047,
	.0626720483341091,
	.0406014298003869,
	.0176140071391521
};

constant double Gauss20Z[20]={
	-.993128599185095,
	-.963971927277914,
	-.912234428251326,
	-.839116971822219,
	-.746331906460151,
	-.636053680726515,
	-.510867001950827,
	-.37370608871542,
	-.227785851141645,
	-.076526521133497,
	.0765265211334973,
	.227785851141645,
	.37370608871542,
	.510867001950827,
	.636053680726515,
	.746331906460151,
	.839116971822219,
	.912234428251326,
	.963971927277914,
	.993128599185095
};

#pragma acc declare copyin( Gauss20Wt[0:20], Gauss20Z[0:20] )

#line 1 ".././models/lib/sas_gamma.c"

/*
The wrapper for gamma function from OpenCL and standard libraries
The OpenCL gamma function fails miserably on values lower than 1.0
while works fine on larger values.
We use gamma definition Gamma(t + 1) = t * Gamma(t) to compute
to function for values lower than 1.0. Namely Gamma(t) = 1/t * Gamma(t + 1)
For t < 0, we use Gamma(t) = pi / ( Gamma(1 - t) * sin(pi * t) )
*/

#if defined(NEED_TGAMMA)
#pragma acc routine seq
static double cephes_stirf(double x)
{
	const double MAXSTIR=143.01608;
	const double SQTPI=2.50662827463100050242E0;
	double y, w, v;

	w = 1.0 / x;

	w = ((((
		7.87311395793093628397E-4*w
		- 2.29549961613378126380E-4)*w
		- 2.68132617805781232825E-3)*w
		+ 3.47222221605458667310E-3)*w
		+ 8.33333333333482257126E-2)*w
		+ 1.0;
	y = exp(x);
	if (x > MAXSTIR)
	{ /* Avoid overflow in pow() */
		v = pow(x, 0.5 * x - 0.25);
		y = v * (v / y);
	}
	else
	{
		y = pow(x, x - 0.5) / y;
	}
	y = SQTPI * y * w;
	return(y);
}

#pragma acc routine seq
static double tgamma(double x) {
	double p, q, z;
	int sgngam;
	int i;

	sgngam = 1;
	if (isnan(x))
		return(x);
	q = fabs(x);

	if (q > 33.0)
	{
		if (x < 0.0)
		{
			p = floor(q);
			if (p == q)
			{
				return (NAN);
			}
			i = p;
			if ((i & 1) == 0)
				sgngam = -1;
			z = q - p;
			if (z > 0.5)
			{
				p += 1.0;
				z = q - p;
			}
			z = q * sin(M_PI * z);
			if (z == 0.0)
			{
				return(NAN);
			}
			z = fabs(z);
			z = M_PI / (z * cephes_stirf(q));
		}
		else
		{
			z = cephes_stirf(x);
		}
		return(sgngam * z);
	}

	z = 1.0;
	while (x >= 3.0)
	{
		x -= 1.0;
		z *= x;
	}

	while (x < 0.0)
	{
		if (x > -1.E-9)
			goto small;
		z /= x;
		x += 1.0;
	}

	while (x < 2.0)
	{
		if (x < 1.e-9)
			goto small;
		z /= x;
		x += 1.0;
	}

	if (x == 2.0)
		return(z);

	x -= 2.0;
	p = (((((
		+1.60119522476751861407E-4*x
		+ 1.19135147006586384913E-3)*x
		+ 1.04213797561761569935E-2)*x
		+ 4.76367800457137231464E-2)*x
		+ 2.07448227648435975150E-1)*x
		+ 4.94214826801497100753E-1)*x
		+ 9.99999999999999996796E-1;
	q = ((((((
		-2.31581873324120129819E-5*x
		+ 5.39605580493303397842E-4)*x
		- 4.45641913851797240494E-3)*x
		+ 1.18139785222060435552E-2)*x
		+ 3.58236398605498653373E-2)*x
		- 2.34591795718243348568E-1)*x
		+ 7.14304917030273074085E-2)*x
		+ 1.00000000000000000320E0;
	return(z * p / q);

small:
	if (x == 0.0)
	{
		return (NAN);
	}
	else
		return(z / ((1.0 + 0.5772156649015329 * x) * x));
}
#endif // NEED_TGAMMA

#pragma acc routine seq
inline double sas_gamma(double x)
{
    // Note: the builtin tgamma can give slow and unreliable results for x<1.
    // The following transform extends it to zero and to negative values.
    // It should return NaN for zero and negative integers but doesn't.
    // The accuracy is okay but not wonderful for negative numbers, maybe
    // one or two digits lost in the calculation. If higher accuracy is
    // needed, you could test the following loop:
    //    double norm = 1.;
    //    while (x<1.) { norm*=x; x+=1.; }
    //    return tgamma(x)/norm;
    return (x<0. ? M_PI/tgamma(1.-x)/sin(M_PI*x) : tgamma(x+1)/x);
}


#line 1 ".././models/superball.c"

static double
form_volume(double length_a, double exponent_p)
{
  double g1 = sas_gamma(1.0 / (2.0 * exponent_p));
  double g3 = sas_gamma(3.0 / (2.0 * exponent_p));
  return cube(length_a) / 12.0 / square(exponent_p) * cube(g1) / g3;
}

static double
radius_from_excluded_volume(double length_a, double exponent_p)
{
  double g1 = sas_gamma(1.0 / (2.0 * exponent_p));
  double g3 = sas_gamma(3.0 / (2.0 * exponent_p));
  double g5 = sas_gamma(5.0 / (2.0 * exponent_p));

  return length_a * g3 * sqrt(3.0 / 10.0 / g1 / g5);
}

static double

radius_effective(int mode, double length_a, double exponent_p)

{
  switch (mode)
  {
  default:
  case 1: // radius of gyration
    return radius_from_excluded_volume(length_a, exponent_p);
  case 2: // equivalent volume sphere
    return cbrt(form_volume(length_a, exponent_p) / M_4PI_3);
  case 3: // half length_a
    return 0.5 * length_a;
  }
}

static double oriented_superball(
    double qx,
    double qy,
    double qz,
    double length_a,
    double exponent_p)
{
  // oriented superball form factor

  // outer integral for x
  const double radius = length_a / 2.0; // superball radius
  const double inverse_2p = 1.0 / (2.0 * exponent_p);

  double outer_integral = 0.0; //initialize integral

  for (int i_x = 0; i_x < GAUSS_N; i_x++)
  {
    const double x = 0.5 * (GAUSS_Z[i_x] + 1.0); // integrate 0, 1
    const double x2p = pow(x, 2.0 * exponent_p);
    const double gamma = pow(1.0 - x2p, inverse_2p);

    // inner integral for y
    double inner_integral = 0.0; //initialize integral
    for (int i_y = 0; i_y < GAUSS_N; i_y++)
    {
      const double y = 0.5 * gamma * (GAUSS_Z[i_y] + 1.0); // integrate 0, gamma
      const double y2p = pow(y, 2.0 * exponent_p);
      const double zeta = pow(1.0 - x2p - y2p, inverse_2p);
      const double cos1 = cos(radius * qy * y);
      const double sinc2 = qz == 0 ? radius * zeta : sin(radius * qz * zeta) / qz;
      const double fq = cos1 * sinc2;
      inner_integral += GAUSS_W[i_y] * fq;
    }

    const double co = cos(radius * qx * x);

    // integration factor for -1,1 quadrature to 0, gamma: gamma/2
    const double integration_factor = 0.5 * gamma;

    // Eq. 21 in [Dresen2021]
    outer_integral += GAUSS_W[i_x] * integration_factor * inner_integral * co * 2.0 * square(length_a);

  }
// Needed to normalise the oriented form factor, but would be reverted later with s = SLD contrast * volume
// outer_integral /= form_volume(length_a, exponent_p); 

  // integration factor for -1,1 quadrature to 0, 1: 1/2
  return 0.5 * outer_integral;
}

static void
Fq(double q,
   double *F1,
   double *F2,
   double sld,
   double solvent_sld,
   double length_a,
   double exponent_p)
{

  // translate a point in [-1,1] to a point in [0, pi/2]
  const double zm = M_PI_4;
  const double zb = M_PI_4;

  double orient_averaged_outer_total_F1 = 0.0; //initialize integral
  double orient_averaged_outer_total_F2 = 0.0; //initialize integral
  // phi integral
  for (int i_phi = 0; i_phi < GAUSS_N; i_phi++)
  {

    const double phi = GAUSS_Z[i_phi]*zm +zb; // integrate 0 .. pi/2

    double sin_phi, cos_phi;
    SINCOS(phi, sin_phi, cos_phi);

    double orient_averaged_inner_total_F1 = 0.0; //initialize integral
    double orient_averaged_inner_total_F2 = 0.0; //initialize integral
    // theta integral
    for (int i_theta = 0; i_theta < GAUSS_N; i_theta++)
    {

      const double cos_theta = GAUSS_Z[i_theta]*0.5 + 0.5; // integrate 0, 1
      const double sin_theta = sqrt( 1.0 - square(cos_theta) );


      const double qx = q * cos_phi * sin_theta;
      const double qy = q * sin_phi * sin_theta;
      const double qz = q * cos_theta;

      const double f_oriented = oriented_superball(qx, qy, qz, length_a, exponent_p);


      orient_averaged_inner_total_F1 += GAUSS_W[i_theta] * f_oriented;
      orient_averaged_inner_total_F2 += GAUSS_W[i_theta] * square(f_oriented);

    }
    orient_averaged_outer_total_F1 += GAUSS_W[i_phi] * orient_averaged_inner_total_F1;
    orient_averaged_outer_total_F2 += GAUSS_W[i_phi] * orient_averaged_inner_total_F2;
  }


  // integration factors for phi and theta integral, divided by solid angle of pi/2
  orient_averaged_outer_total_F1 *= 0.25;
  orient_averaged_outer_total_F2 *= 0.25;
  // Multiply by contrast^2 and convert from [1e-12 A-1] to [cm-1]
  const double s =  (sld - solvent_sld) ;

  *F1 = 1.0e-2 * s * orient_averaged_outer_total_F1;
  *F2 = 1.0e-4 * s * s * orient_averaged_outer_total_F2;
}

static double
Iqabc(double qa, double qb, double qc,
      double sld,
      double solvent_sld,
      double length_a,
      double exponent_p)
{
  const double f_oriented = oriented_superball(qa, qb, qc, length_a, exponent_p);

  const double s = (sld - solvent_sld); 


  const double form = square(s * f_oriented);
  // Square and convert from [1e-12 A-1] to [cm-1]
  return 1.0e-4 * form;
}


