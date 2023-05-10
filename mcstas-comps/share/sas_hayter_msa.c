#define HAS_Iq
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

#line 1 ".././models/hayter_msa.c"

// Hayter-Penfold (rescaled) MSA structure factor for screened Coulomb interactions 
//
// C99 needs declarations of routines here
double Iq(double QQ,
      double radius_effective, double zz, double VolFrac, double Temp, double csalt, double dialec);
int
sqcoef(int ir, double gMSAWave[]);

int
sqfun(int ix, int ir, double gMSAWave[]);

double
sqhcal(double qq, double gMSAWave[]);
  
double Iq(double QQ,
      double radius_effective, double VolFrac, double zz, double Temp, double csalt, double dialec)
{
    double gMSAWave[17]={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17};
	double Elcharge=1.602189e-19;		// electron charge in Coulombs (C)
	double kB=1.380662e-23;				// Boltzman constant in J/K
	double FrSpPerm=8.85418782E-12;	//Permittivity of free space in C^2/(N m^2)
	double SofQ, Qdiam, Vp, ss;
	double SIdiam, diam, Kappa, cs, IonSt;
	double  Perm, Beta;
	double charge;
	int ierr;
	
	diam=2*radius_effective;		//in A

						////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
						//////////////////////////// convert to USEFUL inputs in SI units                                                //
						////////////////////////////    NOTE: easiest to do EVERYTHING in SI units                               //
						////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	Beta=1.0/(kB*Temp);		// in Joules^-1
	Perm=dialec*FrSpPerm;	//in C^2/(N  m^2)
	charge=zz*Elcharge;		//in Coulomb (C)
	SIdiam = diam*1.0E-10;		//in m
	Vp=M_4PI_3*cube(SIdiam/2.0);	//in m^3
	cs=csalt*6.022E23*1.0E3;	//# salt molecules/m^3
	
	//         Compute the derived values of :
	//			 Ionic strength IonSt (in C^2/m^3)  
	// 			Kappa (Debye-Huckel screening length in m)
	//	and		gamma Exp(-k)
	
	// the zz*VolFrac/Vp is for the counterions from the micelle, assumed monovalent, the 2.0*cs if for added salt, assumed 1:1 electolyte 
	IonSt=0.5 * Elcharge*Elcharge*(zz*VolFrac/Vp+2.0*cs);
	Kappa=sqrt(2*Beta*IonSt/Perm);     //Kappa calc from Ionic strength
									   //	Kappa=2/SIdiam					// Use to compare with HP paper
	gMSAWave[5]=Beta*charge*charge/(M_PI*Perm*SIdiam*square(2.0+Kappa*SIdiam));
	
	//         Finally set up dimensionless parameters 
	Qdiam=QQ*diam;
	gMSAWave[6] = Kappa*SIdiam;
	gMSAWave[4] = VolFrac;
	
	//Function sqhpa(qq)  {this is where Hayter-Penfold program began}
	
	//       FIRST CALCULATE COUPLING
	
	ss=pow(gMSAWave[4],(1.0/3.0));
	gMSAWave[9] = 2.0*ss*gMSAWave[5]*exp(gMSAWave[6]-gMSAWave[6]/ss);
	
	//        CALCULATE COEFFICIENTS, CHECK ALL IS WELL
	//        AND IF SO CALCULATE S(Q*SIG)
	
	ierr=0;
	ierr=sqcoef(ierr, gMSAWave);
	if (ierr>=0) {
		SofQ=sqhcal(Qdiam, gMSAWave);
	}else{
       	SofQ=NAN;
		//	print "Error Level = ",ierr
		//      print "Please report HPMSA problem with above error code"
	}
	
	return(SofQ);
}



/////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////
//
//
//      CALCULATES RESCALED VOLUME FRACTION AND CORRESPONDING
//      COEFFICIENTS FOR "SQHPA"
//
//      JOHN B. HAYTER   (I.L.L.)    14-SEP-81
//
//      ON EXIT:
//
//      SETA IS THE RESCALED VOLUME FRACTION
//      SGEK IS THE RESCALED CONTACT POTENTIAL
//      SAK IS THE RESCALED SCREENING CONSTANT
//      A,B,C,F,U,V ARE THE MSA COEFFICIENTS
//      G1= G(1+) IS THE CONTACT VALUE OF G(R/SIG):
//      FOR THE GILLAN CONDITION, THE DIFFERENCE FROM
//      ZERO INDICATES THE COMPUTATIONAL ACCURACY.
//
//      IR > 0:    NORMAL EXIT,  IR IS THE NUMBER OF ITERATIONS.
//         < 0:    FAILED TO CONVERGE
//
int
sqcoef(int ir, double gMSAWave[])
{	
	int itm=40,ix,ig,ii;
	double acc=5.0E-6,del,e1,e2,f1,f2;

	//      WAVE gMSAWave = $"root:HayPenMSA:gMSAWave"
	f1=0;		//these were never properly initialized...
	f2=0;
	
	ig=1;
	if (gMSAWave[6]>=(1.0+8.0*gMSAWave[4])) {
		ig=0;
		gMSAWave[15]=gMSAWave[14];
		gMSAWave[16]=gMSAWave[4];
		ix=1;
		ir = sqfun(ix,ir,gMSAWave);
		gMSAWave[14]=gMSAWave[15];
		gMSAWave[4]=gMSAWave[16];
		if((ir<0.0) || (gMSAWave[14]>=0.0)) {
			return ir;
		}
	}
	gMSAWave[10]=fmin(gMSAWave[4],0.20);
	if ((ig!=1) || ( gMSAWave[9]>=0.15)) {
		ii=0;                             
		do {
			ii=ii+1;
			if(ii>itm) {
				ir=-1;
				return ir;		
			}
			if (gMSAWave[10]<=0.0) {
			    gMSAWave[10]=gMSAWave[4]/ii;
			}
			if(gMSAWave[10]>0.6) {
			    gMSAWave[10] = 0.35/ii;
			}
			e1=gMSAWave[10];
			gMSAWave[15]=f1;
			gMSAWave[16]=e1;
			ix=2;
			ir = sqfun(ix,ir,gMSAWave);
			f1=gMSAWave[15];
			e1=gMSAWave[16];
			e2=gMSAWave[10]*1.01;
			gMSAWave[15]=f2;
			gMSAWave[16]=e2;
			ix=2;
			ir = sqfun(ix,ir,gMSAWave);
			f2=gMSAWave[15];
			e2=gMSAWave[16];
			e2=e1-(e2-e1)*f1/(f2-f1);
			gMSAWave[10] = e2;
			del = fabs((e2-e1)/e1);
		} while (del>acc);
		gMSAWave[15]=gMSAWave[14];
		gMSAWave[16]=e2;
		ix=4;
		ir = sqfun(ix,ir,gMSAWave);
		gMSAWave[14]=gMSAWave[15];
		e2=gMSAWave[16];
		ir=ii;
		if ((ig!=1) || (gMSAWave[10]>=gMSAWave[4])) {
		    return ir;
		}
	}
	gMSAWave[15]=gMSAWave[14];
	gMSAWave[16]=gMSAWave[4];
	ix=3;
	ir = sqfun(ix,ir,gMSAWave);
	gMSAWave[14]=gMSAWave[15];
	gMSAWave[4]=gMSAWave[16];
	if ((ir>=0) && (gMSAWave[14]<0.0)) {
		ir=-3;
	}
	return ir;
}


int
sqfun(int ix, int ir, double gMSAWave[])
{	
	double acc=1.0e-6;
	double reta,eta2,eta21,eta22,eta3,eta32,eta2d,eta2d2,eta3d,eta6d,e12,e24,rgek;
	double rak,ak1,ak2,dak,dak2,dak4,d,d2,dd2,dd4,dd45,ex1,ex2,sk,ck,ckma,skma;
	double al1,al2,al3,al4,al5,al6,be1,be2,be3,vu1,vu2,vu3,vu4,vu5,ph1,ph2,ta1,ta2,ta3,ta4,ta5;
	double a1,a2,a3,b1,b2,b3,v1,v2,v3,p1,p2,p3,pp,pp1,pp2,p1p2,t1,t2,t3,um1,um2,um3,um4,um5,um6;
	double w0,w1,w2,w3,w4,w12,w13,w14,w15,w16,w24,w25,w26,w32,w34,w3425,w35,w3526,w36,w46,w56;
	double fa,fap,ca,e24g,pwk,qpw,pg,del,fun,fund,g24;
	int ii,ibig,itm=40;
	//      WAVE gMSAWave = $"root:HayPenMSA:gMSAWave"
	a2=0;
	a3=0;
	b2=0;
	b3=0;
	v2=0;
	v3=0;
	p2=0;
	p3=0;
	
	//     CALCULATE CONSTANTS; NOTATION IS HAYTER PENFOLD (1981)
	
	reta = gMSAWave[16];                                                
	eta2 = reta*reta;
	eta3 = eta2*reta;
	e12 = 12.0*reta;
	e24 = e12+e12;
	gMSAWave[13] = pow( (gMSAWave[4]/gMSAWave[16]),(1.0/3.0));
	gMSAWave[12]=gMSAWave[6]/gMSAWave[13];
	ibig=0;
	if (( gMSAWave[12]>15.0) && (ix==1)) {
		ibig=1;
	}
    
	gMSAWave[11] = gMSAWave[5]*gMSAWave[13]*exp(gMSAWave[6]- gMSAWave[12]);
	rgek =  gMSAWave[11];
	rak =  gMSAWave[12];
	ak2 = rak*rak;
	ak1 = 1.0+rak;
	dak2 = 1.0/ak2;
	dak4 = dak2*dak2;
	d = 1.0-reta;
	d2 = d*d;
	dak = d/rak;
	dd2 = 1.0/d2;
	dd4 = dd2*dd2;
	dd45 = dd4*2.0e-1;
	eta3d=3.0*reta;
	eta6d = eta3d+eta3d;
	eta32 = eta3+ eta3;
	eta2d = reta+2.0;
	eta2d2 = eta2d*eta2d;
	eta21 = 2.0*reta+1.0;
	eta22 = eta21*eta21;
	
	//     ALPHA(I)
	
	al1 = -eta21*dak;
	al2 = (14.0*eta2-4.0*reta-1.0)*dak2;
	al3 = 36.0*eta2*dak4;
	
	//      BETA(I)
	
	be1 = -(eta2+7.0*reta+1.0)*dak;
	be2 = 9.0*reta*(eta2+4.0*reta-2.0)*dak2;
	be3 = 12.0*reta*(2.0*eta2+8.0*reta-1.0)*dak4;
	
	//      NU(I)
	
	vu1 = -(eta3+3.0*eta2+45.0*reta+5.0)*dak;
	vu2 = (eta32+3.0*eta2+42.0*reta-2.0e1)*dak2;
	vu3 = (eta32+3.0e1*reta-5.0)*dak4;
	vu4 = vu1+e24*rak*vu3;
	vu5 = eta6d*(vu2+4.0*vu3);
	
	//      PHI(I)
	
	ph1 = eta6d/rak;
	ph2 = d-e12*dak2;
	
	//      TAU(I)
	
	ta1 = (reta+5.0)/(5.0*rak);
	ta2 = eta2d*dak2;
	ta3 = -e12*rgek*(ta1+ta2);
	ta4 = eta3d*ak2*(ta1*ta1-ta2*ta2);
	ta5 = eta3d*(reta+8.0)*1.0e-1-2.0*eta22*dak2;
	
	//     double PRECISION SINH(K), COSH(K)
	
	ex1 = exp(rak);
	ex2 = 0.0;
	if ( gMSAWave[12]<20.0) {
		ex2=exp(-rak);
	}
	sk=0.5*(ex1-ex2);
	ck = 0.5*(ex1+ex2);
	ckma = ck-1.0-rak*sk;
	skma = sk-rak*ck;
	
	//      a(I)
	
	a1 = (e24*rgek*(al1+al2+ak1*al3)-eta22)*dd4;
	if (ibig==0) {
		a2 = e24*(al3*skma+al2*sk-al1*ck)*dd4;
		a3 = e24*(eta22*dak2-0.5*d2+al3*ckma-al1*sk+al2*ck)*dd4;
	}
	
	//      b(I)
	
	b1 = (1.5*reta*eta2d2-e12*rgek*(be1+be2+ak1*be3))*dd4;
	if (ibig==0) {
		b2 = e12*(-be3*skma-be2*sk+be1*ck)*dd4;
		b3 = e12*(0.5*d2*eta2d-eta3d*eta2d2*dak2-be3*ckma+be1*sk-be2*ck)*dd4;
	}
	
	//      V(I)
	
	v1 = (eta21*(eta2-2.0*reta+1.0e1)*2.5e-1-rgek*(vu4+vu5))*dd45;
	if (ibig==0) {
		v2 = (vu4*ck-vu5*sk)*dd45;
		v3 = ((eta3-6.0*eta2+5.0)*d-eta6d*(2.0*eta3-3.0*eta2+18.0*reta+1.0e1)*dak2+e24*vu3+vu4*sk-vu5*ck)*dd45;
	}
	
	
	//       P(I)
	
	pp1 = ph1*ph1;
	pp2 = ph2*ph2;
	pp = pp1+pp2;
	p1p2 = ph1*ph2*2.0;
	p1 = (rgek*(pp1+pp2-p1p2)-0.5*eta2d)*dd2;
	if (ibig==0) {
		p2 = (pp*sk+p1p2*ck)*dd2;
		p3 = (pp*ck+p1p2*sk+pp1-pp2)*dd2;
	}
	
	//       T(I)
	
	t1 = ta3+ta4*a1+ta5*b1;
	if (ibig!=0) {
		
		//		VERY LARGE SCREENING:  ASYMPTOTIC SOLUTION
		
  		v3 = ((eta3-6.0*eta2+5.0)*d-eta6d*(2.0*eta3-3.0*eta2+18.0*reta+1.0e1)*dak2+e24*vu3)*dd45;
		t3 = ta4*a3+ta5*b3+e12*ta2 - 4.0e-1*reta*(reta+1.0e1)-1.0;
		p3 = (pp1-pp2)*dd2;
		b3 = e12*(0.5*d2*eta2d-eta3d*eta2d2*dak2+be3)*dd4;
		a3 = e24*(eta22*dak2-0.5*d2-al3)*dd4;
		um6 = t3*a3-e12*v3*v3;
		um5 = t1*a3+a1*t3-e24*v1*v3;
		um4 = t1*a1-e12*v1*v1;
		al6 = e12*p3*p3;
		al5 = e24*p1*p3-b3-b3-ak2;
		al4 = e12*p1*p1-b1-b1;
		w56 = um5*al6-al5*um6;
		w46 = um4*al6-al4*um6;
		fa = -w46/w56;
		ca = -fa;
		gMSAWave[3] = fa;
		gMSAWave[2] = ca;
		gMSAWave[1] = b1+b3*fa;
		gMSAWave[0] = a1+a3*fa;
		gMSAWave[8] = v1+v3*fa;
		gMSAWave[14] = -(p1+p3*fa);
		gMSAWave[15] = gMSAWave[14];
		if (fabs(gMSAWave[15])<1.0e-3) {
			gMSAWave[15] = 0.0;
		}
		gMSAWave[10] = gMSAWave[16];
		
	} else {
        
		t2 = ta4*a2+ta5*b2+e12*(ta1*ck-ta2*sk);
		t3 = ta4*a3+ta5*b3+e12*(ta1*sk-ta2*(ck-1.0))-4.0e-1*reta*(reta+1.0e1)-1.0;
		
		//		MU(i)
		
		um1 = t2*a2-e12*v2*v2;
		um2 = t1*a2+t2*a1-e24*v1*v2;
		um3 = t2*a3+t3*a2-e24*v2*v3;
		um4 = t1*a1-e12*v1*v1;
		um5 = t1*a3+t3*a1-e24*v1*v3;
		um6 = t3*a3-e12*v3*v3;
		
		//			GILLAN CONDITION ?
		//
		//			YES - G(X=1+) = 0
		//
		//			COEFFICIENTS AND FUNCTION VALUE
		//
		if ((ix==1) || (ix==3)) {
			
			//			NO - CALCULATE REMAINING COEFFICIENTS.
			
			//			LAMBDA(I)
			
			al1 = e12*p2*p2;
			al2 = e24*p1*p2-b2-b2;
			al3 = e24*p2*p3;
			al4 = e12*p1*p1-b1-b1;
			al5 = e24*p1*p3-b3-b3-ak2;
			al6 = e12*p3*p3;
			
			//			OMEGA(I)
			
			w16 = um1*al6-al1*um6;
			w15 = um1*al5-al1*um5;
			w14 = um1*al4-al1*um4;
			w13 = um1*al3-al1*um3;
			w12 = um1*al2-al1*um2;
			
			w26 = um2*al6-al2*um6;
			w25 = um2*al5-al2*um5;
			w24 = um2*al4-al2*um4;
			
			w36 = um3*al6-al3*um6;
			w35 = um3*al5-al3*um5;
			w34 = um3*al4-al3*um4;
			w32 = um3*al2-al3*um2;
			//
			w46 = um4*al6-al4*um6;
			w56 = um5*al6-al5*um6;
			w3526 = w35+w26;
			w3425 = w34+w25;
			
			//			QUARTIC COEFFICIENTS
			
			w4 = w16*w16-w13*w36;
			w3 = 2.0*w16*w15-w13*w3526-w12*w36;
			w2 = w15*w15+2.0*w16*w14-w13*w3425-w12*w3526;
			w1 = 2.0*w15*w14-w13*w24-w12*w3425;
			w0 = w14*w14-w12*w24;
			
			//			ESTIMATE THE STARTING VALUE OF f
			
			if (ix==1) {
				//				LARGE K
				fap = (w14-w34-w46)/(w12-w15+w35-w26+w56-w32);
			} else {
				//				ASSUME NOT TOO FAR FROM GILLAN CONDITION.
				//				IF BOTH RGEK AND RAK ARE SMALL, USE P-W ESTIMATE.
				gMSAWave[14]=0.5*eta2d*dd2*exp(-rgek);
				if (( gMSAWave[11]<=2.0) && ( gMSAWave[11]>=0.0) && ( gMSAWave[12]<=1.0)) {
					e24g = e24*rgek*exp(rak);
					pwk = sqrt(e24g);
					qpw = (1.0-sqrt(1.0+2.0*d2*d*pwk/eta22))*eta21/d;
					gMSAWave[14] = -qpw*qpw/e24+0.5*eta2d*dd2;
				}
  				pg = p1+gMSAWave[14];
				ca = ak2*pg+2.0*(b3*pg-b1*p3)+e12*gMSAWave[14]*gMSAWave[14]*p3;
				ca = -ca/(ak2*p2+2.0*(b3*p2-b2*p3));
				fap = -(pg+p2*ca)/p3;
			}
			
			//			AND REFINE IT ACCORDING TO NEWTON
			ii=0;
			do {
				ii = ii+1;
				if (ii>itm) {
					//					FAILED TO CONVERGE IN ITM ITERATIONS
					ir=-2;
					return (ir);
				}
				fa = fap;
				fun = w0+(w1+(w2+(w3+w4*fa)*fa)*fa)*fa;
				fund = w1+(2.0*w2+(3.0*w3+4.0*w4*fa)*fa)*fa;
				fap = fa-fun/fund;
				del=fabs((fap-fa)/fa);
			} while (del>acc);
			
			ir = ir+ii;
			fa = fap;
			ca = -(w16*fa*fa+w15*fa+w14)/(w13*fa+w12);
			gMSAWave[14] = -(p1+p2*ca+p3*fa);
			gMSAWave[15] = gMSAWave[14];
			if (fabs(gMSAWave[15])<1.0e-3) {
				gMSAWave[15] = 0.0;
			}
			gMSAWave[10] = gMSAWave[16];
		} else {
			ca = ak2*p1+2.0*(b3*p1-b1*p3);
			ca = -ca/(ak2*p2+2.0*(b3*p2-b2*p3));
			fa = -(p1+p2*ca)/p3;
			if (ix==2) {
				gMSAWave[15] = um1*ca*ca+(um2+um3*fa)*ca+um4+um5*fa+um6*fa*fa;
			}
			if (ix==4) {
				gMSAWave[15] = -(p1+p2*ca+p3*fa);
			}
		}
   		gMSAWave[3] = fa;
		gMSAWave[2] = ca;
		gMSAWave[1] = b1+b2*ca+b3*fa;
		gMSAWave[0] = a1+a2*ca+a3*fa;
		gMSAWave[8] = (v1+v2*ca+v3*fa)/gMSAWave[0];
	}
   	g24 = e24*rgek*ex1;
	gMSAWave[7] = (rak*ak2*ca-g24)/(ak2*g24);
	return (ir);
}

double
sqhcal(double qq, double gMSAWave[])
{      	
    double SofQ,etaz,akz,gekz,e24,x1,x2,ck,sk,ak2,qk,q2k,qk2,qk3,qqk,sink,cosk,asink,qcosk,aqk,inter; 		
	//	WAVE gMSAWave = $"root:HayPenMSA:gMSAWave"

	etaz = gMSAWave[10];
	akz =  gMSAWave[12];
	gekz =  gMSAWave[11];
	e24 = 24.0*etaz;
	x1 = exp(akz);
	x2 = 0.0;
	if ( gMSAWave[12]<20.0) {
		x2 = exp(-akz);
	}
	ck = 0.5*(x1+x2);
	sk = 0.5*(x1-x2);
	ak2 = akz*akz;
	
	qk = qq/gMSAWave[13];
	q2k = qk*qk;
	if (qk<=1.0e-08) {
		SofQ = -1.0/gMSAWave[0];
	} else {
	// this rescales Q.sigma = 2.Q.Radius, so is hard to predict the value to test the function
	if (qk<=0.01) {
		// try Taylor series expansion at small qk (RKH Feb 2016, with help from Mathematica), 
		// transition point may need to depend on precision of cpu used and ALAS on the values of some of the parameters !
		// note have adsorbed a factor 24 from SofQ=
		// needs thorough test over wide range of parameter space!
		// there seem to be some rounding issues here in single precision, must use double
		aqk = gMSAWave[0]*(8.0+2.0*etaz) + 6*gMSAWave[1] -12.0*gMSAWave[3] 
			-24*(gekz*(1.0+akz) -ck*akz*gMSAWave[2] +gMSAWave[3]*(ck-1.0) +(gMSAWave[2]-gMSAWave[3]*akz)*sk )/ak2
			+q2k*( -(gMSAWave[0]*(48.0+15.0*etaz) +40.0*gMSAWave[1])/60.0 +gMSAWave[3] 
			+(4.0/ak2)*(gekz*(9.0+7.0*akz) +ck*(9.0*gMSAWave[3] -7.0*gMSAWave[2]*akz) +sk*(9.0*gMSAWave[2] -7.0*gMSAWave[3]*akz)) );
		SofQ = 1.0/(1.0-gMSAWave[10]*aqk);
	} else {
		qk2 = 1.0/q2k;
		qk3 = qk2/qk;
		qqk = 1.0/(qk*(q2k+ak2));
		SINCOS(qk,sink,cosk);
		asink = akz*sink;
		qcosk = qk*cosk;
		aqk = gMSAWave[0]*(sink-qcosk);
		aqk=aqk+gMSAWave[1]*((2.0*qk2-1.0)*qcosk+2.0*sink-2.0/qk);
		inter=24.0*qk3+4.0*(1.0-6.0*qk2)*sink;
		aqk=(aqk+0.5*etaz*gMSAWave[0]*(inter-(1.0-12.0*qk2+24.0*qk2*qk2)*qcosk))*qk3;
		aqk=aqk +gMSAWave[2]*(ck*asink-sk*qcosk)*qqk;
		aqk=aqk +gMSAWave[3]*(sk*asink-qk*(ck*cosk-1.0))*qqk;
		aqk=aqk +gMSAWave[3]*(cosk-1.0)*qk2;
		aqk=aqk -gekz*(asink+qcosk)*qqk;
		SofQ = 1.0/(1.0  -e24*aqk);
	} }
	return (SofQ);
}


