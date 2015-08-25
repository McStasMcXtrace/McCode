// GENERATED CODE --- DO NOT EDIT ---
// Code is produced by sasmodels.gen from sasmodels/models/MODEL.c

#ifdef __OPENCL_VERSION__
# define USE_OPENCL
#endif

// If opencl is not available, then we are compiling a C function
// Note: if using a C++ compiler, then define kernel as extern "C"
#ifndef USE_OPENCL
#  ifdef __cplusplus
     #include <cstdio>
     #include <cmath>
     using namespace std;
     #if defined(_MSC_VER)
     #   define kernel extern "C" __declspec( dllexport )
         inline float trunc(float x) { return x>=0?floor(x):-floor(-x); }
	 inline float fmin(float x, float y) { return x>y ? y : x; }
	 inline float fmax(float x, float y) { return x<y ? y : x; }
     #else
     #   define kernel extern "C"
     #endif
     inline void SINCOS(float angle, float &svar, float &cvar) { svar=sin(angle); cvar=cos(angle); }
#  else
     #include <stdio.h>
     #include <tgmath.h> // C99 type-generic math, so sin(float) => sinf
     // MSVC doesn't support C99, so no need for dllexport on C99 branch
     #define kernel
     #define SINCOS(angle,svar,cvar) do {const float _t_=angle; svar=sin(_t_);cvar=cos(_t_);} while (0)
#  endif
#  define global
#  define local
#  define constant const
// OpenCL powr(a,b) = C99 pow(a,b), b >= 0
// OpenCL pown(a,b) = C99 pow(a,b), b integer
#  define powr(a,b) pow(a,b)
#  define pown(a,b) pow(a,b)
#else
#  ifdef USE_SINCOS
#    define SINCOS(angle,svar,cvar) svar=sincos(angle,&cvar)
#  else
#    define SINCOS(angle,svar,cvar) do {const float _t_=angle; svar=sin(_t_);cvar=cos(_t_);} while (0)
#  endif
#endif

// Standard mathematical constants:
//   M_E, M_LOG2E, M_LOG10E, M_LN2, M_LN10, M_PI, M_PI_2=pi/2, M_PI_4=pi/4,
//   M_1_PI=1/pi, M_2_PI=2/pi, M_2_SQRTPI=2/sqrt(pi), SQRT2, SQRT1_2=sqrt(1/2)
// OpenCL defines M_constant_F for float constants, and nothing if float
// is not enabled on the card, which is why these constants may be missing
#ifndef M_PI
#  define M_PI 3.141592653589793f
#endif
#ifndef M_PI_2
#  define M_PI_2 1.570796326794897f
#endif
#ifndef M_PI_4
#  define M_PI_4 0.7853981633974483f
#endif

// Non-standard pi/180, used for converting between degrees and radians
#ifndef M_PI_180
#  define M_PI_180 0.017453292519943295f
#endif


#define VOLUME_PARAMETERS rpolar,requatorial
#define VOLUME_WEIGHT_PRODUCT rpolar_w*requatorial_w
#define IQ_KERNEL_NAME ellipsoid_Iq
#define IQ_PARAMETERS sld, solvent_sld, rpolar, requatorial
#define IQ_FIXED_PARAMETER_DECLARATIONS const float scale, \
    const float background, \
    const float sld, \
    const float solvent_sld
#define IQ_WEIGHT_PRODUCT rpolar_w*requatorial_w
#define IQ_DISPERSION_LENGTH_DECLARATIONS const int Nrpolar, \
    const int Nrequatorial
#define IQ_DISPERSION_LENGTH_SUM Nrpolar+Nrequatorial
#define IQ_OPEN_LOOPS     for (int rpolar_i=0; rpolar_i < Nrpolar; rpolar_i++) { \
      const float rpolar = loops[2*(rpolar_i)]; \
      const float rpolar_w = loops[2*(rpolar_i)+1]; \
      for (int requatorial_i=0; requatorial_i < Nrequatorial; requatorial_i++) { \
        const float requatorial = loops[2*(requatorial_i+Nrpolar)]; \
        const float requatorial_w = loops[2*(requatorial_i+Nrpolar)+1];
#define IQ_CLOSE_LOOPS       } \
    }
#define IQXY_KERNEL_NAME ellipsoid_Iqxy
#define IQXY_PARAMETERS sld, solvent_sld, rpolar, requatorial, theta, phi
#define IQXY_FIXED_PARAMETER_DECLARATIONS const float scale, \
    const float background, \
    const float sld, \
    const float solvent_sld
#define IQXY_WEIGHT_PRODUCT rpolar_w*requatorial_w*theta_w*phi_w
#define IQXY_DISPERSION_LENGTH_DECLARATIONS const int Nrpolar, \
    const int Nrequatorial, \
    const int Ntheta, \
    const int Nphi
#define IQXY_DISPERSION_LENGTH_SUM Nrpolar+Nrequatorial+Ntheta+Nphi
#define IQXY_OPEN_LOOPS     for (int rpolar_i=0; rpolar_i < Nrpolar; rpolar_i++) { \
      const float rpolar = loops[2*(rpolar_i)]; \
      const float rpolar_w = loops[2*(rpolar_i)+1]; \
      for (int requatorial_i=0; requatorial_i < Nrequatorial; requatorial_i++) { \
        const float requatorial = loops[2*(requatorial_i+Nrpolar)]; \
        const float requatorial_w = loops[2*(requatorial_i+Nrpolar)+1]; \
        for (int theta_i=0; theta_i < Ntheta; theta_i++) { \
          const float theta = loops[2*(theta_i+Nrpolar+Nrequatorial)]; \
          const float theta_w = loops[2*(theta_i+Nrpolar+Nrequatorial)+1]; \
          for (int phi_i=0; phi_i < Nphi; phi_i++) { \
            const float phi = loops[2*(phi_i+Nrpolar+Nrequatorial+Ntheta)]; \
            const float phi_w = loops[2*(phi_i+Nrpolar+Nrequatorial+Ntheta)+1];
#define IQXY_CLOSE_LOOPS           } \
        } \
      } \
    }
#define IQXY_HAS_THETA 1

float J1(float x);
float J1(float x)
{
  const float ax = fabs(x);
  if (ax < 8.0f) {
    const float y = x*x;
    const float ans1 = x*(72362614232.0f
              + y*(-7895059235.0f
              + y*(242396853.1f
              + y*(-2972611.439f
              + y*(15704.48260f
              + y*(-30.16036606f))))));
    const float ans2 = 144725228442.0f
              + y*(2300535178.0f
              + y*(18583304.74f
              + y*(99447.43394f
              + y*(376.9991397f
              + y))));
    return ans1/ans2;
  } else {
    const float y = 64.0f/(ax*ax);
    const float xx = ax - 2.356194491f;
    const float ans1 = 1.0f
              + y*(0.183105e-2f
              + y*(-0.3516396496e-4f
              + y*(0.2457520174e-5f
              + y*-0.240337019e-6f)));
    const float ans2 = 0.04687499995f
              + y*(-0.2002690873e-3f
              + y*(0.8449199096e-5f
              + y*(-0.88228987e-6f
              + y*0.105787412e-6f)));
    float sn,cn;
    SINCOS(xx, sn, cn);
    const float ans = sqrt(0.636619772f/ax) * (cn*ans1 - (8.0f/ax)*sn*ans2);
    return (x < 0.0f) ? -ans : ans;
  }
}


/*
 *  GaussWeights.c
 *  SANSAnalysis
 *
 *  Created by Andrew Jackson on 4/23/07.f
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

// Gaussians
constant float Gauss76Wt[76]={
	.00126779163408536f,		//0
	.00294910295364247f,
	.00462793522803742f,
	.00629918049732845f,
	.00795984747723973f,
	.00960710541471375f,
	.0112381685696677f,
	.0128502838475101f,
	.0144407317482767f,
	.0160068299122486f,
	.0175459372914742f,		//10
	.0190554584671906f,
	.020532847967908f,
	.0219756145344162f,
	.0233813253070112f,
	.0247476099206597f,
	.026072164497986f,
	.0273527555318275f,
	.028587223650054f,
	.029773487255905f,
	.0309095460374916f,		//20
	.0319934843404216f,
	.0330234743977917f,
	.0339977794120564f,
	.0349147564835508f,
	.0357728593807139f,
	.0365706411473296f,
	.0373067565423816f,
	.0379799643084053f,
	.0385891292645067f,
	.0391332242205184f,		//30
	.0396113317090621f,
	.0400226455325968f,
	.040366472122844f,
	.0406422317102947f,
	.0408494593018285f,
	.040987805464794f,
	.0410570369162294f,
	.0410570369162294f,
	.040987805464794f,
	.0408494593018285f,		//40
	.0406422317102947f,
	.040366472122844f,
	.0400226455325968f,
	.0396113317090621f,
	.0391332242205184f,
	.0385891292645067f,
	.0379799643084053f,
	.0373067565423816f,
	.0365706411473296f,
	.0357728593807139f,		//50
	.0349147564835508f,
	.0339977794120564f,
	.0330234743977917f,
	.0319934843404216f,
	.0309095460374916f,
	.029773487255905f,
	.028587223650054f,
	.0273527555318275f,
	.026072164497986f,
	.0247476099206597f,		//60
	.0233813253070112f,
	.0219756145344162f,
	.020532847967908f,
	.0190554584671906f,
	.0175459372914742f,
	.0160068299122486f,
	.0144407317482767f,
	.0128502838475101f,
	.0112381685696677f,
	.00960710541471375f,		//70
	.00795984747723973f,
	.00629918049732845f,
	.00462793522803742f,
	.00294910295364247f,
	.00126779163408536f		//75 (indexed from 0)
};

constant float Gauss76Z[76]={
	-.999505948362153f,		//0
	-.997397786355355f,
	-.993608772723527f,
	-.988144453359837f,
	-.981013938975656f,
	-.972229228520377f,
	-.961805126758768f,
	-.949759207710896f,
	-.936111781934811f,
	-.92088586125215f,
	-.904107119545567f,		//10
	-.885803849292083f,
	-.866006913771982f,
	-.844749694983342f,
	-.822068037328975f,
	-.7980001871612f,
	-.77258672828181f,
	-.74587051350361f,
	-.717896592387704f,
	-.688712135277641f,
	-.658366353758143f,		//20
	-.626910417672267f,
	-.594397368836793f,
	-.560882031601237f,
	-.526420920401243f,
	-.491072144462194f,
	-.454895309813726f,
	-.417951418780327f,
	-.380302767117504f,
	-.342012838966962f,
	-.303146199807908f,		//30
	-.263768387584994f,
	-.223945802196474f,
	-.183745593528914f,
	-.143235548227268f,
	-.102483975391227f,
	-.0615595913906112f,
	-.0205314039939986f,
	.0205314039939986f,
	.0615595913906112f,
	.102483975391227f,			//40
	.143235548227268f,
	.183745593528914f,
	.223945802196474f,
	.263768387584994f,
	.303146199807908f,
	.342012838966962f,
	.380302767117504f,
	.417951418780327f,
	.454895309813726f,
	.491072144462194f,		//50
	.526420920401243f,
	.560882031601237f,
	.594397368836793f,
	.626910417672267f,
	.658366353758143f,
	.688712135277641f,
	.717896592387704f,
	.74587051350361f,
	.77258672828181f,
	.7980001871612f,	//60
	.822068037328975f,
	.844749694983342f,
	.866006913771982f,
	.885803849292083f,
	.904107119545567f,
	.92088586125215f,
	.936111781934811f,
	.949759207710896f,
	.961805126758768f,
	.972229228520377f,		//70
	.981013938975656f,
	.988144453359837f,
	.993608772723527f,
	.997397786355355f,
	.999505948362153f		//75
};


float form_volume(float rpolar, float requatorial);
float Iq(float q, float sld, float solvent_sld, float rpolar, float requatorial);
float Iqxy(float qx, float qy, float sld, float solvent_sld,
    float rpolar, float requatorial, float theta, float phi);

float _ellipsoid_kernel(float q, float rpolar, float requatorial, float cos_alpha);
float _ellipsoid_kernel(float q, float rpolar, float requatorial, float cos_alpha)
{
    float sn, cn;
    float ratio = rpolar/requatorial;
    const float u = q*requatorial*sqrt(1.0f
                   + cos_alpha*cos_alpha*(ratio*ratio - 1.0f));
    SINCOS(u, sn, cn);
    const float f = ( u==0.0f ? 1.0f : 3.0f*(sn-u*cn)/(u*u*u) );
    return f*f;
}

float form_volume(float rpolar, float requatorial)
{
    return 1.333333333333333f*M_PI*rpolar*requatorial*requatorial;
}

float Iq(float q,
    float sld,
    float solvent_sld,
    float rpolar,
    float requatorial)
{
    //const float lower = 0.0f;
    //const float upper = 1.0f;
    float total = 0.0f;
    for (int i=0;i<76;i++) {
        //const float cos_alpha = (Gauss76Z[i]*(upper-lower) + upper + lower)/2;
        const float cos_alpha = 0.5f*(Gauss76Z[i] + 1.0f);
        total += Gauss76Wt[i] * _ellipsoid_kernel(q, rpolar, requatorial, cos_alpha);
    }
    //const float form = (upper-lower)/2*total;
    const float form = 0.5f*total;
    const float s = (sld - solvent_sld) * form_volume(rpolar, requatorial);
    return 1.0e-4f * form * s * s;
}

float Iqxy(float qx, float qy,
    float sld,
    float solvent_sld,
    float rpolar,
    float requatorial,
    float theta,
    float phi)
{
    float sn, cn;

    const float q = sqrt(qx*qx + qy*qy);
    SINCOS(theta*M_PI_180, sn, cn);
    const float cos_alpha = cn*cos(phi*M_PI_180)*(qx/q) + sn*(qy/q);
    const float form = _ellipsoid_kernel(q, rpolar, requatorial, cos_alpha);
    const float s = (sld - solvent_sld) * form_volume(rpolar, requatorial);

    return 1.0e-4f * form * s * s;
}



/*
    ##########################################################
    #                                                        #
    #   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #
    #   !!                                              !!   #
    #   !!  KEEP THIS CODE CONSISTENT WITH KERNELPY.PY  !!   #
    #   !!                                              !!   #
    #   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #
    #                                                        #
    ##########################################################
*/

#ifdef IQ_KERNEL_NAME
kernel void IQ_KERNEL_NAME(
    global const float *q,
    global float *result,
    const int Nq,
#ifdef IQ_OPEN_LOOPS
  #ifdef USE_OPENCL
    global float *loops_g,
  #endif
    local float *loops,
    const float cutoff,
    IQ_DISPERSION_LENGTH_DECLARATIONS,
#endif
    IQ_FIXED_PARAMETER_DECLARATIONS
    )
{
#ifdef USE_OPENCL
  #ifdef IQ_OPEN_LOOPS
  // copy loops info to local memory
  event_t e = async_work_group_copy(loops, loops_g, (IQ_DISPERSION_LENGTH_SUM)*2, 0);
  wait_group_events(1, &e);
  #endif

  int i = get_global_id(0);
  if (i < Nq)
#else
  #pragma omp parallel for
  for (int i=0; i < Nq; i++)
#endif
  {
    const float qi = q[i];
#ifdef IQ_OPEN_LOOPS
    float ret=0.0f, norm=0.0f;
  #ifdef VOLUME_PARAMETERS
    float vol=0.0f, norm_vol=0.0f;
  #endif
    IQ_OPEN_LOOPS
    //for (int radius_i=0; radius_i < Nradius; radius_i++) {
    //  const float radius = loops[2*(radius_i)];
    //  const float radius_w = loops[2*(radius_i)+1];

    const float weight = IQ_WEIGHT_PRODUCT;
    if (weight > cutoff) {
      const float scattering = Iq(qi, IQ_PARAMETERS);
      if (scattering >= 0.0f) { // scattering cannot be negative
        ret += weight*scattering;
        norm += weight;
      #ifdef VOLUME_PARAMETERS
        const float vol_weight = VOLUME_WEIGHT_PRODUCT;
        vol += vol_weight*form_volume(VOLUME_PARAMETERS);
        norm_vol += vol_weight;
      #endif
      }
    //else { printf("exclude qx,qy,I:%g,%g,%g\n",qi,scattering); }
    }
    IQ_CLOSE_LOOPS
  #ifdef VOLUME_PARAMETERS
    if (vol*norm_vol != 0.0f) {
      ret *= norm_vol/vol;
    }
  #endif
    result[i] = scale*ret/norm+background;
#else
    result[i] = scale*Iq(qi, IQ_PARAMETERS) + background;
#endif
  }
}
#endif


#ifdef IQXY_KERNEL_NAME
kernel void IQXY_KERNEL_NAME(
    global const float *qx,
    global const float *qy,
    global float *result,
    const int Nq,
#ifdef IQXY_OPEN_LOOPS
  #ifdef USE_OPENCL
    global float *loops_g,
  #endif
    local float *loops,
    const float cutoff,
    IQXY_DISPERSION_LENGTH_DECLARATIONS,
#endif
    IQXY_FIXED_PARAMETER_DECLARATIONS
    )
{
#ifdef USE_OPENCL
  #ifdef IQXY_OPEN_LOOPS
  // copy loops info to local memory
  event_t e = async_work_group_copy(loops, loops_g, (IQXY_DISPERSION_LENGTH_SUM)*2, 0);
  wait_group_events(1, &e);
  #endif

  int i = get_global_id(0);
  if (i < Nq)
#else
  #pragma omp parallel for
  for (int i=0; i < Nq; i++)
#endif
  {
    const float qxi = qx[i];
    const float qyi = qy[i];
#ifdef IQXY_OPEN_LOOPS
    float ret=0.0f, norm=0.0f;
    #ifdef VOLUME_PARAMETERS
    float vol=0.0f, norm_vol=0.0f;
    #endif
    IQXY_OPEN_LOOPS
    //for (int radius_i=0; radius_i < Nradius; radius_i++) {
    //  const float radius = loops[2*(radius_i)];
    //  const float radius_w = loops[2*(radius_i)+1];

    const float weight = IQXY_WEIGHT_PRODUCT;
    if (weight > cutoff) {

      const float scattering = Iqxy(qxi, qyi, IQXY_PARAMETERS);
      if (scattering >= 0.0f) { // scattering cannot be negative
        // TODO: use correct angle for spherical correction
        // Definition of theta and phi are probably reversed relative to the
        // equation which gave rise to this correction, leading to an
        // attenuation of the pattern as theta moves through pi/2.f  Either
        // reverse the meanings of phi and theta in the forms, or use phi
        // rather than theta in this correction.  Current code uses cos(theta)
        // so that values match those of sasview.
      #ifdef IQXY_HAS_THETA
        const float spherical_correction
          = (Ntheta>1 ? fabs(cos(M_PI_180*theta))*M_PI_2:1.0f);
        ret += spherical_correction * weight * scattering;
      #else
        ret += weight * scattering;
      #endif
        norm += weight;
      #ifdef VOLUME_PARAMETERS
        const float vol_weight = VOLUME_WEIGHT_PRODUCT;
        vol += vol_weight*form_volume(VOLUME_PARAMETERS);
      #endif
        norm_vol += vol_weight;
      }
      //else { printf("exclude qx,qy,I:%g,%g,%g\n",qi,scattering); }
    }
    IQXY_CLOSE_LOOPS
  #ifdef VOLUME_PARAMETERS
    if (vol*norm_vol != 0.0f) {
      ret *= norm_vol/vol;
    }
  #endif
    result[i] = scale*ret/norm+background;
#else
    result[i] = scale*Iqxy(qxi, qyi, IQXY_PARAMETERS) + background;
#endif
  }
}
#endif
