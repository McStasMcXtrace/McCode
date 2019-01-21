// GENERATED CODE --- DO NOT EDIT ---
// Code is produced by sasmodels.gen from sasmodels/models/MODEL.c

#ifdef __OPENCL_VERSION__
# define USE_OPENCL
#endif

#define USE_KAHAN_SUMMATION 0

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


#define IQ_KERNEL_NAME mass_fractal_Iq
#define IQ_PARAMETERS radius, mass_dim, cutoff_length
#define IQ_FIXED_PARAMETER_DECLARATIONS const float scale, \
    const float background, \
    const float radius, \
    const float mass_dim, \
    const float cutoff_length
#define IQXY_KERNEL_NAME mass_fractal_Iqxy
#define IQXY_PARAMETERS radius, mass_dim, cutoff_length
#define IQXY_FIXED_PARAMETER_DECLARATIONS const float scale, \
    const float background, \
    const float radius, \
    const float mass_dim, \
    const float cutoff_length

/**
* Spherical Bessel function 3*j1(x)/x
*
* Used for low q to avoid cancellation error.
* Note that the values differ from sasview ~ 5e-12 rather than 5e-14, but
* in this case it is likely cancellation errors in the original expression
* using float precision that are the source.  Single precision only
* requires the first 3 terms.  Double precision requires the 4th term.
* The fifth term is not needed, and is commented out.
* Taylor expansion:
*      1.0f + q2*(-3.f/30.f + q2*(3.f/840.f))+ q2*(-3.f/45360.f + q2*(3.f/3991680.f))))
* Expression returned from Herbie (herbie.uwpise.org/demo):
*      const float t = ((1.f + 3.f*q2*q2/5600.f) - q2/20.f);
*      return t*t;
*/

float sph_j1c(float q);
float sph_j1c(float q)
{
    const float q2 = q*q;
    float sin_q, cos_q;

    SINCOS(q, sin_q, cos_q);

    const float bessel = (q < 0.384038453352533f)
        ? (1.0f + q2*(-3.f/30.f + q2*(3.f/840.f)))
        : 3.0f*(sin_q/q - cos_q)/q2;

    return bessel;

 /*
    // Code to test various expressions
    if (sizeof(q2) > 4) {
        return 3.0f*(sin_q/q - cos_q)/q2;
    } else if (q < 0.384038453352533f) {
        //const float t = ((1.f + 3.f*q2*q2/5600.f) - q2/20.f); return t*t;
        return 1.0f + q2*q2*(3.f/840.f) - q2*(3.f/30.f);
        //return 1.0f + q2*(-3.f/30.f + q2*(3.f/840.f));
        //return 1.0f + q2*(-3.f/30.f + q2*(3.f/840.f + q2*(-3.f/45360.f)));
        //return 1.0f + q2*(-3.f/30.f + q2*(3.f/840.f + q2*(-3.f/45360.f + q2*(3.f/3991680.f))));
    } else {
        return 3.0f*(sin_q/q - cos_q)/q2;
    }
*/
}


float lanczos_gamma(float q);
float lanczos_gamma(float q)
{
    // Lanczos approximation to the Gamma function.

    float x,y,tmp,ser;
    float coeff[6]=
        {76.18009172947146f,     -86.50532032941677f,
         24.01409824083091f,     -1.231739572450155f,
          0.1208650973866179e-2f,-0.5395239384953e-5f};

    y=x=q;
    tmp  = x+5.5f;
    tmp -= (x+0.5f)*log(tmp);
    ser  = 1.000000000190015f;
    for (int j=0; j<=5; j++) {
        y+=1.0f;
        ser += coeff[j]/y;
    }
    return -tmp+log(2.5066282746310005f*ser/x);
}

float form_volume(float radius);

float Iq(float q,
          float radius,
          float mass_dim,
          float cutoff_length);

float Iqxy(float qx, float qy,
          float radius,
          float mass_dim,
          float cutoff_length);


static float _mass_fractal_kernel(float q,
          float radius,
          float mass_dim,
          float cutoff_length)
{
    // Actively check the argument.
    if (mass_dim <= 1.0f){
       return 0.0f;
    }

    //calculate P(q)
    float pq = sph_j1c(q*radius);
    pq = pq*pq;

    //calculate S(q)
    float mmo = mass_dim-1.0f;
    float sq = exp(lanczos_gamma(mmo))*sin((mmo)*atan(q*cutoff_length));
    sq *= pow(cutoff_length, mmo);
    sq /= pow((1.0f + (q*cutoff_length)*(q*cutoff_length)),(mmo/2.0f));
    sq /= q;

    //combine and return
    float result = pq * sq;

    return result;
}
float form_volume(float radius){

    return 1.333333333333333f*M_PI*radius*radius*radius;
}

float Iq(float q,
          float radius,
          float mass_dim,
          float cutoff_length)
{
    return _mass_fractal_kernel(q,
           radius,
           mass_dim,
           cutoff_length);
}

// Iqxy is never called since no orientation or magnetic parameters.
float Iqxy(float qx, float qy,
          float radius,
          float mass_dim,
          float cutoff_length)
{
    float q = sqrt(qx*qx + qy*qy);
    return _mass_fractal_kernel(q,
           radius,
           mass_dim,
           cutoff_length);
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
      // allow kernels to exclude invalid regions by returning NaN
      if (!isnan(scattering)) {
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
    #if USE_KAHAN_SUMMATION
    float accumulated_error = 0.0f;
    #endif
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
      if (!isnan(scattering)) { // if scattering is bad, exclude it from sum
      //if (scattering >= 0.0f) { // scattering cannot be negative
        // TODO: use correct angle for spherical correction
        // Definition of theta and phi are probably reversed relative to the
        // equation which gave rise to this correction, leading to an
        // attenuation of the pattern as theta moves through pi/2.f  Either
        // reverse the meanings of phi and theta in the forms, or use phi
        // rather than theta in this correction.  Current code uses cos(theta)
        // so that values match those of sasview.
      #if defined(IQXY_HAS_THETA) // && 0
        const float spherical_correction
          = (Ntheta>1 ? fabs(cos(M_PI_180*theta))*M_PI_2:1.0f);
        const float next = spherical_correction * weight * scattering;
      #else
        const float next = weight * scattering;
      #endif
      #if USE_KAHAN_SUMMATION
        const float y = next - accumulated_error;
        const float t = ret + y;
        accumulated_error = (t - ret) - y;
        ret = t;
      #else
        ret += next;
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
