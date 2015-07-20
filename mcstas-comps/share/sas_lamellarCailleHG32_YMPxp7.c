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


#define VOLUME_PARAMETERS tail_length,head_length,spacing
#define VOLUME_WEIGHT_PRODUCT tail_length_w*head_length_w*spacing_w
#define VOLUME_PARAMETER_DECLARATIONS float tail_length, float head_length, float spacing
#define IQ_KERNEL_NAME lamellarCailleHG_Iq
#define IQ_PARAMETERS tail_length, head_length, Nlayers, spacing, Caille_parameter, sld, head_sld, solvent_sld
#define IQ_FIXED_PARAMETER_DECLARATIONS const float scale, \
    const float background, \
    const float Nlayers, \
    const float Caille_parameter, \
    const float sld, \
    const float head_sld, \
    const float solvent_sld
#define IQ_WEIGHT_PRODUCT tail_length_w*head_length_w*spacing_w
#define IQ_DISPERSION_LENGTH_DECLARATIONS const int Ntail_length, \
    const int Nhead_length, \
    const int Nspacing
#define IQ_DISPERSION_LENGTH_SUM Ntail_length+Nhead_length+Nspacing
#define IQ_OPEN_LOOPS     for (int tail_length_i=0; tail_length_i < Ntail_length; tail_length_i++) { \
      const float tail_length = loops[2*(tail_length_i)]; \
      const float tail_length_w = loops[2*(tail_length_i)+1]; \
      for (int head_length_i=0; head_length_i < Nhead_length; head_length_i++) { \
        const float head_length = loops[2*(head_length_i+Ntail_length)]; \
        const float head_length_w = loops[2*(head_length_i+Ntail_length)+1]; \
        for (int spacing_i=0; spacing_i < Nspacing; spacing_i++) { \
          const float spacing = loops[2*(spacing_i+Ntail_length+Nhead_length)]; \
          const float spacing_w = loops[2*(spacing_i+Ntail_length+Nhead_length)+1];
#define IQ_CLOSE_LOOPS         } \
      } \
    }
#define IQXY_KERNEL_NAME lamellarCailleHG_Iqxy
#define IQXY_PARAMETERS tail_length, head_length, Nlayers, spacing, Caille_parameter, sld, head_sld, solvent_sld
#define IQXY_FIXED_PARAMETER_DECLARATIONS const float scale, \
    const float background, \
    const float Nlayers, \
    const float Caille_parameter, \
    const float sld, \
    const float head_sld, \
    const float solvent_sld
#define IQXY_WEIGHT_PRODUCT tail_length_w*head_length_w*spacing_w
#define IQXY_DISPERSION_LENGTH_DECLARATIONS const int Ntail_length, \
    const int Nhead_length, \
    const int Nspacing
#define IQXY_DISPERSION_LENGTH_SUM Ntail_length+Nhead_length+Nspacing
#define IQXY_OPEN_LOOPS     for (int tail_length_i=0; tail_length_i < Ntail_length; tail_length_i++) { \
      const float tail_length = loops[2*(tail_length_i)]; \
      const float tail_length_w = loops[2*(tail_length_i)+1]; \
      for (int head_length_i=0; head_length_i < Nhead_length; head_length_i++) { \
        const float head_length = loops[2*(head_length_i+Ntail_length)]; \
        const float head_length_w = loops[2*(head_length_i+Ntail_length)+1]; \
        for (int spacing_i=0; spacing_i < Nspacing; spacing_i++) { \
          const float spacing = loops[2*(spacing_i+Ntail_length+Nhead_length)]; \
          const float spacing_w = loops[2*(spacing_i+Ntail_length+Nhead_length)+1];
#define IQXY_CLOSE_LOOPS         } \
      } \
    }
#define IQXY_PARAMETER_DECLARATIONS float tail_length, float head_length, float Nlayers, float spacing, float Caille_parameter, float sld, float head_sld, float solvent_sld

/*	LamellarCailleHG kernel - allows for name changes of passed parameters ...
    Maths identical to LamellarCaille apart from the line for P(Q)
*/

float Iq(float qval,
      float tail_length,
      float head_length,
      float Nlayers, 
      float dd,
      float Cp,
      float tail_sld,
      float head_sld,
      float solvent_sld);

float Iq(float qval,
      float tail_length,
      float head_length,
      float Nlayers, 
      float dd,
      float Cp,
      float tail_sld,
      float head_sld,
      float solvent_sld)
{
  float NN;   //local variables of coefficient wave
  float inten,Pq,Sq,alpha,temp,t2;
  //float dQ, dQDefault, t1, t3;
  int ii,NNint;
  // from wikipedia 0.577215664901532860606512090082402431042159335f
  const float Euler = 0.577215664901533f;   // Euler's constant, increased sig figs for new models Feb 2015
  //dQDefault = 0.0f;    //[=] 1/A, q-resolution, default value
  //dQ = dQDefault; // REMOVED UNUSED dQ calculations for new models Feb 2015

  NN = trunc(Nlayers);    //be sure that NN is an integer
  
  Pq = (head_sld-solvent_sld)*(sin(qval*(head_length+tail_length))-sin(qval*tail_length)) +
              (tail_sld-solvent_sld)*sin(qval*tail_length);
  Pq *= Pq;
  Pq *= 4.0f/(qval*qval);

  NNint = (int)NN;    //cast to an integer for the loop
  ii=0;
  Sq = 0.0f;
  for(ii=1;ii<=(NNint-1);ii+=1) {

    //fii = (float)ii;   //do I really need to do this? - unused variable, removed 18Feb2015

    temp = 0.0f;
    alpha = Cp/4.0f/M_PI/M_PI*(log(M_PI*ii) + Euler);
    //t1 = 2.0f*dQ*dQ*dd*dd*alpha;
    t2 = 2.0f*qval*qval*dd*dd*alpha;
    //t3 = dQ*dQ*dd*dd*ii*ii;

    temp = 1.0f-ii/NN;
    //temp *= cos(dd*qval*ii/(1.0f+t1));
    temp *= cos(dd*qval*ii);
    //if (temp < 0) printf("q=%g: ii=%d, cos(dd*q*ii)=cos(%g) < 0\n",qval,ii,dd*qval*ii);
    //temp *= exp(-1.0f*(t2 + t3)/(2.0f*(1.0f+t1)) );
    temp *= exp(-t2/2.0f );
    //temp /= sqrt(1.0f+t1);

    Sq += temp;
  }

  Sq *= 2.0f;
  Sq += 1.0f;

  //if (Sq < 0) printf("q=%g: S(q) =%g\n", qval, Sq);

  inten = 2.0f*M_PI*Pq*Sq/(dd*qval*qval);

  inten *= 1.0e-04f;   // 1/A to 1/cm
  return(inten);
}



float form_volume(VOLUME_PARAMETER_DECLARATIONS);
float form_volume(VOLUME_PARAMETER_DECLARATIONS) {
    
    return 1.0f;
    
}


float Iqxy(float qx, float qy, IQXY_PARAMETER_DECLARATIONS);
float Iqxy(float qx, float qy, IQXY_PARAMETER_DECLARATIONS) {
    
    return Iq(sqrt(qx*qx+qy*qy), IQ_PARAMETERS);
    
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
