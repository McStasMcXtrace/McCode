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


#define VOLUME_PARAMETERS radius,cap_radius,length
#define VOLUME_WEIGHT_PRODUCT radius_w*cap_radius_w*length_w
#define IQ_KERNEL_NAME capped_cylinder_Iq
#define IQ_PARAMETERS sld, solvent_sld, radius, cap_radius, length
#define IQ_FIXED_PARAMETER_DECLARATIONS const float scale, \
    const float background, \
    const float sld, \
    const float solvent_sld
#define IQ_WEIGHT_PRODUCT radius_w*cap_radius_w*length_w
#define IQ_DISPERSION_LENGTH_DECLARATIONS const int Nradius, \
    const int Ncap_radius, \
    const int Nlength
#define IQ_DISPERSION_LENGTH_SUM Nradius+Ncap_radius+Nlength
#define IQ_OPEN_LOOPS     for (int radius_i=0; radius_i < Nradius; radius_i++) { \
      const float radius = loops[2*(radius_i)]; \
      const float radius_w = loops[2*(radius_i)+1]; \
      for (int cap_radius_i=0; cap_radius_i < Ncap_radius; cap_radius_i++) { \
        const float cap_radius = loops[2*(cap_radius_i+Nradius)]; \
        const float cap_radius_w = loops[2*(cap_radius_i+Nradius)+1]; \
        for (int length_i=0; length_i < Nlength; length_i++) { \
          const float length = loops[2*(length_i+Nradius+Ncap_radius)]; \
          const float length_w = loops[2*(length_i+Nradius+Ncap_radius)+1];
#define IQ_CLOSE_LOOPS         } \
      } \
    }
#define IQXY_KERNEL_NAME capped_cylinder_Iqxy
#define IQXY_PARAMETERS sld, solvent_sld, radius, cap_radius, length, theta, phi
#define IQXY_FIXED_PARAMETER_DECLARATIONS const float scale, \
    const float background, \
    const float sld, \
    const float solvent_sld
#define IQXY_WEIGHT_PRODUCT radius_w*cap_radius_w*length_w*theta_w*phi_w
#define IQXY_DISPERSION_LENGTH_DECLARATIONS const int Nradius, \
    const int Ncap_radius, \
    const int Nlength, \
    const int Ntheta, \
    const int Nphi
#define IQXY_DISPERSION_LENGTH_SUM Nradius+Ncap_radius+Nlength+Ntheta+Nphi
#define IQXY_OPEN_LOOPS     for (int radius_i=0; radius_i < Nradius; radius_i++) { \
      const float radius = loops[2*(radius_i)]; \
      const float radius_w = loops[2*(radius_i)+1]; \
      for (int cap_radius_i=0; cap_radius_i < Ncap_radius; cap_radius_i++) { \
        const float cap_radius = loops[2*(cap_radius_i+Nradius)]; \
        const float cap_radius_w = loops[2*(cap_radius_i+Nradius)+1]; \
        for (int length_i=0; length_i < Nlength; length_i++) { \
          const float length = loops[2*(length_i+Nradius+Ncap_radius)]; \
          const float length_w = loops[2*(length_i+Nradius+Ncap_radius)+1]; \
          for (int theta_i=0; theta_i < Ntheta; theta_i++) { \
            const float theta = loops[2*(theta_i+Nradius+Ncap_radius+Nlength)]; \
            const float theta_w = loops[2*(theta_i+Nradius+Ncap_radius+Nlength)+1]; \
            for (int phi_i=0; phi_i < Nphi; phi_i++) { \
              const float phi = loops[2*(phi_i+Nradius+Ncap_radius+Nlength+Ntheta)]; \
              const float phi_w = loops[2*(phi_i+Nradius+Ncap_radius+Nlength+Ntheta)+1];
#define IQXY_CLOSE_LOOPS             } \
          } \
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


float form_volume(float radius, float cap_radius, float length);
float Iq(float q, float sld, float solvent_sld,
    float radius, float cap_radius, float length);
float Iqxy(float qx, float qy, float sld, float solvent_sld,
    float radius, float cap_radius, float length, float theta, float phi);

// Integral over a convex lens kernel for t in [h/R,1].  See the docs for
// the definition of the function being integrated.
//   q is the magnitude of the q vector.
//   h is the length of the lens "inside" the cylinder.  This negative wrt the
//       definition of h in the docs.
//   cap_radius is the radius of the lens
//   length is the cylinder length, or the separation between the lens halves
//   alpha is the angle of the cylinder wrt q.
float _cap_kernel(float q, float h, float cap_radius, float length,
                 float sin_alpha, float cos_alpha);
float _cap_kernel(float q, float h, float cap_radius, float length,
                 float sin_alpha, float cos_alpha)
{
    // For speed, we are pre-calculating terms which are constant over the
    // kernel.
    const float upper = 1.0f;
    const float lower = h/cap_radius; // integral lower bound
    // cos term in integral is:
    //    cos (q (R t - h + L/2) cos(alpha))
    // so turn it into:
    //    cos (m t + b)
    // where:
    //    m = q R cos(alpha)
    //    b = q(L/2-h) cos(alpha)
    const float m = q*cap_radius*cos_alpha; // cos argument slope
    const float b = q*(0.5f*length-h)*cos_alpha; // cos argument intercept
    const float qrst = q*cap_radius*sin_alpha; // Q*R*sin(theta)
    float total = 0.0f;
    for (int i=0; i<76 ;i++) {
        // translate a point in [-1,1] to a point in [lower,upper]
        //const float t = ( Gauss76Z[i]*(upper-lower) + upper + lower )/2.0f;
        const float t = 0.5f*(Gauss76Z[i]*(upper-lower)+upper+lower);
        const float radical = 1.0f - t*t;
        const float arg = qrst*sqrt(radical); // cap bessel function arg
        const float be = (arg == 0.0f ? 0.5f : J1(arg)/arg);
        const float Fq = cos(m*t + b) * radical * be;
        total += Gauss76Wt[i] * Fq;
    }
    // translate dx in [-1,1] to dx in [lower,upper]
    //const float form = (upper-lower)/2.0f*total;
    const float integral = 0.5f*(upper-lower)*total;
    return 4.0f*M_PI*cap_radius*cap_radius*cap_radius*integral;
}

float form_volume(float radius, float cap_radius, float length)
{
    // cap radius should never be less than radius when this is called

    // Note: volume V = 2*V_cap + V_cyl
    //
    // V_cyl = pi r_cyl^2 L
    // V_cap = 1/6 pi h_c (3 r_cyl^2 + h_c^2) = 1/3 pi h_c^2 (3 r_cap - h_c)
    //
    // The docs for capped cylinder give the volume as:
    //    V = pi r^2 L + 2/3 pi (R-h)^2 (2R + h)
    // where r_cap=R and h = R - h_c.
    //
    // The first part is clearly V_cyl.  The second part requires some work:
    //    (R-h)^2 => h_c^2
    //    (2R+h) => 2R+ h_c-h_c + h => 2R + (R-h)-hc + h => 3R-h_c
    // And so:
    //    2/3 pi (R-h)^2 (2R + h) => 2/3 pi h_c^2 (3 r_cap - h_c)
    // which is 2 V_cap, using the second form above.
    //
    // In this function we are going to use the first form of V_cap
    //
    //      V = V_cyl + 2 V_cap
    //        = pi r^2 L + pi hc (r^2 + hc^2/3)
    //        = pi (r^2 (L+hc) + hc^3/3)
    const float hc = cap_radius - sqrt(cap_radius*cap_radius - radius*radius);
    return M_PI*(radius*radius*(length+hc) + 0.333333333333333f*hc*hc*hc);
}

float Iq(float q,
    float sld,
    float solvent_sld,
    float radius,
    float cap_radius,
    float length)
{
    float sn, cn; // slots to hold sincos function output

    // Exclude invalid inputs.
    if (cap_radius < radius) return -1.0f;

    const float lower = 0.0f;
    const float upper = M_PI_2;
    const float h = sqrt(cap_radius*cap_radius - radius*radius); // negative h
    float total = 0.0f;
    for (int i=0; i<76 ;i++) {
        // translate a point in [-1,1] to a point in [lower,upper]
        const float alpha= 0.5f*(Gauss76Z[i]*(upper-lower) + upper + lower);
        SINCOS(alpha, sn, cn);

        const float cap_Fq = _cap_kernel(q, h, cap_radius, length, sn, cn);

        // The following is CylKernel() / sin(alpha), but we are doing it in place
        // to avoid sin(alpha)/sin(alpha) for alpha = 0.f  It is also a teensy bit
        // faster since we don't multiply and divide sin(alpha).
        const float besarg = q*radius*sn;
        const float siarg = q*0.5f*length*cn;
        // lim_{x->0} J1(x)/x = 1/2,   lim_{x->0} sin(x)/x = 1
        const float bj = (besarg == 0.0f ? 0.5f : J1(besarg)/besarg);
        const float si = (siarg == 0.0f ? 1.0f : sin(siarg)/siarg);
        const float cyl_Fq = M_PI*radius*radius*length*2.0f*bj*si;

        // Volume weighted average F(q)
        const float Aq = cyl_Fq + cap_Fq;
        total += Gauss76Wt[i] * Aq * Aq * sn; // sn for spherical coord integration
    }
    // translate dx in [-1,1] to dx in [lower,upper]
    const float form = total * 0.5f*(upper-lower);

    // Multiply by contrast^2, normalize by cylinder volume and convert to cm-1
    // NOTE that for this (Fournet) definition of the integral, one must MULTIPLY by Vcyl
    // The additional volume factor is for polydisperse volume normalization.
    const float s = (sld - solvent_sld);
    return 1.0e-4f * form * s * s; // form_volume(radius, cap_radius, length);
}


float Iqxy(float qx, float qy,
    float sld,
    float solvent_sld,
    float radius,
    float cap_radius,
    float length,
    float theta,
    float phi)
{
    float sn, cn; // slots to hold sincos function output

    // Exclude invalid inputs.
    if (cap_radius < radius) return -1.0f;

    // Compute angle alpha between q and the cylinder axis
    SINCOS(theta*M_PI_180, sn, cn);
    // # The following correction factor exists in sasview, but it can't be
    // # right, so we are leaving it out for now.
    const float q = sqrt(qx*qx+qy*qy);
    const float cos_val = cn*cos(phi*M_PI_180)*(qx/q) + sn*(qy/q);
    const float alpha = acos(cos_val); // rod angle relative to q
    SINCOS(alpha, sn, cn);

    const float h = sqrt(cap_radius*cap_radius - radius*radius); // negative h
    const float cap_Fq = _cap_kernel(q, h, cap_radius, length, sn, cn);

    const float besarg = q*radius*sn;
    const float siarg = q*0.5f*length*cn;
    // lim_{x->0} J1(x)/x = 1/2,   lim_{x->0} sin(x)/x = 1
    const float bj = (besarg == 0.0f ? 0.5f : J1(besarg)/besarg);
    const float si = (siarg == 0.0f ? 1.0f : sin(siarg)/siarg);
    const float cyl_Fq = M_PI*radius*radius*length*2.0f*bj*si;

    // Volume weighted average F(q)
    const float Aq = cyl_Fq + cap_Fq;

    // Multiply by contrast^2, normalize by cylinder volume and convert to cm-1
    const float s = (sld - solvent_sld);
    return 1.0e-4f * Aq * Aq * s * s; // form_volume(radius, cap_radius, length);
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
