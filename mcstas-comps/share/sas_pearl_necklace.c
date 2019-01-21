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


#define VOLUME_PARAMETERS radius,edge_separation,string_thickness,number_of_pearls
#define VOLUME_WEIGHT_PRODUCT radius_w*edge_separation_w*string_thickness_w*number_of_pearls_w
#define IQ_KERNEL_NAME pearl_necklace_Iq
#define IQ_PARAMETERS radius, edge_separation, string_thickness, number_of_pearls, sld, string_sld, solvent_sld
#define IQ_FIXED_PARAMETER_DECLARATIONS const float scale, \
    const float background, \
    const float sld, \
    const float string_sld, \
    const float solvent_sld
#define IQ_WEIGHT_PRODUCT radius_w*edge_separation_w*string_thickness_w*number_of_pearls_w
#define IQ_DISPERSION_LENGTH_DECLARATIONS const int Nradius, \
    const int Nedge_separation, \
    const int Nstring_thickness, \
    const int Nnumber_of_pearls
#define IQ_DISPERSION_LENGTH_SUM Nradius+Nedge_separation+Nstring_thickness+Nnumber_of_pearls
#define IQ_OPEN_LOOPS     for (int radius_i=0; radius_i < Nradius; radius_i++) { \
      const float radius = loops[2*(radius_i)]; \
      const float radius_w = loops[2*(radius_i)+1]; \
      for (int edge_separation_i=0; edge_separation_i < Nedge_separation; edge_separation_i++) { \
        const float edge_separation = loops[2*(edge_separation_i+Nradius)]; \
        const float edge_separation_w = loops[2*(edge_separation_i+Nradius)+1]; \
        for (int string_thickness_i=0; string_thickness_i < Nstring_thickness; string_thickness_i++) { \
          const float string_thickness = loops[2*(string_thickness_i+Nradius+Nedge_separation)]; \
          const float string_thickness_w = loops[2*(string_thickness_i+Nradius+Nedge_separation)+1]; \
          for (int number_of_pearls_i=0; number_of_pearls_i < Nnumber_of_pearls; number_of_pearls_i++) { \
            const float number_of_pearls = loops[2*(number_of_pearls_i+Nradius+Nedge_separation+Nstring_thickness)]; \
            const float number_of_pearls_w = loops[2*(number_of_pearls_i+Nradius+Nedge_separation+Nstring_thickness)+1];
#define IQ_CLOSE_LOOPS           } \
        } \
      } \
    }
#define IQXY_KERNEL_NAME pearl_necklace_Iqxy
#define IQXY_PARAMETERS radius, edge_separation, string_thickness, number_of_pearls, sld, string_sld, solvent_sld
#define IQXY_FIXED_PARAMETER_DECLARATIONS const float scale, \
    const float background, \
    const float sld, \
    const float string_sld, \
    const float solvent_sld
#define IQXY_WEIGHT_PRODUCT radius_w*edge_separation_w*string_thickness_w*number_of_pearls_w
#define IQXY_DISPERSION_LENGTH_DECLARATIONS const int Nradius, \
    const int Nedge_separation, \
    const int Nstring_thickness, \
    const int Nnumber_of_pearls
#define IQXY_DISPERSION_LENGTH_SUM Nradius+Nedge_separation+Nstring_thickness+Nnumber_of_pearls
#define IQXY_OPEN_LOOPS     for (int radius_i=0; radius_i < Nradius; radius_i++) { \
      const float radius = loops[2*(radius_i)]; \
      const float radius_w = loops[2*(radius_i)+1]; \
      for (int edge_separation_i=0; edge_separation_i < Nedge_separation; edge_separation_i++) { \
        const float edge_separation = loops[2*(edge_separation_i+Nradius)]; \
        const float edge_separation_w = loops[2*(edge_separation_i+Nradius)+1]; \
        for (int string_thickness_i=0; string_thickness_i < Nstring_thickness; string_thickness_i++) { \
          const float string_thickness = loops[2*(string_thickness_i+Nradius+Nedge_separation)]; \
          const float string_thickness_w = loops[2*(string_thickness_i+Nradius+Nedge_separation)+1]; \
          for (int number_of_pearls_i=0; number_of_pearls_i < Nnumber_of_pearls; number_of_pearls_i++) { \
            const float number_of_pearls = loops[2*(number_of_pearls_i+Nradius+Nedge_separation+Nstring_thickness)]; \
            const float number_of_pearls_w = loops[2*(number_of_pearls_i+Nradius+Nedge_separation+Nstring_thickness)+1];
#define IQXY_CLOSE_LOOPS           } \
        } \
      } \
    }

float Si(float x);

// integral of sin(x)/x Taylor series approximated to w/i 0.1f%
float Si(float x)
{
	float out;
	float pi = 4.0f*atan(1.0f);

	if (x >= pi*6.2f/4.0f){
		float out_sin = 0.0f;
		float out_cos = 0.0f;
		out = pi/2.0f;

		// Explicitly writing factorial values triples the speed of the calculation
		out_cos = 1/x - 2/pow(x,3) + 24/pow(x,5) - 720/pow(x,7) + 40320/pow(x,9);
		out_sin = 1/x - 6/pow(x,4) + 120/pow(x,6) - 5040/pow(x,8) + 362880/pow(x,10);

		out -= cos(x) * out_cos;
		out -= sin(x) * out_sin;
		return out;
	}

	// Explicitly writing factorial values triples the speed of the calculation
	out = x - pow(x, 3)/18 + pow(x,5)/600 - pow(x,7)/35280 + pow(x,9)/3265920;

	//printf ("Si=%g %g\n", x, out);
	return out;
}

float _pearl_necklace_kernel(float q, float radius, float edge_separation,
	float thick_string, float num_pearls, float sld_pearl,
	float sld_string, float sld_solv);
float form_volume(float radius, float edge_separation,
	float string_thickness, float number_of_pearls);
float sinc(float x);
	
float Iq(float q, float radius, float edge_separation,
	float string_thickness, float number_of_pearls, float sld, 
	float string_sld, float solvent_sld);
float Iqxy(float qx, float qy, float radius, float edge_separation,
	float string_thickness, float number_of_pearls, float sld, 
	float string_sld, float solvent_sld);
	
float ER(float radius, float edge_separation,
	float string_thickness, float number_of_pearls);
float VR(float radius, float edge_separation,
	float string_thickness, float number_of_pearls);

// From Igor library
float _pearl_necklace_kernel(float q, float radius, float edge_separation, float thick_string,
	float num_pearls, float sld_pearl, float sld_string, float sld_solv)
{
	float contrast_pearl = sld_pearl - sld_solv;
	float contrast_string = sld_string - sld_solv;

	//total volume
	float pi = 4.0f*atan(1.0f);
	float tot_vol = form_volume(radius, edge_separation, thick_string, num_pearls);
	float string_vol = edge_separation * pi * pow((thick_string / 2.0f), 2);
	float pearl_vol = 4.0f /3.0f * pi * pow(radius, 3);
	float num_strings = num_pearls - 1;
	//each mass
	float m_r= contrast_string * string_vol;
	float m_s= contrast_pearl * pearl_vol;
	float psi, gamma, beta;
	//form factors
	float sss; //pearls
	float srr; //strings
	float srs; //cross
	float A_s, srr_1, srr_2, srr_3;
	float form_factor;

	//sine functions of a pearl
	psi = sin(q*radius);
	psi -= q * radius * cos(q * radius);
	psi /= pow((q * radius), 3);
	psi *= 3.0f;

	// Note take only 20 terms in Si series: 10 terms may be enough though.
	gamma = Si(q* edge_separation);
	gamma /= (q* edge_separation);
	beta = Si(q * (edge_separation + radius));
	beta -= Si(q * radius);
	beta /= (q* edge_separation);

	// center to center distance between the neighboring pearls
	A_s = edge_separation + 2.0f * radius;

	// form factor for num_pearls
	sss = 1.0f - pow(sinc(q*A_s), num_pearls );
	sss /= pow((1.0f-sinc(q*A_s)), 2);
	sss *= -sinc(q*A_s);
	sss -= num_pearls/2.0f;
	sss += num_pearls/(1.0f-sinc(q*A_s));
	sss *= 2.0f * pow((m_s*psi), 2);

	// form factor for num_strings (like thin rods)
	srr_1 = -pow(sinc(q*edge_separation/2.0f), 2);

	srr_1 += 2.0f * gamma;
	srr_1 *= num_strings;
	srr_2 = 2.0f/(1.0f-sinc(q*A_s));
	srr_2 *= num_strings * pow(beta, 2);
	srr_3 = 1.0f - pow(sinc(q*A_s), num_strings);
	srr_3 /= pow((1.0f-sinc(q*A_s)), 2);
	srr_3 *= -2.0f * pow(beta, 2);

	// total srr
	srr = srr_1 + srr_2 + srr_3;
	srr *= pow(m_r, 2);

	// form factor for correlations
	srs = 1.0f;
	srs -= pow(sinc(q*A_s), num_strings);
	srs /= pow((1.0f-sinc(q*A_s)), 2);
	srs *= -sinc(q*A_s);
	srs += (num_strings/(1.0f-sinc(q*A_s)));
	srs *= 4.0f * (m_r * m_s * beta * psi);

	form_factor = sss + srr + srs;
	form_factor /= (tot_vol * 1.0e4f); // norm by volume and A^-1 to cm^-1

	return (form_factor);
}

float form_volume(float radius, float edge_separation,
	float string_thickness, float number_of_pearls)
{
	float total_vol;

	float pi = 4.0f*atan(1.0f);
	float number_of_strings = number_of_pearls - 1.0f;
	
	float string_vol = edge_separation * pi * pow((string_thickness / 2.0f), 2);
	float pearl_vol = 4.0f /3.0f * pi * pow(radius, 3);

	total_vol = number_of_strings * string_vol;
	total_vol += number_of_pearls * pearl_vol;

	return(total_vol);
}

float sinc(float x)
{
	float num = sin(x);
	float denom = x;
	return num/denom;
}


float Iq(float q, float radius, float edge_separation,
	float string_thickness, float number_of_pearls, float sld, 
	float string_sld, float solvent_sld)
{
	float value = 0.0f;
	float tot_vol = 0.0f;
	
	value = _pearl_necklace_kernel(q, radius, edge_separation, string_thickness,
		number_of_pearls, sld, string_sld, solvent_sld);
	tot_vol = form_volume(radius, edge_separation, string_thickness, number_of_pearls);

	return value*tot_vol;
}

float Iqxy(float qx, float qy, float radius, float edge_separation,
	float string_thickness, float number_of_pearls, float sld, 
	float string_sld, float solvent_sld)
{
    float q = sqrt(qx*qx + qy*qy);
    return(Iq(q, radius, edge_separation, string_thickness, number_of_pearls, 
		sld, string_sld, solvent_sld));
}


float ER(float radius, float edge_separation,
	float string_thickness, float number_of_pearls)
{
	float tot_vol = form_volume(radius, edge_separation, string_thickness, number_of_pearls);
	float pi = 4.0f*atan(1.0f);

    float rad_out = pow((3.0f*tot_vol/4.0f/pi), 0.33333f);
    
    return rad_out;
}
float VR(float radius, float edge_separation,
	float string_thickness, float number_of_pearls)
{
	return 1.0f;
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
