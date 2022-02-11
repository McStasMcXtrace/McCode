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


#define VOLUME_PARAMETERS length,kuhn_length,radius
#define VOLUME_WEIGHT_PRODUCT length_w*kuhn_length_w*radius_w
#define IQ_KERNEL_NAME flexible_cylinder_ex_Iq
#define IQ_PARAMETERS length, kuhn_length, radius, axis_ratio, sld, solvent_sld
#define IQ_FIXED_PARAMETER_DECLARATIONS const float scale, \
    const float background, \
    const float axis_ratio, \
    const float sld, \
    const float solvent_sld
#define IQ_WEIGHT_PRODUCT length_w*kuhn_length_w*radius_w
#define IQ_DISPERSION_LENGTH_DECLARATIONS const int Nlength, \
    const int Nkuhn_length, \
    const int Nradius
#define IQ_DISPERSION_LENGTH_SUM Nlength+Nkuhn_length+Nradius
#define IQ_OPEN_LOOPS     for (int length_i=0; length_i < Nlength; length_i++) { \
      const float length = loops[2*(length_i)]; \
      const float length_w = loops[2*(length_i)+1]; \
      for (int kuhn_length_i=0; kuhn_length_i < Nkuhn_length; kuhn_length_i++) { \
        const float kuhn_length = loops[2*(kuhn_length_i+Nlength)]; \
        const float kuhn_length_w = loops[2*(kuhn_length_i+Nlength)+1]; \
        for (int radius_i=0; radius_i < Nradius; radius_i++) { \
          const float radius = loops[2*(radius_i+Nlength+Nkuhn_length)]; \
          const float radius_w = loops[2*(radius_i+Nlength+Nkuhn_length)+1];
#define IQ_CLOSE_LOOPS         } \
      } \
    }
#define IQXY_KERNEL_NAME flexible_cylinder_ex_Iqxy
#define IQXY_PARAMETERS length, kuhn_length, radius, axis_ratio, sld, solvent_sld
#define IQXY_FIXED_PARAMETER_DECLARATIONS const float scale, \
    const float background, \
    const float axis_ratio, \
    const float sld, \
    const float solvent_sld
#define IQXY_WEIGHT_PRODUCT length_w*kuhn_length_w*radius_w
#define IQXY_DISPERSION_LENGTH_DECLARATIONS const int Nlength, \
    const int Nkuhn_length, \
    const int Nradius
#define IQXY_DISPERSION_LENGTH_SUM Nlength+Nkuhn_length+Nradius
#define IQXY_OPEN_LOOPS     for (int length_i=0; length_i < Nlength; length_i++) { \
      const float length = loops[2*(length_i)]; \
      const float length_w = loops[2*(length_i)+1]; \
      for (int kuhn_length_i=0; kuhn_length_i < Nkuhn_length; kuhn_length_i++) { \
        const float kuhn_length = loops[2*(kuhn_length_i+Nlength)]; \
        const float kuhn_length_w = loops[2*(kuhn_length_i+Nlength)+1]; \
        for (int radius_i=0; radius_i < Nradius; radius_i++) { \
          const float radius = loops[2*(radius_i+Nlength+Nkuhn_length)]; \
          const float radius_w = loops[2*(radius_i+Nlength+Nkuhn_length)+1];
#define IQXY_CLOSE_LOOPS         } \
      } \
    }

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
constant float Gauss76Wt[]={
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

#pragma acc declare create ( Gauss76Wt )

constant float Gauss76Z[]={
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

#pragma acc declare create ( Gauss76Z )

/*
    Functions for WRC implementation of flexible cylinders
*/
static float
AlphaSquare(float x)
{
    return pow( (1.0f + (x/3.12f)*(x/3.12f) +
         (x/8.67f)*(x/8.67f)*(x/8.67f)),(0.176f/3.0f) );
}

//
static float
Rgsquarezero(float q, float L, float b)
{
    return (L*b/6.0f) * (1.0f - 1.5f*(b/L) + 1.5f*pow((b/L),2) -
         0.75f*pow((b/L),3)*(1.0f - exp(-2.0f*(L/b))));
}

//
static float
Rgsquareshort(float q, float L, float b)
{
    return AlphaSquare(L/b) * Rgsquarezero(q,L,b);
}

//
static float
Rgsquare(float q, float L, float b)
{
    return AlphaSquare(L/b)*L*b/6.0f;
}

static float
sech_WR(float x)
{
    return(1/cosh(x));
}

static float
a1long(float q, float L, float b, float p1, float p2, float q0)
{
    float yy,C,C1,C2,C3,C4,C5,miu,Rg2;
    float t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15;
    float E,pi;
    float b2,b3,b4,q02,q03,q04,q05,Rg22;

    pi = 4.0f*atan(1.0f);
    E = 2.718281828459045091f;

    if( L/b > 10.0f) {
        C = 3.06f/pow((L/b),0.44f);
    } else {
        C = 1.0f;
    }

    C1 = 1.22f;
    C2 = 0.4288f;
    C3 = -1.651f;
    C4 = 1.523f;
    C5 = 0.1477f;
    miu = 0.585f;

    Rg2 = Rgsquare(q,L,b);
    Rg22 = Rg2*Rg2;
    b2 = b*b;
    b3 = b*b*b;
    b4 = b3*b;
    q02 = q0*q0;
    q03 = q0*q0*q0;
    q04 = q03*q0;
    q05 = q04*q0;

    t1 = (b*C*((4.0f/15.0f - pow(E,(-((q02*Rg2)/b2)))*((11.0f/15.0f +
        (7.0f*b2)/(15.0f*q02*Rg2))) + (7.0f*b2)/(15.0f*q02*Rg2))));

    t2 = (2.0f*b4*(((-1.0f) + pow(E,(-((q02*Rg2)/b2))) +
        (q02*Rg2)/b2))*((1.0f + 1.0f/2.0f*(((-1.0f) -
        tanh(((-C4) + (sqrt(Rg2)*q0)/b)/C5))))));

    t3 = ((C3*pow((((sqrt(Rg2)*q0)/b)),((-3.0f)/miu)) +
        C2*pow((((sqrt(Rg2)*q0)/b)),((-2.0f)/miu)) +
        C1*pow((((sqrt(Rg2)*q0)/b)),((-1.0f)/miu))));

    t4 = ((1.0f + tanh(((-C4) + (sqrt(Rg2)*q0)/b)/C5)));

    t5 = (1.0f/(b*p1*pow(q0,((-1.0f) - p1 - p2)) -
        b*p2*pow(q0,((-1.0f) - p1 - p2))));

    t6 = (b*C*(((-((14.0f*b3)/(15.0f*q03*Rg2))) +
        (14.0f*b3*pow(E,(-((q02*Rg2)/b2))))/(15.0f*q03*Rg2) +
        (2.0f*pow(E,(-((q02*Rg2)/b2)))*q0*((11.0f/15.0f +
        (7.0f*b2)/(15.0f*q02*Rg2)))*Rg2)/b)));

    t7 = (sqrt(Rg2)*((C3*pow((((sqrt(Rg2)*q0)/b)),((-3.0f)/miu)) +
        C2*pow((((sqrt(Rg2)*q0)/b)),((-2.0f)/miu)) +
        C1*pow((((sqrt(Rg2)*q0)/b)),((-1.0f)/miu))))*pow(sech_WR(((-C4) +
        (sqrt(Rg2)*q0)/b)/C5),2));

    t8 = (b4*sqrt(Rg2)*(((-1.0f) + pow(E,(-((q02*Rg2)/b2))) +
        (q02*Rg2)/b2))*pow(sech_WR(((-C4) + (sqrt(Rg2)*q0)/b)/C5),2));

    t9 = (2.0f*b4*(((2.0f*q0*Rg2)/b -
        (2.0f*pow(E,(-((q02*Rg2)/b2)))*q0*Rg2)/b))*((1.0f + 1.0f/2.0f*(((-1.0f) -
        tanh(((-C4) + (sqrt(Rg2)*q0)/b)/C5))))));

    t10 = (8.0f*b4*b*(((-1.0f) + pow(E,(-((q02*Rg2)/b2))) +
        (q02*Rg2)/b2))*((1.0f + 1.0f/2.0f*(((-1.0f) - tanh(((-C4) +
        (sqrt(Rg2)*q0)/b)/C5))))));

    t11 = (((-((3.0f*C3*sqrt(Rg2)*pow((((sqrt(Rg2)*q0)/b)),((-1.0f) -
        3.0f/miu)))/miu)) - (2.0f*C2*sqrt(Rg2)*pow((((sqrt(Rg2)*q0)/b)),((-1.0f) -
        2.0f/miu)))/miu - (C1*sqrt(Rg2)*pow((((sqrt(Rg2)*q0)/b)),((-1.0f) -
        1.0f/miu)))/miu));

    t12 = ((1.0f + tanh(((-C4) + (sqrt(Rg2)*q0)/b)/C5)));

    t13 = (b*C*((4.0f/15.0f - pow(E,(-((q02*Rg2)/b2)))*((11.0f/15.0f +
        (7.0f*b2)/(15.0f*q02* Rg2))) +
        (7.0f*b2)/(15.0f*q02*Rg2))));

    t14 = (2.0f*b4*(((-1.0f) + pow(E,(-((q02*Rg2)/b2))) +
        (q02*Rg2)/b2))*((1.0f + 1.0f/2.0f*(((-1.0f) - tanh(((-C4) +
        (sqrt(Rg2)*q0)/b)/C5))))));

    t15 = ((C3*pow((((sqrt(Rg2)*q0)/b)),((-3.0f)/miu)) +
        C2*pow((((sqrt(Rg2)*q0)/b)),((-2.0f)/miu)) +
        C1*pow((((sqrt(Rg2)*q0)/b)),((-1.0f)/miu))));


    yy = (pow(q0,p1)*(((-((b*pi)/(L*q0))) +t1/L +t2/(q04*Rg22) +
        1.0f/2.0f*t3*t4)) + (t5*((pow(q0,(p1 - p2))*
        (((-pow(q0,(-p1)))*(((b2*pi)/(L*q02) +t6/L +t7/(2.0f*C5) -
        t8/(C5*q04*Rg22) + t9/(q04*Rg22) -t10/(q05*Rg22) + 1.0f/2.0f*t11*t12)) -
        b*p1*pow(q0,((-1.0f) - p1))*(((-((b*pi)/(L*q0))) + t13/L +
        t14/(q04*Rg22) + 1.0f/2.0f*t15*((1.0f + tanh(((-C4) +
        (sqrt(Rg2)*q0)/b)/C5)))))))))));

    return (yy);
}

static float
a2long(float q, float L, float b, float p1, float p2, float q0)
{
    float yy,C1,C2,C3,C4,C5,miu,C,Rg2;
    float t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,pi;
    float E,b2,b3,b4,q02,q03,q04,q05,Rg22;

    pi = 4.0f*atan(1.0f);
    E = 2.718281828459045091f;
    if( L/b > 10.0f) {
        C = 3.06f/pow((L/b),0.44f);
    } else {
        C = 1.0f;
    }

    C1 = 1.22f;
    C2 = 0.4288f;
    C3 = -1.651f;
    C4 = 1.523f;
    C5 = 0.1477f;
    miu = 0.585f;

    Rg2 = Rgsquare(q,L,b);
    Rg22 = Rg2*Rg2;
    b2 = b*b;
    b3 = b*b*b;
    b4 = b3*b;
    q02 = q0*q0;
    q03 = q0*q0*q0;
    q04 = q03*q0;
    q05 = q04*q0;

    t1 = (1.0f/(b* p1*pow(q0,((-1.0f) - p1 - p2)) -
        b*p2*pow(q0,((-1.0f) - p1 - p2)) ));

    t2 = (b*C*(((-1.0f*((14.0f*b3)/(15.0f*q03*Rg2))) +
        (14.0f*b3*pow(E,(-((q02*Rg2)/b2))))/(15.0f*q03*Rg2) +
        (2.0f*pow(E,(-((q02*Rg2)/b2)))*q0*((11.0f/15.0f +
        (7*b2)/(15.0f*q02*Rg2)))*Rg2)/b)))/L;

    t3 = (sqrt(Rg2)*((C3*pow((((sqrt(Rg2)*q0)/b)),((-3.0f)/miu)) +
        C2*pow((((sqrt(Rg2)*q0)/b)),((-2.0f)/miu)) +
        C1*pow((((sqrt(Rg2)*q0)/b)),((-1.0f)/miu))))*
        pow(sech_WR(((-C4) +(sqrt(Rg2)*q0)/b)/C5),2.0f))/(2.0f*C5);

    t4 = (b4*sqrt(Rg2)*(((-1.0f) + pow(E,(-((q02*Rg2)/b2))) +
        (q02*Rg2)/b2))*pow(sech_WR(((-C4) +
        (sqrt(Rg2)*q0)/b)/C5),2))/(C5*q04*Rg22);

    t5 = (2.0f*b4*(((2.0f*q0*Rg2)/b -
        (2.0f*pow(E,(-((q02*Rg2)/b2)))*q0*Rg2)/b))*
        ((1.0f + 1.0f/2.0f*(((-1.0f) - tanh(((-C4) +
        (sqrt(Rg2)*q0)/b)/C5))))))/(q04*Rg22);

    t6 = (8.0f*b4*b*(((-1.0f) + pow(E,(-((q02*Rg2)/b2))) +
        (q02*Rg2)/b2))*((1.0f + 1.0f/2.0f*(((-1) - tanh(((-C4) +
        (sqrt(Rg2)*q0)/b)/C5))))))/(q05*Rg22);

    t7 = (((-((3.0f*C3*sqrt(Rg2)*pow((((sqrt(Rg2)*q0)/b)),((-1.0f) -
        3.0f/miu)))/miu)) - (2.0f*C2*sqrt(Rg2)*pow((((sqrt(Rg2)*q0)/b)),
        ((-1.0f) - 2.0f/miu)))/miu - (C1*sqrt(Rg2)*pow((((sqrt(Rg2)*q0)/b)),
        ((-1.0f) - 1.0f/miu)))/miu));

    t8 = ((1.0f + tanh(((-C4) + (sqrt(Rg2)*q0)/b)/C5)));

    t9 = (b*C*((4.0f/15.0f - pow(E,(-((q02*Rg2)/b2)))*((11.0f/15.0f +
        (7.0f    *b2)/(15*q02*Rg2))) + (7.0f*b2)/(15.0f*q02*Rg2))))/L;

    t10 = (2.0f*b4*(((-1) + pow(E,(-((q02*Rg2)/b2))) +
        (q02*Rg2)/b2))*((1.0f + 1.0f/2.0f*(((-1) - tanh(((-C4) +
        (sqrt(Rg2)*q0)/b)/C5))))))/(q04*Rg22);

    yy = ((-1.0f*(t1* ((-pow(q0,-p1)*(((b2*pi)/(L*q02) +
        t2 + t3 - t4 + t5 - t6 + 1.0f/2.0f*t7*t8)) - b*p1*pow(q0,((-1.0f) - p1))*
        (((-((b*pi)/(L*q0))) + t9 + t10 +
        1.0f/2.0f*((C3*pow((((sqrt(Rg2)*q0)/b)),((-3.0f)/miu)) +
        C2*pow((((sqrt(Rg2)*q0)/b)),((-2.0f)/miu)) +
        C1*pow((((sqrt(Rg2)*q0)/b)),((-1.0f)/miu))))*
        ((1.0f + tanh(((-C4) + (sqrt(Rg2)*q0)/b)/C5))))))))));

    return (yy);
}

//
static float
a1short(float q, float L, float b, float p1short, float p2short, float q0)
{
    float yy,Rg2_sh;
    float t1,E,Rg2_sh2,Et1,Emt1,q02,q0p,b3;
    float pi;

    E = 2.718281828459045091f;
    pi = 4.0f*atan(1.0f);
    Rg2_sh = Rgsquareshort(q,L,b);
    Rg2_sh2 = Rg2_sh*Rg2_sh;
    b3 = b*b*b;
    t1 = ((q0*q0*Rg2_sh)/(b*b));
    Et1 = pow(E,t1);
    Emt1 =pow(E,-t1);
    q02 = q0*q0;
    q0p = pow(q0,(-4.0f + p1short) );

    yy = ((1.0f/(L*((p1short - p2short))*Rg2_sh2)*
        ((b*Emt1*q0p*((8.0f*b3*L - 8.0f*b3*Et1*L - 2.0f*b3*L*p2short +
        2.0f*b3*Et1*L*p2short + 4.0f*b*L*q02*Rg2_sh + 4.0f*b*Et1*L*q02*Rg2_sh -
        2.0f*b*Et1*L*p2short*q02*Rg2_sh - Et1*pi*q02*q0*Rg2_sh2 +
        Et1*p2short*pi*q02*q0*Rg2_sh2))))));

    return(yy);
}

static float
a2short(float q, float L, float b, float p1short, float p2short, float q0)
{
    float yy,Rg2_sh;
    float t1,E,Rg2_sh2,Et1,Emt1,q02,q0p;
    float pi;

    E = 2.718281828459045091f;
    pi = 4.0f*atan(1.0f);
    Rg2_sh = Rgsquareshort(q,L,b);
    Rg2_sh2 = Rg2_sh*Rg2_sh;
    t1 = ((q0*q0*Rg2_sh)/(b*b));
    Et1 = pow(E,t1);
    Emt1 =pow(E,-t1);
    q02 = q0*q0;
    q0p = pow(q0,(-4.0f + p2short) );

    //E is the number e
    yy = ((-(1.0f/(L*((p1short - p2short))*Rg2_sh2)*
        ((b*Emt1*q0p*((8.0f*b*b*b*L - 8.0f*b*b*b*Et1*L - 2.0f*b*b*b*L*p1short +
        2.0f*b*b*b*Et1*L*p1short + 4.0f*b*L*q02*Rg2_sh + 4.0f*b*Et1*L*q02*Rg2_sh -
        2.0f*b*Et1*L*p1short*q02*Rg2_sh - Et1*pi*q02*q0*Rg2_sh2 +
        Et1*p1short*pi*q02*q0*Rg2_sh2)))))));

    return (yy);
}

//WR named this w (too generic)
static float
w_WR(float x)
{
    return 0.5f*(1 + tanh((x - 1.523f)/0.1477f));
}

//
static float
u1(float q, float L, float b)
{
    return Rgsquareshort(q,L,b)*q*q;
}

static float
u_WR(float q, float L, float b)
{
    return Rgsquare(q,L,b)*q*q;
}

static float
Sdebye_kernel(float arg)
{
    // ORIGINAL
    float result = 2.0f*(exp(-arg) + arg -1.0f)/(pow((arg),2));

    // CONVERSION 1 from http://herbie.uwplse.org/
    //
    // exhibits discontinuity - needs more investigation
    //float a1 = 1.0f/6.0f;
    //float a2 = 1.0f/72.0f;
    //float a3 = 1.0f/24.0f;
    //float result = pow((1.0f - a1*arg - (a2+a3)*arg*arg), 2);

    return result;
}
static float
Sdebye(float q, float L, float b)
{
    float arg = u_WR(q,L,b);
    return Sdebye_kernel(arg);
}

//
static float
Sdebye1(float q, float L, float b)
{
    float arg = u1(q,L,b);
    return Sdebye_kernel(arg);

}

//
static float
Sexv(float q, float L, float b)
{
    float yy,C1,C2,C3,miu,Rg2;
    C1=1.22f;
    C2=0.4288f;
    C3=-1.651f;
    miu = 0.585f;

    Rg2 = Rgsquare(q,L,b);

    yy = (1.0f - w_WR(q*sqrt(Rg2)))*Sdebye(q,L,b) +
        w_WR(q*sqrt(Rg2))*(C1*pow((q*sqrt(Rg2)),(-1.0f/miu)) +
        C2*pow((q*sqrt(Rg2)),(-2.0f/miu)) +
        C3*pow((q*sqrt(Rg2)),(-3.0f/miu)));

    return (yy);
}


static float
Sexvnew(float q, float L, float b)
{
    float yy,C1,C2,C3,miu;
    float del=1.05f,C_star2,Rg2;

    C1=1.22f;
    C2=0.4288f;
    C3=-1.651f;
    miu = 0.585f;

    //calculating the derivative to decide on the corection (cutoff) term?
    // I have modified this from WRs original code

    if( (Sexv(q*del,L,b)-Sexv(q,L,b))/(q*del - q) >= 0.0f ) {
        C_star2 = 0.0f;
    } else {
        C_star2 = 1.0f;
    }

    Rg2 = Rgsquare(q,L,b);

    yy = (1.0f - w_WR(q*sqrt(Rg2)))*Sdebye(q,L,b) +
        C_star2*w_WR(q*sqrt(Rg2))*(C1*pow((q*sqrt(Rg2)),(-1.0f/miu)) +
        C2*pow((q*sqrt(Rg2)),(-2.0f/miu)) + C3*pow((q*sqrt(Rg2)),(-3.0f/miu)));

    return (yy);
}

float Sk_WR(float q, float L, float b)
{
  //
  float p1,p2,p1short,p2short,q0;
  float C,ans,q0short,Sexvmodify,pi;

  pi = 4.0f*atan(1.0f);

  p1 = 4.12f;
  p2 = 4.42f;
  p1short = 5.36f;
  p2short = 5.62f;
  q0 = 3.1f;

  q0short = fmax(1.9f/sqrt(Rgsquareshort(q,L,b)),3.0f);


  if(L/b > 10.0f) {
    C = 3.06f/pow((L/b),0.44f);
  } else {
    C = 1.0f;
  }
  //

  //
  if( L > 4*b ) { // Longer Chains
    if (q*b <= 3.1f) {   //Modified by Yun on Oct. 15,

      Sexvmodify = Sexvnew(q, L, b);

      ans = Sexvmodify + C * (4.0f/15.0f + 7.0f/(15.0f*u_WR(q,L,b)) -
            (11.0f/15.0f + 7.0f/(15.0f*u_WR(q,L,b)))*exp(-u_WR(q,L,b)))*(b/L);

    } else { //q(i)*b > 3.1f
      ans = a1long(q, L, b, p1, p2, q0)/(pow((q*b),p1)) +
            a2long(q, L, b, p1, p2, q0)/(pow((q*b),p2)) + pi/(q*L);
    }
  } else { //L <= 4*b Shorter Chains
    if (q*b <= fmax(1.9f/sqrt(Rgsquareshort(q,L,b)),3.0f) ) {
      if (q*b<=0.01f) {
        ans = 1.0f - Rgsquareshort(q,L,b)*(q*q)/3.0f;
      } else {
        ans = Sdebye1(q,L,b);
      }
    } else {  //q*b > max(1.9f/sqrt(Rgsquareshort(q(i),L,b)),3)
      ans = a1short(q,L,b,p1short,p2short,q0short)/(pow((q*b),p1short)) +
            a2short(q,L,b,p1short,p2short,q0short)/(pow((q*b),p2short)) +
            pi/(q*L);
    }
  }

  return(ans);
}


float form_volume(float length, float kuhn_length, float radius);
float Iq(float q, float length, float kuhn_length, float radius,
          float axis_ratio, float sld, float solvent_sld);
float Iqxy(float qx, float qy, float length, float kuhn_length,
            float radius, float axis_ratio, float sld, float solvent_sld);
float flexible_cylinder_ex_kernel(float q, float length, float kuhn_length,
                                float radius, float axis_ratio, float sld,
                                float solvent_sld);
float elliptical_crosssection(float q, float a, float b);

float form_volume(float length, float kuhn_length, float radius)
{
    return 1.0f;
}

float
elliptical_crosssection(float q, float a, float b)
{
    float uplim,lolim,Pi,summ,arg,zi,yyy,answer;
    int i,nord=76;

    Pi = 4.0f*atan(1.0f);
    lolim=0.0f;
    uplim=Pi/2.0f;
    summ=0.0f;

    for(i=0;i<nord;i++) {
		zi = ( Gauss76Z[i]*(uplim-lolim) + uplim + lolim )/2.0f;
		arg = q*sqrt(a*a*sin(zi)*sin(zi)+b*b*cos(zi)*cos(zi));
		yyy = pow((2.0f * J1(arg) / arg),2);
		yyy *= Gauss76Wt[i];
		summ += yyy;
    }
    answer = (uplim-lolim)/2.0f*summ;
    answer *= 2.0f/Pi;
    return(answer);

}

float flexible_cylinder_ex_kernel(float q,
          float length,
          float kuhn_length,
          float radius,
          float axis_ratio,
          float sld,
          float solvent_sld)
{

	float Pi,flex,crossSect, cont;

	Pi = 4.0f*atan(1.0f);
	cont = sld - solvent_sld;
	crossSect = elliptical_crosssection(q,radius,(radius*axis_ratio));

	flex = Sk_WR(q,length,kuhn_length);
	flex *= crossSect;
	flex *= Pi*radius*radius*axis_ratio*axis_ratio*length;
	flex *= cont*cont;
	flex *= 1.0e-4f;

	return flex;
}

float Iq(float q,
          float length,
          float kuhn_length,
          float radius,
          float axis_ratio,
          float sld,
          float solvent_sld)
{

	float result = flexible_cylinder_ex_kernel(q,
	                length,
	                kuhn_length,
	                radius,
	                axis_ratio,
	                sld,
	                solvent_sld);

	return result;
}

float Iqxy(float qx, float qy,
            float length,
            float kuhn_length,
            float radius,
            float axis_ratio,
            float sld,
            float solvent_sld)
{
	float q;
	q = sqrt(qx*qx+qy*qy);
	float result = flexible_cylinder_ex_kernel(q,
	                length,
	                kuhn_length,
	                radius,
	                axis_ratio,
	                sld,
	                solvent_sld);

	return result;
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
