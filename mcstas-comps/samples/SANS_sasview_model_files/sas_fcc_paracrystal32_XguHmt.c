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


#define VOLUME_PARAMETERS radius
#define VOLUME_WEIGHT_PRODUCT radius_w
#define IQ_KERNEL_NAME fcc_paracrystal_Iq
#define IQ_PARAMETERS dnn, d_factor, radius, sld, solvent_sld
#define IQ_FIXED_PARAMETER_DECLARATIONS const float scale, \
    const float background, \
    const float dnn, \
    const float d_factor, \
    const float sld, \
    const float solvent_sld
#define IQ_WEIGHT_PRODUCT radius_w
#define IQ_DISPERSION_LENGTH_DECLARATIONS const int Nradius
#define IQ_DISPERSION_LENGTH_SUM Nradius
#define IQ_OPEN_LOOPS     for (int radius_i=0; radius_i < Nradius; radius_i++) { \
      const float radius = loops[2*(radius_i)]; \
      const float radius_w = loops[2*(radius_i)+1];
#define IQ_CLOSE_LOOPS     }
#define IQXY_KERNEL_NAME fcc_paracrystal_Iqxy
#define IQXY_PARAMETERS dnn, d_factor, radius, sld, solvent_sld, theta, phi, psi
#define IQXY_FIXED_PARAMETER_DECLARATIONS const float scale, \
    const float background, \
    const float dnn, \
    const float d_factor, \
    const float sld, \
    const float solvent_sld
#define IQXY_WEIGHT_PRODUCT radius_w*theta_w*phi_w*psi_w
#define IQXY_DISPERSION_LENGTH_DECLARATIONS const int Nradius, \
    const int Ntheta, \
    const int Nphi, \
    const int Npsi
#define IQXY_DISPERSION_LENGTH_SUM Nradius+Ntheta+Nphi+Npsi
#define IQXY_OPEN_LOOPS     for (int radius_i=0; radius_i < Nradius; radius_i++) { \
      const float radius = loops[2*(radius_i)]; \
      const float radius_w = loops[2*(radius_i)+1]; \
      for (int theta_i=0; theta_i < Ntheta; theta_i++) { \
        const float theta = loops[2*(theta_i+Nradius)]; \
        const float theta_w = loops[2*(theta_i+Nradius)+1]; \
        for (int phi_i=0; phi_i < Nphi; phi_i++) { \
          const float phi = loops[2*(phi_i+Nradius+Ntheta)]; \
          const float phi_w = loops[2*(phi_i+Nradius+Ntheta)+1]; \
          for (int psi_i=0; psi_i < Npsi; psi_i++) { \
            const float psi = loops[2*(psi_i+Nradius+Ntheta+Nphi)]; \
            const float psi_w = loops[2*(psi_i+Nradius+Ntheta+Nphi)+1];
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
constant float Gauss150Z[150]={
  	-0.9998723404457334f,
  	-0.9993274305065947f,
  	-0.9983473449340834f,
  	-0.9969322929775997f,
  	-0.9950828645255290f,
  	-0.9927998590434373f,
  	-0.9900842691660192f,
  	-0.9869372772712794f,
  	-0.9833602541697529f,
  	-0.9793547582425894f,
  	-0.9749225346595943f,
  	-0.9700655145738374f,
  	-0.9647858142586956f,
  	-0.9590857341746905f,
  	-0.9529677579610971f,
  	-0.9464345513503147f,
  	-0.9394889610042837f,
  	-0.9321340132728527f,
  	-0.9243729128743136f,
  	-0.9162090414984952f,
  	-0.9076459563329236f,
  	-0.8986873885126239f,
  	-0.8893372414942055f,
  	-0.8795995893549102f,
  	-0.8694786750173527f,
  	-0.8589789084007133f,
  	-0.8481048644991847f,
  	-0.8368612813885015f,
  	-0.8252530581614230f,
  	-0.8132852527930605f,
  	-0.8009630799369827f,
  	-0.7882919086530552f,
  	-0.7752772600680049f,
  	-0.7619248049697269f,
  	-0.7482403613363824f,
  	-0.7342298918013638f,
  	-0.7198995010552305f,
  	-0.7052554331857488f,
  	-0.6903040689571928f,
  	-0.6750519230300931f,
  	-0.6595056411226444f,
  	-0.6436719971150083f,
  	-0.6275578900977726f,
  	-0.6111703413658551f,
  	-0.5945164913591590f,
  	-0.5776035965513142f,
  	-0.5604390262878617f,
  	-0.5430302595752546f,
  	-0.5253848818220803f,
  	-0.5075105815339176f,
  	-0.4894151469632753f,
  	-0.4711064627160663f,
  	-0.4525925063160997f,
  	-0.4338813447290861f,
  	-0.4149811308476706f,
  	-0.3959000999390257f,
  	-0.3766465660565522f,
  	-0.3572289184172501f,
  	-0.3376556177463400f,
  	-0.3179351925907259f,
  	-0.2980762356029071f,
  	-0.2780873997969574f,
  	-0.2579773947782034f,
  	-0.2377549829482451f,
  	-0.2174289756869712f,
  	-0.1970082295132342f,
  	-0.1765016422258567f,
  	-0.1559181490266516f,
  	-0.1352667186271445f,
  	-0.1145563493406956f,
  	-0.0937960651617229f,
  	-0.0729949118337358f,
  	-0.0521619529078925f,
  	-0.0313062657937972f,
  	-0.0104369378042598f,
  	0.0104369378042598f,
  	0.0313062657937972f,
  	0.0521619529078925f,
  	0.0729949118337358f,
  	0.0937960651617229f,
  	0.1145563493406956f,
  	0.1352667186271445f,
  	0.1559181490266516f,
  	0.1765016422258567f,
  	0.1970082295132342f,
  	0.2174289756869712f,
  	0.2377549829482451f,
  	0.2579773947782034f,
  	0.2780873997969574f,
  	0.2980762356029071f,
  	0.3179351925907259f,
  	0.3376556177463400f,
  	0.3572289184172501f,
  	0.3766465660565522f,
  	0.3959000999390257f,
  	0.4149811308476706f,
  	0.4338813447290861f,
  	0.4525925063160997f,
  	0.4711064627160663f,
  	0.4894151469632753f,
  	0.5075105815339176f,
  	0.5253848818220803f,
  	0.5430302595752546f,
  	0.5604390262878617f,
  	0.5776035965513142f,
  	0.5945164913591590f,
  	0.6111703413658551f,
  	0.6275578900977726f,
  	0.6436719971150083f,
  	0.6595056411226444f,
  	0.6750519230300931f,
  	0.6903040689571928f,
  	0.7052554331857488f,
  	0.7198995010552305f,
  	0.7342298918013638f,
  	0.7482403613363824f,
  	0.7619248049697269f,
  	0.7752772600680049f,
  	0.7882919086530552f,
  	0.8009630799369827f,
  	0.8132852527930605f,
  	0.8252530581614230f,
  	0.8368612813885015f,
  	0.8481048644991847f,
  	0.8589789084007133f,
  	0.8694786750173527f,
  	0.8795995893549102f,
  	0.8893372414942055f,
  	0.8986873885126239f,
  	0.9076459563329236f,
  	0.9162090414984952f,
  	0.9243729128743136f,
  	0.9321340132728527f,
  	0.9394889610042837f,
  	0.9464345513503147f,
  	0.9529677579610971f,
  	0.9590857341746905f,
  	0.9647858142586956f,
  	0.9700655145738374f,
  	0.9749225346595943f,
  	0.9793547582425894f,
  	0.9833602541697529f,
  	0.9869372772712794f,
  	0.9900842691660192f,
  	0.9927998590434373f,
  	0.9950828645255290f,
  	0.9969322929775997f,
  	0.9983473449340834f,
  	0.9993274305065947f,
  	0.9998723404457334f
};

constant float Gauss150Wt[150]={
  	0.0003276086705538f,
  	0.0007624720924706f,
  	0.0011976474864367f,
  	0.0016323569986067f,
  	0.0020663664924131f,
  	0.0024994789888943f,
  	0.0029315036836558f,
  	0.0033622516236779f,
  	0.0037915348363451f,
  	0.0042191661429919f,
  	0.0046449591497966f,
  	0.0050687282939456f,
  	0.0054902889094487f,
  	0.0059094573005900f,
  	0.0063260508184704f,
  	0.0067398879387430f,
  	0.0071507883396855f,
  	0.0075585729801782f,
  	0.0079630641773633f,
  	0.0083640856838475f,
  	0.0087614627643580f,
  	0.0091550222717888f,
  	0.0095445927225849f,
  	0.0099300043714212f,
  	0.0103110892851360f,
  	0.0106876814158841f,
  	0.0110596166734735f,
  	0.0114267329968529f,
  	0.0117888704247183f,
  	0.0121458711652067f,
  	0.0124975796646449f,
  	0.0128438426753249f,
  	0.0131845093222756f,
  	0.0135194311690004f,
  	0.0138484622795371f,
  	0.0141714592928592f,
  	0.0144882814685445f,
  	0.0147987907597169f,
  	0.0151028518701744f,
  	0.0154003323133401f,
  	0.0156911024699895f,
  	0.0159750356447283f,
  	0.0162520081211971f,
  	0.0165218992159766f,
  	0.0167845913311726f,
  	0.0170399700056559f,
  	0.0172879239649355f,
  	0.0175283451696437f,
  	0.0177611288626114f,
  	0.0179861736145128f,
  	0.0182033813680609f,
  	0.0184126574807331f,
  	0.0186139107660094f,
  	0.0188070535331042f,
  	0.0189920016251754f,
  	0.0191686744559934f,
  	0.0193369950450545f,
  	0.0194968900511231f,
  	0.0196482898041878f,
  	0.0197911283358190f,
  	0.0199253434079123f,
  	0.0200508765398072f,
  	0.0201676730337687f,
  	0.0202756819988200f,
  	0.0203748563729175f,
  	0.0204651529434560f,
  	0.0205465323660984f,
  	0.0206189591819181f,
  	0.0206824018328499f,
  	0.0207368326754401f,
  	0.0207822279928917f,
  	0.0208185680053983f,
  	0.0208458368787627f,
  	0.0208640227312962f,
  	0.0208731176389954f,
  	0.0208731176389954f,
  	0.0208640227312962f,
  	0.0208458368787627f,
  	0.0208185680053983f,
  	0.0207822279928917f,
  	0.0207368326754401f,
  	0.0206824018328499f,
  	0.0206189591819181f,
  	0.0205465323660984f,
  	0.0204651529434560f,
  	0.0203748563729175f,
  	0.0202756819988200f,
  	0.0201676730337687f,
  	0.0200508765398072f,
  	0.0199253434079123f,
  	0.0197911283358190f,
  	0.0196482898041878f,
  	0.0194968900511231f,
  	0.0193369950450545f,
  	0.0191686744559934f,
  	0.0189920016251754f,
  	0.0188070535331042f,
  	0.0186139107660094f,
  	0.0184126574807331f,
  	0.0182033813680609f,
  	0.0179861736145128f,
  	0.0177611288626114f,
  	0.0175283451696437f,
  	0.0172879239649355f,
  	0.0170399700056559f,
  	0.0167845913311726f,
  	0.0165218992159766f,
  	0.0162520081211971f,
  	0.0159750356447283f,
  	0.0156911024699895f,
  	0.0154003323133401f,
  	0.0151028518701744f,
  	0.0147987907597169f,
  	0.0144882814685445f,
  	0.0141714592928592f,
  	0.0138484622795371f,
  	0.0135194311690004f,
  	0.0131845093222756f,
  	0.0128438426753249f,
  	0.0124975796646449f,
  	0.0121458711652067f,
  	0.0117888704247183f,
  	0.0114267329968529f,
  	0.0110596166734735f,
  	0.0106876814158841f,
  	0.0103110892851360f,
  	0.0099300043714212f,
  	0.0095445927225849f,
  	0.0091550222717888f,
  	0.0087614627643580f,
  	0.0083640856838475f,
  	0.0079630641773633f,
  	0.0075585729801782f,
  	0.0071507883396855f,
  	0.0067398879387430f,
  	0.0063260508184704f,
  	0.0059094573005900f,
  	0.0054902889094487f,
  	0.0050687282939456f,
  	0.0046449591497966f,
  	0.0042191661429919f,
  	0.0037915348363451f,
  	0.0033622516236779f,
  	0.0029315036836558f,
  	0.0024994789888943f,
  	0.0020663664924131f,
  	0.0016323569986067f,
  	0.0011976474864367f,
  	0.0007624720924706f,
  	0.0003276086705538f
};


float form_volume(float radius);
float Iq(float q,float dnn,float d_factor, float radius,float sld, float solvent_sld);
float Iqxy(float qx, float qy, float dnn,
    float d_factor, float radius,float sld, float solvent_sld,
    float theta, float phi, float psi);

float _FCC_Integrand(float q, float dnn, float d_factor, float theta, float phi);
float _FCCeval(float Theta, float Phi, float temp1, float temp3);
float _sphereform(float q, float radius, float sld, float solvent_sld);


float _FCC_Integrand(float q, float dnn, float d_factor, float theta, float phi) {

	const float Da = d_factor*dnn;
	const float temp1 = q*q*Da*Da;
	const float temp3 = q*dnn;

	float retVal = _FCCeval(theta,phi,temp1,temp3)/(4.0f*M_PI);
	return(retVal);
}

float _FCCeval(float Theta, float Phi, float temp1, float temp3) {

	float temp6,temp7,temp8,temp9,temp10;
	float result;

	temp6 = sin(Theta);
	temp7 = sin(Theta)*sin(Phi)+cos(Theta);
	temp8 = -1.0f*sin(Theta)*cos(Phi)+cos(Theta);
	temp9 = -1.0f*sin(Theta)*cos(Phi)+sin(Theta)*sin(Phi);
	temp10 = exp((-1.0f/8.0f)*temp1*((temp7*temp7)+(temp8*temp8)+(temp9*temp9)));
	result = pow((1.0f-(temp10*temp10)),3)*temp6/((1.0f-2.0f*temp10*cos(0.5f*temp3*(temp7))+(temp10*temp10))*(1.0f-2.0f*temp10*cos(0.5f*temp3*(temp8))+(temp10*temp10))*(1.0f-2.0f*temp10*cos(0.5f*temp3*(temp9))+(temp10*temp10)));

	return (result);
}

float _sphereform(float q, float radius, float sld, float solvent_sld){
    const float qr = q*radius;
    float sn, cn;
    SINCOS(qr, sn, cn);
    const float bes = (qr == 0.0f ? 1.0f : 3.0f*(sn-qr*cn)/(qr*qr*qr));
    const float fq = bes * (sld - solvent_sld)*form_volume(radius);
    return 1.0e-4f*fq*fq;
}

float form_volume(float radius){
    return 1.333333333333333f*M_PI*radius*radius*radius;
}


float Iq(float q, float dnn,
  float d_factor, float radius,
  float sld, float solvent_sld){

	//Volume fraction calculated from lattice symmetry and sphere radius
	const float s1 = dnn*sqrt(2.0f);
	const float latticescale = 4.0f*(4.0f/3.0f)*M_PI*(radius*radius*radius)/(s1*s1*s1);

    const float va = 0.0f;
    const float vb = 2.0f*M_PI;
    const float vaj = 0.0f;
    const float vbj = M_PI;

    float summ = 0.0f;
    float answer = 0.0f;
	for(int i=0; i<150; i++) {
		//setup inner integral over the ellipsoidal cross-section
		float summj=0.0f;
		const float zphi = ( Gauss150Z[i]*(vb-va) + va + vb )/2.0f;		//the outer dummy is phi
		for(int j=0;j<150;j++) {
			//20 gauss points for the inner integral
			float ztheta = ( Gauss150Z[j]*(vbj-vaj) + vaj + vbj )/2.0f;		//the inner dummy is theta
			float yyy = Gauss150Wt[j] * _FCC_Integrand(q,dnn,d_factor,ztheta,zphi);
			summj += yyy;
		}
		//now calculate the value of the inner integral
		float answer = (vbj-vaj)/2.0f*summj;

		//now calculate outer integral
		summ = summ+(Gauss150Wt[i] * answer);
	}		//final scaling is done at the end of the function, after the NT_FP64 case

	answer = (vb-va)/2.0f*summ;
	answer = answer*_sphereform(q,radius,sld,solvent_sld)*latticescale;

    return answer;


}


float Iqxy(float qx, float qy, float dnn,
    float d_factor, float radius,float sld, float solvent_sld,
    float theta, float phi, float psi){

  float b3_x, b3_y, b1_x, b1_y, b2_x, b2_y; //b3_z,
  float q_z;
  float cos_val_b3, cos_val_b2, cos_val_b1;
  float a1_dot_q, a2_dot_q,a3_dot_q;
  float answer;
  float Zq, Fkq, Fkq_2;

  //convert to q and make scaled values
  float q = sqrt(qx*qx+qy*qy);
  float q_x = qx/q;
  float q_y = qy/q;

  //convert angle degree to radian
  theta = theta * M_PI_180;
  phi = phi * M_PI_180;
  psi = psi * M_PI_180;

  const float Da = d_factor*dnn;
  const float s1 = dnn/sqrt(0.75f);


  //the occupied volume of the lattice
  const float latticescale = 2.0f*(4.0f/3.0f)*M_PI*(radius*radius*radius)/(s1*s1*s1);
  // q vector
  q_z = 0.0f; // for SANS; assuming qz is negligible
  /// Angles here are respect to detector coordinate
  ///  instead of against q coordinate(PRB 36(46), 3(6), 1754(3854))
    // b3 axis orientation
    b3_x = cos(theta) * cos(phi);
    b3_y = sin(theta);
    //b3_z = -cos(theta) * sin(phi);
    cos_val_b3 =  b3_x*q_x + b3_y*q_y;// + b3_z*q_z;

    //alpha = acos(cos_val_b3);
    // b1 axis orientation
    b1_x = -cos(phi)*sin(psi) * sin(theta)+sin(phi)*cos(psi);
    b1_y = sin(psi)*cos(theta);
    cos_val_b1 = b1_x*q_x + b1_y*q_y;
    // b2 axis orientation
    b2_x = -sin(theta)*cos(psi)*cos(phi)-sin(psi)*sin(phi);
  	b2_y = cos(theta)*cos(psi);
    cos_val_b2 = b2_x*q_x + b2_y*q_y;

    // The following test should always pass
    if (fabs(cos_val_b3)>1.0f) {
      //printf("FCC_ana_2D: Unexpected error: cos()>1\n");
      cos_val_b3 = 1.0f;
    }
    if (fabs(cos_val_b2)>1.0f) {
      //printf("FCC_ana_2D: Unexpected error: cos()>1\n");
      cos_val_b2 = 1.0f;
    }
    if (fabs(cos_val_b1)>1.0f) {
      //printf("FCC_ana_2D: Unexpected error: cos()>1\n");
      cos_val_b1 = 1.0f;
    }
    // Compute the angle btw vector q and the a3 axis
    a3_dot_q = 0.5f*dnn*q*(cos_val_b2+cos_val_b1-cos_val_b3);

    // a1 axis
    a1_dot_q = 0.5f*dnn*q*(cos_val_b3+cos_val_b2-cos_val_b1);

    // a2 axis
    a2_dot_q = 0.5f*dnn*q*(cos_val_b3+cos_val_b1-cos_val_b2);


    // Get Fkq and Fkq_2
    Fkq = exp(-0.5f*pow(Da/dnn,2.0f)*(a1_dot_q*a1_dot_q+a2_dot_q*a2_dot_q+a3_dot_q*a3_dot_q));
    Fkq_2 = Fkq*Fkq;
    // Call Zq=Z1*Z2*Z3
    Zq = (1.0f-Fkq_2)/(1.0f-2.0f*Fkq*cos(a1_dot_q)+Fkq_2);
    Zq *= (1.0f-Fkq_2)/(1.0f-2.0f*Fkq*cos(a2_dot_q)+Fkq_2);
    Zq *= (1.0f-Fkq_2)/(1.0f-2.0f*Fkq*cos(a3_dot_q)+Fkq_2);

  // Use SphereForm directly from libigor
  answer = _sphereform(q,radius,sld,solvent_sld)*Zq*latticescale;

  return answer;
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
