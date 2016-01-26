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


#define IQ_KERNEL_NAME rpa_Iq
#define IQ_PARAMETERS case_num, Na, Phia, va, La, ba, Nb, Phib, vb, Lb, bb, Nc, Phic, vc, Lc, bc, Nd, Phid, vd, Ld, bd, Kab, Kac, Kad, Kbc, Kbd, Kcd
#define IQ_FIXED_PARAMETER_DECLARATIONS const float scale, \
    const float background, \
    const float case_num, \
    const float Na, \
    const float Phia, \
    const float va, \
    const float La, \
    const float ba, \
    const float Nb, \
    const float Phib, \
    const float vb, \
    const float Lb, \
    const float bb, \
    const float Nc, \
    const float Phic, \
    const float vc, \
    const float Lc, \
    const float bc, \
    const float Nd, \
    const float Phid, \
    const float vd, \
    const float Ld, \
    const float bd, \
    const float Kab, \
    const float Kac, \
    const float Kad, \
    const float Kbc, \
    const float Kbd, \
    const float Kcd
#define IQXY_KERNEL_NAME rpa_Iqxy
#define IQXY_PARAMETERS case_num, Na, Phia, va, La, ba, Nb, Phib, vb, Lb, bb, Nc, Phic, vc, Lc, bc, Nd, Phid, vd, Ld, bd, Kab, Kac, Kad, Kbc, Kbd, Kcd
#define IQXY_FIXED_PARAMETER_DECLARATIONS const float scale, \
    const float background, \
    const float case_num, \
    const float Na, \
    const float Phia, \
    const float va, \
    const float La, \
    const float ba, \
    const float Nb, \
    const float Phib, \
    const float vb, \
    const float Lb, \
    const float bb, \
    const float Nc, \
    const float Phic, \
    const float vc, \
    const float Lc, \
    const float bc, \
    const float Nd, \
    const float Phid, \
    const float vd, \
    const float Ld, \
    const float bd, \
    const float Kab, \
    const float Kac, \
    const float Kad, \
    const float Kbc, \
    const float Kbd, \
    const float Kcd

float Iq(float q, float case_num,
    float Na, float Phia, float va, float a_sld, float ba,
    float Nb, float Phib, float vb, float b_sld, float bb,
    float Nc, float Phic, float vc, float c_sld, float bc,
    float Nd, float Phid, float vd, float d_sld, float bd,
    float Kab, float Kac, float Kad,
    float Kbc, float Kbd, float Kcd
    );

float Iqxy(float qx, float qy, float case_num,
    float Na, float Phia, float va, float a_sld, float ba,
    float Nb, float Phib, float vb, float b_sld, float bb,
    float Nc, float Phic, float vc, float c_sld, float bc,
    float Nd, float Phid, float vd, float d_sld, float bd,
    float Kab, float Kac, float Kad,
    float Kbc, float Kbd, float Kcd
    );

float form_volume(void);

float form_volume(void)
{
    return 1.0f;
}

float Iq(float q, float case_num,
    float Na, float Phia, float va, float La, float ba,
    float Nb, float Phib, float vb, float Lb, float bb,
    float Nc, float Phic, float vc, float Lc, float bc,
    float Nd, float Phid, float vd, float Ld, float bd,
    float Kab, float Kac, float Kad,
    float Kbc, float Kbd, float Kcd
    ) {
  int icase = (int)case_num;

#if 0  // Sasview defaults
  if (icase <= 1) {
    Na=Nb=1000.0f;
    Phia=Phib=0.0000001f;
    Kab=Kac=Kad=Kbc=Kbd=-0.0004f;
    La=Lb=1e-12;
    va=vb=100.0f;
    ba=bb=5.0f;
  } else if (icase <= 4) {
    Phia=0.0000001f;
    Kab=Kac=Kad=-0.0004f;
    La=1e-12;
    va=100.0f;
    ba=5.0f;
  }
#else
  if (icase <= 1) {
    Na=Nb=0.0f;
    Phia=Phib=0.0f;
    Kab=Kac=Kad=Kbc=Kbd=0.0f;
    La=Lb=Ld;
    va=vb=vd;
    ba=bb=0.0f;
  } else if (icase <= 4) {
    Na = 0.0f;
    Phia=0.0f;
    Kab=Kac=Kad=0.0f;
    La=Ld;
    va=vd;
    ba=0.0f;
  }
#endif

  const float Xa = q*q*ba*ba*Na/6.0f;
  const float Xb = q*q*bb*bb*Nb/6.0f;
  const float Xc = q*q*bc*bc*Nc/6.0f;
  const float Xd = q*q*bd*bd*Nd/6.0f;

  // limit as Xa goes to 0 is 1
  const float Pa = Xa==0 ? 1.0f : -expm1(-Xa)/Xa;
  const float Pb = Xb==0 ? 1.0f : -expm1(-Xb)/Xb;
  const float Pc = Xc==0 ? 1.0f : -expm1(-Xc)/Xc;
  const float Pd = Xd==0 ? 1.0f : -expm1(-Xd)/Xd;

  // limit as Xa goes to 0 is 1
  const float Paa = Xa==0 ? 1.0f : 2.0f*(1.0f-Pa)/Xa;
  const float Pbb = Xb==0 ? 1.0f : 2.0f*(1.0f-Pb)/Xb;
  const float Pcc = Xc==0 ? 1.0f : 2.0f*(1.0f-Pc)/Xc;
  const float Pdd = Xd==0 ? 1.0f : 2.0f*(1.0f-Pd)/Xd;


  // Note: S0ij only defined for copolymers; otherwise set to zero
  // 0: C/D     binary mixture
  // 1: C-D     diblock copolymer
  // 2: B/C/D   ternery mixture
  // 3: B/C-D   binary mixture,1 homopolymer, 1 diblock copolymer
  // 4: B-C-D   triblock copolymer
  // 5: A/B/C/D quaternary mixture
  // 6: A/B/C-D ternery mixture, 2 homopolymer, 1 diblock copolymer
  // 7: A/B-C-D binary mixture, 1 homopolymer, 1 triblock copolymer
  // 8: A-B/C-D binary mixture, 2 diblock copolymer
  // 9: A-B-C-D tetra-block copolymer
#if 0
  const float S0aa = icase<5
                      ? 1.0f : Na*Phia*va*Paa;
  const float S0bb = icase<2
                      ? 1.0f : Nb*Phib*vb*Pbb;
  const float S0cc = Nc*Phic*vc*Pcc;
  const float S0dd = Nd*Phid*vd*Pdd;
  const float S0ab = icase<8
                      ? 0.0f : sqrt(Na*va*Phia*Nb*vb*Phib)*Pa*Pb;
  const float S0ac = icase<9
                      ? 0.0f : sqrt(Na*va*Phia*Nc*vc*Phic)*Pa*Pc*exp(-Xb);
  const float S0ad = icase<9
                      ? 0.0f : sqrt(Na*va*Phia*Nd*vd*Phid)*Pa*Pd*exp(-Xb-Xc);
  const float S0bc = (icase!=4 && icase!=7 && icase!= 9)
                      ? 0.0f : sqrt(Nb*vb*Phib*Nc*vc*Phic)*Pb*Pc;
  const float S0bd = (icase!=4 && icase!=7 && icase!= 9)
                      ? 0.0f : sqrt(Nb*vb*Phib*Nd*vd*Phid)*Pb*Pd*exp(-Xc);
  const float S0cd = (icase==0 || icase==2 || icase==5)
                      ? 0.0f : sqrt(Nc*vc*Phic*Nd*vd*Phid)*Pc*Pd;
#else  // sasview equivalent
//printf("Xc=%g, S0cc=%g*%g*%g*%g\n",Xc,Nc,Phic,vc,Pcc);
  float S0aa = Na*Phia*va*Paa;
  float S0bb = Nb*Phib*vb*Pbb;
  float S0cc = Nc*Phic*vc*Pcc;
  float S0dd = Nd*Phid*vd*Pdd;
  float S0ab = sqrt(Na*va*Phia*Nb*vb*Phib)*Pa*Pb;
  float S0ac = sqrt(Na*va*Phia*Nc*vc*Phic)*Pa*Pc*exp(-Xb);
  float S0ad = sqrt(Na*va*Phia*Nd*vd*Phid)*Pa*Pd*exp(-Xb-Xc);
  float S0bc = sqrt(Nb*vb*Phib*Nc*vc*Phic)*Pb*Pc;
  float S0bd = sqrt(Nb*vb*Phib*Nd*vd*Phid)*Pb*Pd*exp(-Xc);
  float S0cd = sqrt(Nc*vc*Phic*Nd*vd*Phid)*Pc*Pd;
switch(icase){
  case 0:
    S0aa=0.000001f;
    S0ab=0.000002f;
    S0ac=0.000003f;
    S0ad=0.000004f;
    S0bb=0.000005f;
    S0bc=0.000006f;
    S0bd=0.000007f;
    S0cd=0.000008f;
    break;
  case 1:
    S0aa=0.000001f;
    S0ab=0.000002f;
    S0ac=0.000003f;
    S0ad=0.000004f;
    S0bb=0.000005f;
    S0bc=0.000006f;
    S0bd=0.000007f;
    break;
  case 2:
    S0aa=0.000001f;
    S0ab=0.000002f;
    S0ac=0.000003f;
    S0ad=0.000004f;
    S0bc=0.000005f;
    S0bd=0.000006f;
    S0cd=0.000007f;
    break;
  case 3:
    S0aa=0.000001f;
    S0ab=0.000002f;
    S0ac=0.000003f;
    S0ad=0.000004f;
    S0bc=0.000005f;
    S0bd=0.000006f;
    break;
  case 4:
    S0aa=0.000001f;
    S0ab=0.000002f;
    S0ac=0.000003f;
    S0ad=0.000004f;
    break;
  case 5:
    S0ab=0.000001f;
    S0ac=0.000002f;
    S0ad=0.000003f;
    S0bc=0.000004f;
    S0bd=0.000005f;
    S0cd=0.000006f;
    break;
  case 6:
    S0ab=0.000001f;
    S0ac=0.000002f;
    S0ad=0.000003f;
    S0bc=0.000004f;
    S0bd=0.000005f;
    break;
  case 7:
    S0ab=0.000001f;
    S0ac=0.000002f;
    S0ad=0.000003f;
    break;
  case 8:
    S0ac=0.000001f;
    S0ad=0.000002f;
    S0bc=0.000003f;
    S0bd=0.000004f;
    break;
  default : //case 9:
    break;
  }
#endif

  // eq 12a: \kappa_{ij}^F = \chi_{ij}^F - \chi_{i0}^F - \chi_{j0}^F
  const float Kaa = 0.0f;
  const float Kbb = 0.0f;
  const float Kcc = 0.0f;
  const float Kdd = 0.0f;
  const float Zaa = Kaa - Kad - Kad;
  const float Zab = Kab - Kad - Kbd;
  const float Zac = Kac - Kad - Kcd;
  const float Zbb = Kbb - Kbd - Kbd;
  const float Zbc = Kbc - Kbd - Kcd;
  const float Zcc = Kcc - Kcd - Kcd;
//printf("Za: %10.5fg %10.5fg %10.5fg\n", Zaa, Zab, Zac);
//printf("Zb: %10.5fg %10.5fg %10.5fg\n", Zab, Zbb, Zbc);
//printf("Zc: %10.5fg %10.5fg %10.5fg\n", Zac, Zbc, Zcc);

  // T = inv(S0)
  const float DenT = (- S0ac*S0bb*S0ac + S0ab*S0bc*S0ac + S0ac*S0ab*S0bc
                       - S0aa*S0bc*S0bc - S0ab*S0ab*S0cc + S0aa*S0bb*S0cc);
  const float T11 = (-S0bc*S0bc + S0bb*S0cc)/DenT;
  const float T12 = ( S0ac*S0bc - S0ab*S0cc)/DenT;
  const float T13 = (-S0ac*S0bb + S0ab*S0bc)/DenT;
  const float T22 = (-S0ac*S0ac + S0aa*S0cc)/DenT;
  const float T23 = ( S0ac*S0ab - S0aa*S0bc)/DenT;
  const float T33 = (-S0ab*S0ab + S0aa*S0bb)/DenT;

//printf("T1: %10.5fg %10.5fg %10.5fg\n", T11, T12, T13);
//printf("T2: %10.5fg %10.5fg %10.5fg\n", T12, T22, T23);
//printf("T3: %10.5fg %10.5fg %10.5fg\n", T13, T23, T33);

  // eq 18e: m = 1/(S0_{dd} - s0^T inv(S0) s0)
  const float ZZ = S0ad*(T11*S0ad + T12*S0bd + T13*S0cd)
                  + S0bd*(T12*S0ad + T22*S0bd + T23*S0cd)
                  + S0cd*(T13*S0ad + T23*S0bd + T33*S0cd);

  const float m=1.0f/(S0dd-ZZ);

  // eq 18d: Y = inv(S0)s0 + e
  const float Y1 = T11*S0ad + T12*S0bd + T13*S0cd + 1.0f;
  const float Y2 = T12*S0ad + T22*S0bd + T23*S0cd + 1.0f;
  const float Y3 = T13*S0ad + T23*S0bd + T33*S0cd + 1.0f;

  // N = mYY^T + \kappa^F
  const float N11 = m*Y1*Y1 + Zaa;
  const float N12 = m*Y1*Y2 + Zab;
  const float N13 = m*Y1*Y3 + Zac;
  const float N22 = m*Y2*Y2 + Zbb;
  const float N23 = m*Y2*Y3 + Zbc;
  const float N33 = m*Y3*Y3 + Zcc;

//printf("N1: %10.5fg %10.5fg %10.5fg\n", N11, N12, N13);
//printf("N2: %10.5fg %10.5fg %10.5fg\n", N12, N22, N23);
//printf("N3: %10.5fg %10.5fg %10.5fg\n", N13, N23, N33);
//printf("S0a: %10.5fg %10.5fg %10.5fg\n", S0aa, S0ab, S0ac);
//printf("S0b: %10.5fg %10.5fg %10.5fg\n", S0ab, S0bb, S0bc);
//printf("S0c: %10.5fg %10.5fg %10.5fg\n", S0ac, S0bc, S0cc);

  // M = I + S0 N
  const float Maa = N11*S0aa + N12*S0ab + N13*S0ac + 1.0f;
  const float Mab = N11*S0ab + N12*S0bb + N13*S0bc;
  const float Mac = N11*S0ac + N12*S0bc + N13*S0cc;
  const float Mba = N12*S0aa + N22*S0ab + N23*S0ac;
  const float Mbb = N12*S0ab + N22*S0bb + N23*S0bc + 1.0f;
  const float Mbc = N12*S0ac + N22*S0bc + N23*S0cc;
  const float Mca = N13*S0aa + N23*S0ab + N33*S0ac;
  const float Mcb = N13*S0ab + N23*S0bb + N33*S0bc;
  const float Mcc = N13*S0ac + N23*S0bc + N33*S0cc + 1.0f;
//printf("M1: %10.5fg %10.5fg %10.5fg\n", Maa, Mab, Mac);
//printf("M2: %10.5fg %10.5fg %10.5fg\n", Mba, Mbb, Mbc);
//printf("M3: %10.5fg %10.5fg %10.5fg\n", Mca, Mcb, Mcc);

  // Q = inv(M) = inv(I + S0 N)
  const float DenQ = (+ Maa*Mbb*Mcc - Maa*Mbc*Mcb - Mab*Mba*Mcc
                       + Mab*Mbc*Mca + Mac*Mba*Mcb - Mac*Mbb*Mca);

  const float Q11 = ( Mbb*Mcc - Mbc*Mcb)/DenQ;
  const float Q12 = (-Mab*Mcc + Mac*Mcb)/DenQ;
  const float Q13 = ( Mab*Mbc - Mac*Mbb)/DenQ;
  const float Q21 = (-Mba*Mcc + Mbc*Mca)/DenQ;
  const float Q22 = ( Maa*Mcc - Mac*Mca)/DenQ;
  const float Q23 = (-Maa*Mbc + Mac*Mba)/DenQ;
  const float Q31 = ( Mba*Mcb - Mbb*Mca)/DenQ;
  const float Q32 = (-Maa*Mcb + Mab*Mca)/DenQ;
  const float Q33 = ( Maa*Mbb - Mab*Mba)/DenQ;

//printf("Q1: %10.5fg %10.5fg %10.5fg\n", Q11, Q12, Q13);
//printf("Q2: %10.5fg %10.5fg %10.5fg\n", Q21, Q22, Q23);
//printf("Q3: %10.5fg %10.5fg %10.5fg\n", Q31, Q32, Q33);
  // eq 18c: inv(S) = inv(S0) + mYY^T + \kappa^F
  // eq A1 in the appendix
  // To solve for S, use:
  //      S = inv(inv(S^0) + N) inv(S^0) S^0
  //        = inv(S^0 inv(S^0) + N) S^0
  //        = inv(I + S^0 N) S^0
  //        = Q S^0
  const float S11 = Q11*S0aa + Q12*S0ab + Q13*S0ac;
  const float S12 = Q12*S0aa + Q22*S0ab + Q23*S0ac;
  const float S13 = Q13*S0aa + Q23*S0ab + Q33*S0ac;
  const float S22 = Q12*S0ab + Q22*S0bb + Q23*S0bc;
  const float S23 = Q13*S0ab + Q23*S0bb + Q33*S0bc;
  const float S33 = Q13*S0ac + Q23*S0bc + Q33*S0cc;
  // If the full S is needed...it isn't since Ldd = (rho_d - rho_d) = 0 below
  //const float S14=-S11-S12-S13;
  //const float S24=-S12-S22-S23;
  //const float S34=-S13-S23-S33;
  //const float S44=S11+S22+S33 + 2.0f*(S12+S13+S23);

  // eq 12 of Akcasu, 1990: I(q) = L^T S L
  // Note: eliminate cases without A and B polymers by setting Lij to 0
  // Note: 1e-13 to convert from fm to cm for scattering length
  const float sqrt_Nav=sqrt(6.022045e+23f) * 1.0e-13f;
  const float Lad = icase<5 ? 0.0f : (La/va - Ld/vd)*sqrt_Nav;
  const float Lbd = icase<2 ? 0.0f : (Lb/vb - Ld/vd)*sqrt_Nav;
  const float Lcd = (Lc/vc - Ld/vd)*sqrt_Nav;

  const float result=Lad*Lad*S11 + Lbd*Lbd*S22 + Lcd*Lcd*S33
                    + 2.0f*(Lad*Lbd*S12 + Lbd*Lcd*S23 + Lad*Lcd*S13);

  return result;

}

float Iqxy(float qx, float qy,
    float case_num,
    float Na, float Phia, float va, float a_sld, float ba,
    float Nb, float Phib, float vb, float b_sld, float bb,
    float Nc, float Phic, float vc, float c_sld, float bc,
    float Nd, float Phid, float vd, float d_sld, float bd,
    float Kab, float Kac, float Kad,
    float Kbc, float Kbd, float Kcd
    )
{
    float q = sqrt(qx*qx + qy*qy);
    return Iq(q,
        case_num,
        Na, Phia, va, a_sld, ba,
        Nb, Phib, vb, b_sld, bb,
        Nc, Phic, vc, c_sld, bc,
        Nd, Phid, vd, d_sld, bd,
        Kab, Kac, Kad,
        Kbc, Kbd, Kcd);
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
