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


#define VOLUME_PARAMETERS effect_radius
#define VOLUME_WEIGHT_PRODUCT effect_radius_w
#define VOLUME_PARAMETER_DECLARATIONS float effect_radius
#define IQ_KERNEL_NAME HayterMSAsq_Iq
#define IQ_PARAMETERS effect_radius, charge, volfraction, temperature, saltconc, dielectconst
#define IQ_FIXED_PARAMETER_DECLARATIONS const float scale, \
    const float background, \
    const float charge, \
    const float volfraction, \
    const float temperature, \
    const float saltconc, \
    const float dielectconst
#define IQ_WEIGHT_PRODUCT effect_radius_w
#define IQ_DISPERSION_LENGTH_DECLARATIONS const int Neffect_radius
#define IQ_DISPERSION_LENGTH_SUM Neffect_radius
#define IQ_OPEN_LOOPS     for (int effect_radius_i=0; effect_radius_i < Neffect_radius; effect_radius_i++) { \
      const float effect_radius = loops[2*(effect_radius_i)]; \
      const float effect_radius_w = loops[2*(effect_radius_i)+1];
#define IQ_CLOSE_LOOPS     }
#define IQXY_KERNEL_NAME HayterMSAsq_Iqxy
#define IQXY_PARAMETERS effect_radius, charge, volfraction, temperature, saltconc, dielectconst
#define IQXY_FIXED_PARAMETER_DECLARATIONS const float scale, \
    const float background, \
    const float charge, \
    const float volfraction, \
    const float temperature, \
    const float saltconc, \
    const float dielectconst
#define IQXY_WEIGHT_PRODUCT effect_radius_w
#define IQXY_DISPERSION_LENGTH_DECLARATIONS const int Neffect_radius
#define IQXY_DISPERSION_LENGTH_SUM Neffect_radius
#define IQXY_OPEN_LOOPS     for (int effect_radius_i=0; effect_radius_i < Neffect_radius; effect_radius_i++) { \
      const float effect_radius = loops[2*(effect_radius_i)]; \
      const float effect_radius_w = loops[2*(effect_radius_i)+1];
#define IQXY_CLOSE_LOOPS     }
#define IQXY_PARAMETER_DECLARATIONS float effect_radius, float charge, float volfraction, float temperature, float saltconc, float dielectconst

// Hayter-Penfold (rescaled) MSA structure factor for screened Coulomb interactions 
//
// C99 needs declarations of routines here
float Iq(float QQ,
      float effect_radius, float zz, float VolFrac, float Temp, float csalt, float dialec);
int
sqcoef(int ir, float gMSAWave[]);

int
sqfun(int ix, int ir, float gMSAWave[]);

float
sqhcal(float qq, float gMSAWave[]);
  
float Iq(float QQ,
      float effect_radius, float zz, float VolFrac, float Temp, float csalt, float dialec)  
{
    float gMSAWave[17]={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17};
	float Elcharge=1.602189e-19f;		// electron charge in Coulombs (C)
	float kB=1.380662e-23f;				// Boltzman constant in J/K
	float FrSpPerm=8.85418782E-12f;	//Permittivity of free space in C^2/(N m^2)
	float SofQ, Qdiam, Vp, ss;
	float SIdiam, diam, Kappa, cs, IonSt;
	float  Perm, Beta;
	float pi, charge;
	int ierr;
	
	pi = M_PI;

	diam=2*effect_radius;		//in A

						////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
						//////////////////////////// convert to USEFUL inputs in SI units                                                //
						////////////////////////////    NOTE: easiest to do EVERYTHING in SI units                               //
						////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	Beta=1.0f/(kB*Temp);		// in Joules^-1
	Perm=dialec*FrSpPerm;	//in C^2/(N  m^2)
	charge=zz*Elcharge;		//in Coulomb (C)
	SIdiam = diam*1.0E-10f;		//in m
	Vp=4.0f*pi/3.0f*(SIdiam/2.0f)*(SIdiam/2.0f)*(SIdiam/2.0f);	//in m^3
	cs=csalt*6.022E23f*1.0E3f;	//# salt molecules/m^3
	
	//         Compute the derived values of :
	//			 Ionic strength IonSt (in C^2/m^3)  
	// 			Kappa (Debye-Huckel screening length in m)
	//	and		gamma Exp(-k)
	IonSt=0.5f * Elcharge*Elcharge*(zz*VolFrac/Vp+2.0f*cs);
	Kappa=sqrt(2*Beta*IonSt/Perm);     //Kappa calc from Ionic strength
									   //	Kappa=2/SIdiam					// Use to compare with HP paper
	gMSAWave[5]=Beta*charge*charge/(pi*Perm*SIdiam*pow((2.0f+Kappa*SIdiam),2));
	
	//         Finally set up dimensionless parameters 
	Qdiam=QQ*diam;
	gMSAWave[6] = Kappa*SIdiam;
	gMSAWave[4] = VolFrac;
	
	//Function sqhpa(qq)  {this is where Hayter-Penfold program began}
	
	//       FIRST CALCULATE COUPLING
	
	ss=pow(gMSAWave[4],(1.0f/3.0f));
	gMSAWave[9] = 2.0f*ss*gMSAWave[5]*exp(gMSAWave[6]-gMSAWave[6]/ss);
	
	//        CALCULATE COEFFICIENTS, CHECK ALL IS WELL
	//        AND IF SO CALCULATE S(Q*SIG)
	
	ierr=0;
	ierr=sqcoef(ierr, gMSAWave);
	if (ierr>=0) {
		SofQ=sqhcal(Qdiam, gMSAWave);
	}else{
       	//SofQ=NaN;
		SofQ=-1.0f;
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
sqcoef(int ir, float gMSAWave[])
{	
	int itm=40,ix,ig,ii;
	float acc=5.0E-6f,del,e1,e2,f1,f2;

	//      WAVE gMSAWave = $"root:HayPenMSA:gMSAWave"
	f1=0;		//these were never properly initialized...
	f2=0;
	
	ig=1;
	if (gMSAWave[6]>=(1.0f+8.0f*gMSAWave[4])) {
		ig=0;
		gMSAWave[15]=gMSAWave[14];
		gMSAWave[16]=gMSAWave[4];
		ix=1;
		ir = sqfun(ix,ir,gMSAWave);
		gMSAWave[14]=gMSAWave[15];
		gMSAWave[4]=gMSAWave[16];
		if((ir<0.0f) || (gMSAWave[14]>=0.0f)) {
			return ir;
		}
	}
	gMSAWave[10]=fmin(gMSAWave[4],0.20f);
	if ((ig!=1) || ( gMSAWave[9]>=0.15f)) {
		ii=0;                             
		do {
			ii=ii+1;
			if(ii>itm) {
				ir=-1;
				return ir;		
			}
			if (gMSAWave[10]<=0.0f) {
			    gMSAWave[10]=gMSAWave[4]/ii;
			}
			if(gMSAWave[10]>0.6f) {
			    gMSAWave[10] = 0.35f/ii;
			}
			e1=gMSAWave[10];
			gMSAWave[15]=f1;
			gMSAWave[16]=e1;
			ix=2;
			ir = sqfun(ix,ir,gMSAWave);
			f1=gMSAWave[15];
			e1=gMSAWave[16];
			e2=gMSAWave[10]*1.01f;
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
	if ((ir>=0) && (gMSAWave[14]<0.0f)) {
		ir=-3;
	}
	return ir;
}


int
sqfun(int ix, int ir, float gMSAWave[])
{	
	float acc=1.0e-6f;
	float reta,eta2,eta21,eta22,eta3,eta32,eta2d,eta2d2,eta3d,eta6d,e12,e24,rgek;
	float rak,ak1,ak2,dak,dak2,dak4,d,d2,dd2,dd4,dd45,ex1,ex2,sk,ck,ckma,skma;
	float al1,al2,al3,al4,al5,al6,be1,be2,be3,vu1,vu2,vu3,vu4,vu5,ph1,ph2,ta1,ta2,ta3,ta4,ta5;
	float a1,a2,a3,b1,b2,b3,v1,v2,v3,p1,p2,p3,pp,pp1,pp2,p1p2,t1,t2,t3,um1,um2,um3,um4,um5,um6;
	float w0,w1,w2,w3,w4,w12,w13,w14,w15,w16,w24,w25,w26,w32,w34,w3425,w35,w3526,w36,w46,w56;
	float fa,fap,ca,e24g,pwk,qpw,pg,del,fun,fund,g24;
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
	e12 = 12.0f*reta;
	e24 = e12+e12;
	gMSAWave[13] = pow( (gMSAWave[4]/gMSAWave[16]),(1.0f/3.0f));
	gMSAWave[12]=gMSAWave[6]/gMSAWave[13];
	ibig=0;
	if (( gMSAWave[12]>15.0f) && (ix==1)) {
		ibig=1;
	}
    
	gMSAWave[11] = gMSAWave[5]*gMSAWave[13]*exp(gMSAWave[6]- gMSAWave[12]);
	rgek =  gMSAWave[11];
	rak =  gMSAWave[12];
	ak2 = rak*rak;
	ak1 = 1.0f+rak;
	dak2 = 1.0f/ak2;
	dak4 = dak2*dak2;
	d = 1.0f-reta;
	d2 = d*d;
	dak = d/rak;
	dd2 = 1.0f/d2;
	dd4 = dd2*dd2;
	dd45 = dd4*2.0e-1f;
	eta3d=3.0f*reta;
	eta6d = eta3d+eta3d;
	eta32 = eta3+ eta3;
	eta2d = reta+2.0f;
	eta2d2 = eta2d*eta2d;
	eta21 = 2.0f*reta+1.0f;
	eta22 = eta21*eta21;
	
	//     ALPHA(I)
	
	al1 = -eta21*dak;
	al2 = (14.0f*eta2-4.0f*reta-1.0f)*dak2;
	al3 = 36.0f*eta2*dak4;
	
	//      BETA(I)
	
	be1 = -(eta2+7.0f*reta+1.0f)*dak;
	be2 = 9.0f*reta*(eta2+4.0f*reta-2.0f)*dak2;
	be3 = 12.0f*reta*(2.0f*eta2+8.0f*reta-1.0f)*dak4;
	
	//      NU(I)
	
	vu1 = -(eta3+3.0f*eta2+45.0f*reta+5.0f)*dak;
	vu2 = (eta32+3.0f*eta2+42.0f*reta-2.0e1f)*dak2;
	vu3 = (eta32+3.0e1f*reta-5.0f)*dak4;
	vu4 = vu1+e24*rak*vu3;
	vu5 = eta6d*(vu2+4.0f*vu3);
	
	//      PHI(I)
	
	ph1 = eta6d/rak;
	ph2 = d-e12*dak2;
	
	//      TAU(I)
	
	ta1 = (reta+5.0f)/(5.0f*rak);
	ta2 = eta2d*dak2;
	ta3 = -e12*rgek*(ta1+ta2);
	ta4 = eta3d*ak2*(ta1*ta1-ta2*ta2);
	ta5 = eta3d*(reta+8.0f)*1.0e-1f-2.0f*eta22*dak2;
	
	//     float PRECISION SINH(K), COSH(K)
	
	ex1 = exp(rak);
	ex2 = 0.0f;
	if ( gMSAWave[12]<20.0f) {
		ex2=exp(-rak);
	}
	sk=0.5f*(ex1-ex2);
	ck = 0.5f*(ex1+ex2);
	ckma = ck-1.0f-rak*sk;
	skma = sk-rak*ck;
	
	//      a(I)
	
	a1 = (e24*rgek*(al1+al2+ak1*al3)-eta22)*dd4;
	if (ibig==0) {
		a2 = e24*(al3*skma+al2*sk-al1*ck)*dd4;
		a3 = e24*(eta22*dak2-0.5f*d2+al3*ckma-al1*sk+al2*ck)*dd4;
	}
	
	//      b(I)
	
	b1 = (1.5f*reta*eta2d2-e12*rgek*(be1+be2+ak1*be3))*dd4;
	if (ibig==0) {
		b2 = e12*(-be3*skma-be2*sk+be1*ck)*dd4;
		b3 = e12*(0.5f*d2*eta2d-eta3d*eta2d2*dak2-be3*ckma+be1*sk-be2*ck)*dd4;
	}
	
	//      V(I)
	
	v1 = (eta21*(eta2-2.0f*reta+1.0e1f)*2.5e-1f-rgek*(vu4+vu5))*dd45;
	if (ibig==0) {
		v2 = (vu4*ck-vu5*sk)*dd45;
		v3 = ((eta3-6.0f*eta2+5.0f)*d-eta6d*(2.0f*eta3-3.0f*eta2+18.0f*reta+1.0e1f)*dak2+e24*vu3+vu4*sk-vu5*ck)*dd45;
	}
	
	
	//       P(I)
	
	pp1 = ph1*ph1;
	pp2 = ph2*ph2;
	pp = pp1+pp2;
	p1p2 = ph1*ph2*2.0f;
	p1 = (rgek*(pp1+pp2-p1p2)-0.5f*eta2d)*dd2;
	if (ibig==0) {
		p2 = (pp*sk+p1p2*ck)*dd2;
		p3 = (pp*ck+p1p2*sk+pp1-pp2)*dd2;
	}
	
	//       T(I)
	
	t1 = ta3+ta4*a1+ta5*b1;
	if (ibig!=0) {
		
		//		VERY LARGE SCREENING:  ASYMPTOTIC SOLUTION
		
  		v3 = ((eta3-6.0f*eta2+5.0f)*d-eta6d*(2.0f*eta3-3.0f*eta2+18.0f*reta+1.0e1f)*dak2+e24*vu3)*dd45;
		t3 = ta4*a3+ta5*b3+e12*ta2 - 4.0e-1f*reta*(reta+1.0e1f)-1.0f;
		p3 = (pp1-pp2)*dd2;
		b3 = e12*(0.5f*d2*eta2d-eta3d*eta2d2*dak2+be3)*dd4;
		a3 = e24*(eta22*dak2-0.5f*d2-al3)*dd4;
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
		if (fabs(gMSAWave[15])<1.0e-3f) {
			gMSAWave[15] = 0.0f;
		}
		gMSAWave[10] = gMSAWave[16];
		
	} else {
        
		t2 = ta4*a2+ta5*b2+e12*(ta1*ck-ta2*sk);
		t3 = ta4*a3+ta5*b3+e12*(ta1*sk-ta2*(ck-1.0f))-4.0e-1f*reta*(reta+1.0e1f)-1.0f;
		
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
			w3 = 2.0f*w16*w15-w13*w3526-w12*w36;
			w2 = w15*w15+2.0f*w16*w14-w13*w3425-w12*w3526;
			w1 = 2.0f*w15*w14-w13*w24-w12*w3425;
			w0 = w14*w14-w12*w24;
			
			//			ESTIMATE THE STARTING VALUE OF f
			
			if (ix==1) {
				//				LARGE K
				fap = (w14-w34-w46)/(w12-w15+w35-w26+w56-w32);
			} else {
				//				ASSUME NOT TOO FAR FROM GILLAN CONDITION.
				//				IF BOTH RGEK AND RAK ARE SMALL, USE P-W ESTIMATE.
				gMSAWave[14]=0.5f*eta2d*dd2*exp(-rgek);
				if (( gMSAWave[11]<=2.0f) && ( gMSAWave[11]>=0.0f) && ( gMSAWave[12]<=1.0f)) {
					e24g = e24*rgek*exp(rak);
					pwk = sqrt(e24g);
					qpw = (1.0f-sqrt(1.0f+2.0f*d2*d*pwk/eta22))*eta21/d;
					gMSAWave[14] = -qpw*qpw/e24+0.5f*eta2d*dd2;
				}
  				pg = p1+gMSAWave[14];
				ca = ak2*pg+2.0f*(b3*pg-b1*p3)+e12*gMSAWave[14]*gMSAWave[14]*p3;
				ca = -ca/(ak2*p2+2.0f*(b3*p2-b2*p3));
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
				fund = w1+(2.0f*w2+(3.0f*w3+4.0f*w4*fa)*fa)*fa;
				fap = fa-fun/fund;
				del=fabs((fap-fa)/fa);
			} while (del>acc);
			
			ir = ir+ii;
			fa = fap;
			ca = -(w16*fa*fa+w15*fa+w14)/(w13*fa+w12);
			gMSAWave[14] = -(p1+p2*ca+p3*fa);
			gMSAWave[15] = gMSAWave[14];
			if (fabs(gMSAWave[15])<1.0e-3f) {
				gMSAWave[15] = 0.0f;
			}
			gMSAWave[10] = gMSAWave[16];
		} else {
			ca = ak2*p1+2.0f*(b3*p1-b1*p3);
			ca = -ca/(ak2*p2+2.0f*(b3*p2-b2*p3));
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

float
sqhcal(float qq, float gMSAWave[])
{      	
    float SofQ,etaz,akz,gekz,e24,x1,x2,ck,sk,ak2,qk,q2k,qk2,qk3,qqk,sink,cosk,asink,qcosk,aqk,inter; 		
	//	WAVE gMSAWave = $"root:HayPenMSA:gMSAWave"

	etaz = gMSAWave[10];
	akz =  gMSAWave[12];
	gekz =  gMSAWave[11];
	e24 = 24.0f*etaz;
	x1 = exp(akz);
	x2 = 0.0f;
	if ( gMSAWave[12]<20.0f) {
		x2 = exp(-akz);
	}
	ck = 0.5f*(x1+x2);
	sk = 0.5f*(x1-x2);
	ak2 = akz*akz;
	
	if (qq<=0.0f) {
		SofQ = -1.0f/gMSAWave[0];
	} else {
		qk = qq/gMSAWave[13];
		q2k = qk*qk;
		qk2 = 1.0f/q2k;
		qk3 = qk2/qk;
		qqk = 1.0f/(qk*(q2k+ak2));
		sink = sin(qk);
		cosk = cos(qk);
		asink = akz*sink;
		qcosk = qk*cosk;
		aqk = gMSAWave[0]*(sink-qcosk);
		aqk=aqk+gMSAWave[1]*((2.0f*qk2-1.0f)*qcosk+2.0f*sink-2.0f/qk);
		inter=24.0f*qk3+4.0f*(1.0f-6.0f*qk2)*sink;
		aqk=(aqk+0.5f*etaz*gMSAWave[0]*(inter-(1.0f-12.0f*qk2+24.0f*qk2*qk2)*qcosk))*qk3;
		aqk=aqk +gMSAWave[2]*(ck*asink-sk*qcosk)*qqk;
		aqk=aqk +gMSAWave[3]*(sk*asink-qk*(ck*cosk-1.0f))*qqk;
		aqk=aqk +gMSAWave[3]*(cosk-1.0f)*qk2;
		aqk=aqk -gekz*(asink+qcosk)*qqk;
		SofQ = 1.0f/(1.0f-e24*aqk);
	}
	return (SofQ);
}


float form_volume(VOLUME_PARAMETER_DECLARATIONS);
float form_volume(VOLUME_PARAMETER_DECLARATIONS) {
    
    return 1.0f;
    
}


float Iqxy(float qx, float qy, IQXY_PARAMETER_DECLARATIONS);
float Iqxy(float qx, float qy, IQXY_PARAMETER_DECLARATIONS) {
    
    // never called since no orientation or magnetic parameters.
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
