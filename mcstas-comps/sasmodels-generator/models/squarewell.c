double Iq(double q, double radius_effective, double volfraction, double welldepth, double wellwidth)
{

// single precision is very poor at extreme small Q, would need a Taylor series
	double req,phis,edibkb,lambda,struc;
	double sigma,eta,eta2,eta3,eta4,etam1,etam14,alpha,beta,gamm;
	double x,sk,sk2,sk3,sk4,t1,t2,t3,t4,ck;
	double S,C,SL,CL;
	x= q;

	req = radius_effective;
	phis = volfraction;
	edibkb = welldepth;
	lambda = wellwidth;

	sigma = req*2.;
	eta = phis;
	eta2 = eta*eta;
	eta3 = eta*eta2;
	eta4 = eta*eta3;
	etam1 = 1. - eta;
	etam14 = etam1*etam1*etam1*etam1;
	// temp borrow sk for an intermediate calc
	sk = 1.0 +2.0*eta;
	sk *= sk;
	alpha = (  sk + eta3*( eta-4.0 )  )/etam14;
	beta = -(eta/3.0) * ( 18. + 20.*eta - 12.*eta2 + eta4 )/etam14;
	gamm = 0.5*eta*( sk + eta3*(eta-4.) )/etam14;

	//  calculate the structure factor

	sk = x*sigma;
	sk2 = sk*sk;
	sk3 = sk*sk2;
	sk4 = sk3*sk;
	SINCOS(sk,S,C);
	SINCOS(lambda*sk,SL,CL);
	t1 = alpha * sk3 * ( S - sk * C );
	t2 = beta * sk2 * 2.0*( sk*S - (0.5*sk2 - 1.)*C - 1.0 );
	t3 = gamm*( ( 4.0*sk3 - 24.*sk ) * S - ( sk4 - 12.0*sk2 + 24.0 )*C + 24.0 );
	t4 = -edibkb*sk3*(SL +sk*(C - lambda*CL) - S );
	ck =  -24.0*eta*( t1 + t2 + t3 + t4 )/sk3/sk3;
	struc  = 1./(1.-ck);

	return(struc);
}