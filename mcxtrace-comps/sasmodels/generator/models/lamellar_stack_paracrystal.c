/*	Lamellar_ParaCrystal - Pedersen's model

*/
double paraCryst_sn(double ww, double qval, double davg, int Nlayers, double an);
double paraCryst_an(double ww, double qval, double davg, int Nlayers);

static double
Iq(double qval,
   double th,
   double fp_Nlayers,
   double davg,
   double pd,
   double sld,
   double solvent_sld)
{
	//get the fractional part of Nlayers, to determine the "mixing" of N's
	int n1 = (int)(fp_Nlayers);		//truncate towards zero
	int n2 = n1 + 1;
	const double xn = (double)n2 - fp_Nlayers;	//fractional contribution of n1

	const double ww = exp(-0.5*square(qval*pd*davg));

	//calculate the n1 contribution
	double Znq,Snq,an;
	an = paraCryst_an(ww,qval,davg,n1);
	Snq = paraCryst_sn(ww,qval,davg,n1,an);

	Znq = xn*Snq;

	//calculate the n2 contribution
	an = paraCryst_an(ww,qval,davg,n2);
	Snq = paraCryst_sn(ww,qval,davg,n2,an);

	Znq += (1.0-xn)*Snq;

	//and the independent contribution
	Znq += (1.0-ww*ww)/(1.0+ww*ww-2.0*ww*cos(qval*davg));

	//the limit when Nlayers approaches infinity
//	Zq = (1-ww^2)/(1+ww^2-2*ww*cos(qval*davg))

	const double xi = th/2.0;		//use 1/2 the bilayer thickness
	const double Pbil = square(sas_sinx_x(qval*xi));

	const double contr = sld - solvent_sld;
	const double inten = 2.0*M_PI*contr*contr*Pbil*Znq/(qval*qval);
//printf("q=%.7e wwm1=%g ww=%.5e an=% 12.5e Snq=% 12.5e Znq=% 12.5e Pbil=% 12.5e\n",qval,wwm1,ww,an,Snq,Znq,Pbil);
	return 1.0e-4*inten;
}

// functions for the lamellar paracrystal model
double
paraCryst_sn(double ww, double qval, double davg, int Nlayers, double an) {

	double Snq;

	Snq = an/( (double)Nlayers*square(1.0+ww*ww-2.0*ww*cos(qval*davg)) );

	return Snq;
}

double
paraCryst_an(double ww, double qval, double davg, int Nlayers) {
	double an;

	an = 4.0*ww*ww - 2.0*(ww*ww*ww+ww)*cos(qval*davg);
	an -= 4.0*pow(ww,(Nlayers+2))*cos((double)Nlayers*qval*davg);
	an += 2.0*pow(ww,(Nlayers+3))*cos((double)(Nlayers-1)*qval*davg);
	an += 2.0*pow(ww,(Nlayers+1))*cos((double)(Nlayers+1)*qval*davg);

	return an;
}

