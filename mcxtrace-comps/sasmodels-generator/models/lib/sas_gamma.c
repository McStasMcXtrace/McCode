/*
The wrapper for gamma function from OpenCL and standard libraries
The OpenCL gamma function fails miserably on values lower than 1.0
while works fine on larger values.
We use gamma definition Gamma(t + 1) = t * Gamma(t) to compute
to function for values lower than 1.0. Namely Gamma(t) = 1/t * Gamma(t + 1)
For t < 0, we use Gamma(t) = pi / ( Gamma(1 - t) * sin(pi * t) )
*/

#if defined(NEED_TGAMMA)
#pragma acc routine seq
static double cephes_stirf(double x)
{
	const double MAXSTIR=143.01608;
	const double SQTPI=2.50662827463100050242E0;
	double y, w, v;

	w = 1.0 / x;

	w = ((((
		7.87311395793093628397E-4*w
		- 2.29549961613378126380E-4)*w
		- 2.68132617805781232825E-3)*w
		+ 3.47222221605458667310E-3)*w
		+ 8.33333333333482257126E-2)*w
		+ 1.0;
	y = exp(x);
	if (x > MAXSTIR)
	{ /* Avoid overflow in pow() */
		v = pow(x, 0.5 * x - 0.25);
		y = v * (v / y);
	}
	else
	{
		y = pow(x, x - 0.5) / y;
	}
	y = SQTPI * y * w;
	return(y);
}

#pragma acc routine seq
static double tgamma(double x) {
	double p, q, z;
	int sgngam;
	int i;

	sgngam = 1;
	if (isnan(x))
		return(x);
	q = fabs(x);

	if (q > 33.0)
	{
		if (x < 0.0)
		{
			p = floor(q);
			if (p == q)
			{
				return (NAN);
			}
			i = p;
			if ((i & 1) == 0)
				sgngam = -1;
			z = q - p;
			if (z > 0.5)
			{
				p += 1.0;
				z = q - p;
			}
			z = q * sin(M_PI * z);
			if (z == 0.0)
			{
				return(NAN);
			}
			z = fabs(z);
			z = M_PI / (z * cephes_stirf(q));
		}
		else
		{
			z = cephes_stirf(x);
		}
		return(sgngam * z);
	}

	z = 1.0;
	while (x >= 3.0)
	{
		x -= 1.0;
		z *= x;
	}

	while (x < 0.0)
	{
		if (x > -1.E-9)
			goto small;
		z /= x;
		x += 1.0;
	}

	while (x < 2.0)
	{
		if (x < 1.e-9)
			goto small;
		z /= x;
		x += 1.0;
	}

	if (x == 2.0)
		return(z);

	x -= 2.0;
	p = (((((
		+1.60119522476751861407E-4*x
		+ 1.19135147006586384913E-3)*x
		+ 1.04213797561761569935E-2)*x
		+ 4.76367800457137231464E-2)*x
		+ 2.07448227648435975150E-1)*x
		+ 4.94214826801497100753E-1)*x
		+ 9.99999999999999996796E-1;
	q = ((((((
		-2.31581873324120129819E-5*x
		+ 5.39605580493303397842E-4)*x
		- 4.45641913851797240494E-3)*x
		+ 1.18139785222060435552E-2)*x
		+ 3.58236398605498653373E-2)*x
		- 2.34591795718243348568E-1)*x
		+ 7.14304917030273074085E-2)*x
		+ 1.00000000000000000320E0;
	return(z * p / q);

small:
	if (x == 0.0)
	{
		return (NAN);
	}
	else
		return(z / ((1.0 + 0.5772156649015329 * x) * x));
}
#endif // NEED_TGAMMA

#pragma acc routine seq
inline double sas_gamma(double x)
{
    // Note: the builtin tgamma can give slow and unreliable results for x<1.
    // The following transform extends it to zero and to negative values.
    // It should return NaN for zero and negative integers but doesn't.
    // The accuracy is okay but not wonderful for negative numbers, maybe
    // one or two digits lost in the calculation. If higher accuracy is
    // needed, you could test the following loop:
    //    double norm = 1.;
    //    while (x<1.) { norm*=x; x+=1.; }
    //    return tgamma(x)/norm;
    return (x<0. ? M_PI/tgamma(1.-x)/sin(M_PI*x) : tgamma(x+1)/x);
}
