static double
form_volume(double rg)
{
    return 1.0;
}

static double
radius_effective(int mode, double rg)
{
    switch (mode) {
    default:
    case 1: // R_g
        return rg;
    case 2: // 2R_g
        return 2.0*rg;
    case 3: // 3R_g
        return 3.0*rg;
    case 4: // (5/3)^0.5*R_g
        return sqrt(5.0/3.0)*rg;
    }
}

static double
gauss_coil(double qr)
{
    const double x = qr*qr;

    // Use series expansion at low q for higher accuracy. We could use
    // smaller polynomials if we sacrifice some digits of precision or
    // introduce an additional series expansion around x == 1.
    // See explore/precision.py, gauss_coil function.
#if FLOAT_SIZE>4 // DOUBLE_PRECISION
    // For double precision: use O(5) Pade with 0.5 cutoff (10 mad + 1 divide)
    if (x < 0.5) {
        // PadeApproximant[2*Exp[-x^2] + x^2-1)/x^4, {x, 0, 8}]
        const double A1=1./12., A2=2./99., A3=1./2640., A4=1./23760., A5=-1./1995840.;
        const double B1=5./12., B2=5./66., B3=1./132., B4=1./2376., B5=1./95040.;
        return (((((A5*x + A4)*x + A3)*x + A2)*x + A1)*x + 1.)
                / (((((B5*x + B4)*x + B3)*x + B2)*x + B1)*x + 1.);
    }
#else
    // For single precision: use O(7) Taylor with 0.8 cutoff (7 mad)
    if (x < 0.8) {
        const double C0 = +1.;
        const double C1 = -1./3.;
        const double C2 = +1./12.;
        const double C3 = -1./60.;
        const double C4 = +1./360.;
        const double C5 = -1./2520.;
        const double C6 = +1./20160.;
        const double C7 = -1./181440.;
        return ((((((C7*x + C6)*x + C5)*x + C4)*x + C3)*x + C2)*x + C1)*x + C0;
    }
#endif

    return 2.0 * (expm1(-x) + x)/(x*x);
}

static double
Iq(double q, double i_zero, double rg)
{
    return i_zero * gauss_coil(q*rg);
}
