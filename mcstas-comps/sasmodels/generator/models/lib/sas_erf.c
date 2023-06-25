
/*
 * Cephes Math Library Release 2.2:  June, 1992
 * Copyright 1984, 1987, 1988, 1992 by Stephen L. Moshier
 * Direct inquiries to 30 Frost Street, Cambridge, MA 02140
 */
/*
 *
 *	Error function
 *
 *
 *
 * SYNOPSIS:
 *
 * double x, y, erf();
 *
 * y = erf( x );
 *
 *
 *
 * DESCRIPTION:
 *
 * The integral is
 *
 *                           x
 *                            -
 *                 2         | |          2
 *   erf(x)  =  --------     |    exp( - t  ) dt.
 *              sqrt(pi)   | |
 *                          -
 *                           0
 *
 * For 0 <= |x| < 1, erf(x) = x * P4(x**2)/Q5(x**2); otherwise
 * erf(x) = 1 - erfc(x).
 *
 *
 *
 * ACCURACY:
 *
 *                      Relative error:
 * arithmetic   domain     # trials      peak         rms
 *    IEEE      0,1         30000       3.7e-16     1.0e-16
 *
 */
/*
 *
 *	Complementary error function
 *
 *
 *
 * SYNOPSIS:
 *
 * double x, y, erfc();
 *
 * y = erfc( x );
 *
 *
 *
 * DESCRIPTION:
 *
 *
 *  1 - erf(x) =
 *
 *                           inf.
 *                             -
 *                  2         | |          2
 *   erfc(x)  =  --------     |    exp( - t  ) dt
 *               sqrt(pi)   | |
 *                           -
 *                            x
 *
 *
 * For small x, erfc(x) = 1 - erf(x); otherwise rational
 * approximations are computed.
 *
 *
 *
 * ACCURACY:
 *
 *                      Relative error:
 * arithmetic   domain     # trials      peak         rms
 *    IEEE      0,26.6417   30000       5.7e-14     1.5e-14
 */

#ifdef NEED_ERF

#if FLOAT_SIZE>4  // DOUBLE_PRECISION

double cephes_erf(double x);
double cephes_erfc(double a);

constant double PD[] = {
    2.46196981473530512524E-10,
    5.64189564831068821977E-1,
    7.46321056442269912687E0,
    4.86371970985681366614E1,
    1.96520832956077098242E2,
    5.26445194995477358631E2,
    9.34528527171957607540E2,
    1.02755188689515710272E3,
    5.57535335369399327526E2
};

constant double QD[] = {
    /* 1.00000000000000000000E0, */
    1.32281951154744992508E1,
    8.67072140885989742329E1,
    3.54937778887819891062E2,
    9.75708501743205489753E2,
    1.82390916687909736289E3,
    2.24633760818710981792E3,
    1.65666309194161350182E3,
    5.57535340817727675546E2
};

constant double RD[] = {
    5.64189583547755073984E-1,
    1.27536670759978104416E0,
    5.01905042251180477414E0,
    6.16021097993053585195E0,
    7.40974269950448939160E0,
    2.97886665372100240670E0
};

constant double SD[] = {
    /* 1.00000000000000000000E0, */
    2.26052863220117276590E0,
    9.39603524938001434673E0,
    1.20489539808096656605E1,
    1.70814450747565897222E1,
    9.60896809063285878198E0,
    3.36907645100081516050E0
};

constant double TD[] = {
    9.60497373987051638749E0,
    9.00260197203842689217E1,
    2.23200534594684319226E3,
    7.00332514112805075473E3,
    5.55923013010394962768E4
};

constant double UD[] = {
    /* 1.00000000000000000000E0, */
    3.35617141647503099647E1,
    5.21357949780152679795E2,
    4.59432382970980127987E3,
    2.26290000613890934246E4,
    4.92673942608635921086E4
};

#pragma acc declare copyin(PD[0:1], QD[0:1], RD[0:1], SD[0:1], TD[0:1], UD[0:1])

#pragma acc routine seq
double cephes_erfc(double a)
{
    double MAXLOG = 88.72283905206835;
    double p, q, x, y, z;


    x = fabs(a);

    if (x < 1.0) {
        // The line below causes problems on the GPU, so inline
        // the erf function instead and z < 1.0.
        //return (1.0 - cephes_erf(a));
        // 2017-05-18 PAK - use erf(a) rather than erf(|a|)
        z = a * a;
        y = a * polevl(z, TD, 4) / p1evl(z, UD, 5);

        return 1.0 - y;
    }

    z = -a * a;

    if (z < -MAXLOG) {
        if (a < 0)
            return (2.0);
        else
            return (0.0);
    }

    z = exp(z);


    if (x < 8.0) {
        p = polevl(x, PD, 8);
        q = p1evl(x, QD, 8);
    }
    else {
        p = polevl(x, RD, 5);
        q = p1evl(x, SD, 6);
    }
    y = (z * p) / q;

    if (a < 0)
        y = 2.0 - y;

    if (y == 0.0) {
        if (a < 0)
            return (2.0);
        else
            return (0.0);
    }
    return y;
}


double cephes_erf(double x)
{
    double y, z;

    if (fabs(x) > 1.0)
        return (1.0 - cephes_erfc(x));

    z = x * x;
    y = x * polevl(z, TD, 4) / p1evl(z, UD, 5);

    return y;
}

#else // SINGLE PRECISION

float cephes_erff(float x);
float cephes_erfcf(float a);

/* erfc(x) = exp(-x^2) P(1/x), 1 < x < 2 */
constant float PF[] = {
    2.326819970068386E-002,
    -1.387039388740657E-001,
    3.687424674597105E-001,
    -5.824733027278666E-001,
    6.210004621745983E-001,
    -4.944515323274145E-001,
    3.404879937665872E-001,
    -2.741127028184656E-001,
    5.638259427386472E-001
};

/* erfc(x) = exp(-x^2) 1/x P(1/x^2), 2 < x < 14 */
constant float RF[] = {
    -1.047766399936249E+001,
    1.297719955372516E+001,
    -7.495518717768503E+000,
    2.921019019210786E+000,
    -1.015265279202700E+000,
    4.218463358204948E-001,
    -2.820767439740514E-001,
    5.641895067754075E-001
};

/* erf(x) = x P(x^2), 0 < x < 1 */
 constant float TF[] = {
    7.853861353153693E-005,
    -8.010193625184903E-004,
    5.188327685732524E-003,
    -2.685381193529856E-002,
    1.128358514861418E-001,
    -3.761262582423300E-001,
    1.128379165726710E+000
};

#pragma acc declare copyin(PF[0:1], RF[0:1], TF[0:1])

#pragma acc routine seq
float cephes_erfcf(float a)
{
    float MAXLOG = 88.72283905206835;
    float p, q, x, y, z;


    /*if (a < 0.0)
        x = -a;
    else
        x = a;*/
    // TODO: tinycc does not support fabsf
    x = fabs(a);


    if (x < 1.0) {
        //The line below is a troublemaker for GPU, so sas_erf function
        //is explicit here for the case < 1.0
        //return (1.0 - sas_erf(a));
        // 2017-05-18 PAK - use erf(a) rather than erf(|a|)
        z = a * a;
        y = a * polevl( z, TF, 6 );

        return 1.0 - y;
    }

    z = -a * a;

    if (z < -MAXLOG) {
        if (a < 0)
            return (2.0);
        else
            return (0.0);
    }
    z = expf(z);


    q=1.0/x;
    y=q*q;
    if( x < 2.0 ) {
        p = polevl( y, PF, 8 );
    } else {
        p = polevl( y, RF, 7 );
    }
    y = z * q * p;

    if (a < 0)
        y = 2.0 - y;

    if (y == 0.0) {
        if (a < 0)
            return (2.0);
        else
            return (0.0);
    }
    return y;
}

#pragma acc routine seq
float cephes_erff(float x)
{
    float y, z;

    // TODO: tinycc does not support fabsf
    if (fabs(x) > 1.0)
        return (1.0 - cephes_erfcf(x));

    z = x * x;
    y = x * polevl( z, TF, 6 );

    return y;
}

#endif // SINGLE_PRECISION

#if FLOAT_SIZE>4
//static double sas_erf(double x) { return erf(x); }
//static double sas_erfc(double x) { return erfc(x); }
#define sas_erf cephes_erf
#define sas_erfc cephes_erfc
#else
#define sas_erf cephes_erff
#define sas_erfc cephes_erfcf
#endif

#else // !NEED_ERF

#if FLOAT_SIZE>4
//static double sas_erf(double x) { return erf(x); }
//static double sas_erfc(double x) { return erfc(x); }
#define sas_erf erf
#define sas_erfc erfc
#else
#define sas_erf erff
#define sas_erfc erfcf
#endif
#endif // !NEED_ERF
