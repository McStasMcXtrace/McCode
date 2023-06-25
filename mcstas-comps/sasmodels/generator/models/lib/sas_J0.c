/*							j0.c
 *
 *	Bessel function of order zero
 *
 *
 *
 * SYNOPSIS:
 *
 * double x, y, j0();
 *
 * y = j0( x );
 *
 *
 *
 * DESCRIPTION:
 *
 * Returns Bessel function of order zero of the argument.
 *
 * The domain is divided into the intervals [0, 5] and
 * (5, infinity). In the first interval the following rational
 * approximation is used:
 *
 *
 *        2         2
 * (w - r  ) (w - r  ) P (w) / Q (w)
 *       1         2    3       8
 *
 *            2
 * where w = x  and the two r's are zeros of the function.
 *
 * In the second interval, the Hankel asymptotic expansion
 * is employed with two rational functions of degree 6/6
 * and 7/7.
 *
 *
 *
 * ACCURACY:
 *
 *                      Absolute error:
 * arithmetic   domain     # trials      peak         rms
 *    DEC       0, 30       10000       4.4e-17     6.3e-18
 *    IEEE      0, 30       60000       4.2e-16     1.1e-16
 *
 */

/*
Cephes Math Library Release 2.8:  June, 2000
Copyright 1984, 1987, 1989, 2000 by Stephen L. Moshier
*/

/* Note: all coefficients satisfy the relative error criterion
 * except YP, YQ which are designed for absolute error. */

#if FLOAT_SIZE>4
//Cephes double precission
double cephes_j0(double x);

 constant double PPJ0[8] = {
        7.96936729297347051624E-4,
        8.28352392107440799803E-2,
        1.23953371646414299388E0,
        5.44725003058768775090E0,
        8.74716500199817011941E0,
        5.30324038235394892183E0,
        9.99999999999999997821E-1,
        0.0
    };

 constant double PQJ0[8] = {
        9.24408810558863637013E-4,
        8.56288474354474431428E-2,
        1.25352743901058953537E0,
        5.47097740330417105182E0,
        8.76190883237069594232E0,
        5.30605288235394617618E0,
        1.00000000000000000218E0,
        0.0
    };

 constant double QPJ0[8] = {
        -1.13663838898469149931E-2,
        -1.28252718670509318512E0,
        -1.95539544257735972385E1,
        -9.32060152123768231369E1,
        -1.77681167980488050595E2,
        -1.47077505154951170175E2,
        -5.14105326766599330220E1,
        -6.05014350600728481186E0,
    };

 constant double QQJ0[8] = {
        /*  1.00000000000000000000E0,*/
        6.43178256118178023184E1,
        8.56430025976980587198E2,
        3.88240183605401609683E3,
        7.24046774195652478189E3,
        5.93072701187316984827E3,
        2.06209331660327847417E3,
        2.42005740240291393179E2,
    };

 constant double YPJ0[8] = {
        1.55924367855235737965E4,
        -1.46639295903971606143E7,
        5.43526477051876500413E9,
        -9.82136065717911466409E11,
        8.75906394395366999549E13,
        -3.46628303384729719441E15,
        4.42733268572569800351E16,
        -1.84950800436986690637E16,
 };


 constant double YQJ0[7] = {
        /* 1.00000000000000000000E0,*/
        1.04128353664259848412E3,
        6.26107330137134956842E5,
        2.68919633393814121987E8,
        8.64002487103935000337E10,
        2.02979612750105546709E13,
        3.17157752842975028269E15,
        2.50596256172653059228E17,
  };

 constant double RPJ0[8] = {
        -4.79443220978201773821E9,
        1.95617491946556577543E12,
        -2.49248344360967716204E14,
        9.70862251047306323952E15,
        0.0,
        0.0,
        0.0,
        0.0
  };

 constant double RQJ0[8] = {
        /* 1.00000000000000000000E0,*/
        4.99563147152651017219E2,
        1.73785401676374683123E5,
        4.84409658339962045305E7,
        1.11855537045356834862E10,
        2.11277520115489217587E12,
        3.10518229857422583814E14,
        3.18121955943204943306E16,
        1.71086294081043136091E18,
  };

#pragma acc declare copyin( PPJ0[0:8], PQJ0[0:8], QPJ0[0:8], QQJ0[0:8], YPJ0[0:8], YQJ0[0:7], RPJ0[0:8], RQJ0[0:8])

#pragma acc routine seq
double cephes_j0(double x)
{
    double w, z, p, q, xn;

    //const double TWOOPI = 6.36619772367581343075535E-1;
    const double SQ2OPI = 7.9788456080286535587989E-1;
    const double PIO4 = 7.85398163397448309616E-1;

    const double DR1 = 5.78318596294678452118E0;
    const double DR2 = 3.04712623436620863991E1;


    if( x < 0 )
        x = -x;

    if( x <= 5.0 ) {
        z = x * x;
        if( x < 1.0e-5 )
            return( 1.0 - z/4.0 );

        p = (z - DR1) * (z - DR2);
        p = p * polevl( z, RPJ0, 3)/p1evl( z, RQJ0, 8 );
        return( p );
    }

    w = 5.0/x;
    q = 25.0/(x*x);
    p = polevl( q, PPJ0, 6)/polevl( q, PQJ0, 6 );
    q = polevl( q, QPJ0, 7)/p1evl( q, QQJ0, 7 );
    xn = x - PIO4;

    double sn, cn;
    SINCOS(xn, sn, cn);
    p = p * cn - w * q * sn;

    return( p * SQ2OPI / sqrt(x) );
}
#else
//Cephes single precission

#pragma acc routine seq
float cephes_j0f(float x);

 constant float MOJ0[8] = {
        -6.838999669318810E-002,
        1.864949361379502E-001,
        -2.145007480346739E-001,
        1.197549369473540E-001,
        -3.560281861530129E-003,
        -4.969382655296620E-002,
        -3.355424622293709E-006,
        7.978845717621440E-001
  };

 constant float PHJ0[8] = {
        3.242077816988247E+001,
        -3.630592630518434E+001,
        1.756221482109099E+001,
        -4.974978466280903E+000,
        1.001973420681837E+000,
        -1.939906941791308E-001,
        6.490598792654666E-002,
        -1.249992184872738E-001
  };

 constant float JPJ0[8] = {
        -6.068350350393235E-008,
        6.388945720783375E-006,
        -3.969646342510940E-004,
        1.332913422519003E-002,
        -1.729150680240724E-001,
        0.0,
        0.0,
        0.0
 };

#pragma acc declare copyin( MOJ0[0:8], PHJ0[0:8], JPJ0[0:8])

#pragma acc routine seq
float cephes_j0f(float x)
{
    float xx, w, z, p, q, xn;

    //const double YZ1 =  0.43221455686510834878;
    //const double YZ2 = 22.401876406482861405;
    //const double YZ3 = 64.130620282338755553;
    const float DR1 =  5.78318596294678452118;
    const float PIO4F = 0.7853981633974483096;

    if( x < 0 )
        xx = -x;
    else
        xx = x;

    // 2017-05-18 PAK - support negative x
    if( xx <= 2.0 ) {
        z = xx * xx;
        if( xx < 1.0e-3 )
            return( 1.0 - 0.25*z );

        p = (z-DR1) * polevl( z, JPJ0, 4);
        return( p );
    }

    q = 1.0/xx;
    w = sqrt(q);

    p = w * polevl( q, MOJ0, 7);
    w = q*q;
    xn = q * polevl( w, PHJ0, 7) - PIO4F;
    p = p * cos(xn + xx);
    return(p);
}
#endif

#if FLOAT_SIZE>4
#define sas_J0 cephes_j0
#else
#define sas_J0 cephes_j0f
#endif
