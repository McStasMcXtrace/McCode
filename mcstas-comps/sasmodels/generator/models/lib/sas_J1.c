/*							j1.c
 *
 *	Bessel function of order one
 *
 *
 *
 * SYNOPSIS:
 *
 * double x, y, j1();
 *
 * y = j1( x );
 *
 *
 *
 * DESCRIPTION:
 *
 * Returns Bessel function of order one of the argument.
 *
 * The domain is divided into the intervals [0, 8] and
 * (8, infinity). In the first interval a 24 term Chebyshev
 * expansion is used. In the second, the asymptotic
 * trigonometric representation is employed using two
 * rational functions of degree 5/5.
 *
 *
 *
 * ACCURACY:
 *
 *                      Absolute error:
 * arithmetic   domain      # trials      peak         rms
 *    DEC       0, 30       10000       4.0e-17     1.1e-17
 *    IEEE      0, 30       30000       2.6e-16     1.1e-16
 *
 *
 */

/*
Cephes Math Library Release 2.8:  June, 2000
Copyright 1984, 1987, 1989, 2000 by Stephen L. Moshier
*/

#if FLOAT_SIZE>4
//Cephes double pression function

constant double RPJ1[8] = {
    -8.99971225705559398224E8,
    4.52228297998194034323E11,
    -7.27494245221818276015E13,
    3.68295732863852883286E15,
    0.0,
    0.0,
    0.0,
    0.0 };

constant double RQJ1[8] = {
    6.20836478118054335476E2,
    2.56987256757748830383E5,
    8.35146791431949253037E7,
    2.21511595479792499675E10,
    4.74914122079991414898E12,
    7.84369607876235854894E14,
    8.95222336184627338078E16,
    5.32278620332680085395E18
    };

constant double PPJ1[8] = {
    7.62125616208173112003E-4,
    7.31397056940917570436E-2,
    1.12719608129684925192E0,
    5.11207951146807644818E0,
    8.42404590141772420927E0,
    5.21451598682361504063E0,
    1.00000000000000000254E0,
    0.0} ;


constant double PQJ1[8] = {
    5.71323128072548699714E-4,
    6.88455908754495404082E-2,
    1.10514232634061696926E0,
    5.07386386128601488557E0,
    8.39985554327604159757E0,
    5.20982848682361821619E0,
    9.99999999999999997461E-1,
    0.0 };

constant double QPJ1[8] = {
    5.10862594750176621635E-2,
    4.98213872951233449420E0,
    7.58238284132545283818E1,
    3.66779609360150777800E2,
    7.10856304998926107277E2,
    5.97489612400613639965E2,
    2.11688757100572135698E2,
    2.52070205858023719784E1 };

constant double QQJ1[8] = {
    7.42373277035675149943E1,
    1.05644886038262816351E3,
    4.98641058337653607651E3,
    9.56231892404756170795E3,
    7.99704160447350683650E3,
    2.82619278517639096600E3,
    3.36093607810698293419E2,
    0.0 };

#pragma acc declare copyin( RPJ1[0:8], RQJ1[0:8], PPJ1[0:8], PQJ1[0:8], QPJ1[0:8], QQJ1[0:8])

#pragma acc routine seq
static
double cephes_j1(double x)
{

    double w, z, p, q, abs_x, sign_x;

    const double Z1 = 1.46819706421238932572E1;
    const double Z2 = 4.92184563216946036703E1;

    // 2017-05-18 PAK - mathematica and mpmath use J1(-x) = -J1(x)
    if (x < 0) {
        abs_x = -x;
        sign_x = -1.0;
    } else {
        abs_x = x;
        sign_x = 1.0;
    }

    if( abs_x <= 5.0 ) {
        z = abs_x * abs_x;
        w = polevl( z, RPJ1, 3 ) / p1evl( z, RQJ1, 8 );
        w = w * abs_x * (z - Z1) * (z - Z2);
        return( sign_x * w );
    }

    w = 5.0/abs_x;
    z = w * w;
    p = polevl( z, PPJ1, 6)/polevl( z, PQJ1, 6 );
    q = polevl( z, QPJ1, 7)/p1evl( z, QQJ1, 7 );

    // 2017-05-19 PAK improve accuracy using trig identies
    // original:
    //    const double THPIO4 =  2.35619449019234492885;
    //    const double SQ2OPI = 0.79788456080286535588;
    //    double sin_xn, cos_xn;
    //    SINCOS(abs_x - THPIO4, sin_xn, cos_xn);
    //    p = p * cos_xn - w * q * sin_xn;
    //    return( sign_x * p * SQ2OPI / sqrt(abs_x) );
    // expanding p*cos(a - 3 pi/4) - wq sin(a - 3 pi/4)
    //    [ p(sin(a) - cos(a)) + wq(sin(a) + cos(a)) / sqrt(2)
    // note that sqrt(1/2) * sqrt(2/pi) = sqrt(1/pi)
    const double SQRT1_PI = 0.56418958354775628;
    double sin_x, cos_x;
    SINCOS(abs_x, sin_x, cos_x);
    p = p*(sin_x - cos_x) + w*q*(sin_x + cos_x);
    return( sign_x * p * SQRT1_PI / sqrt(abs_x) );
}

#else
//Single precission version of cephes

constant float JPJ1[8] = {
    -4.878788132172128E-009,
    6.009061827883699E-007,
    -4.541343896997497E-005,
    1.937383947804541E-003,
    -3.405537384615824E-002,
    0.0,
    0.0,
    0.0
    };

constant float MO1J1[8] = {
    6.913942741265801E-002,
    -2.284801500053359E-001,
    3.138238455499697E-001,
    -2.102302420403875E-001,
    5.435364690523026E-003,
    1.493389585089498E-001,
    4.976029650847191E-006,
    7.978845453073848E-001
    };

constant float PH1J1[8] = {
    -4.497014141919556E+001,
    5.073465654089319E+001,
    -2.485774108720340E+001,
    7.222973196770240E+000,
    -1.544842782180211E+000,
    3.503787691653334E-001,
    -1.637986776941202E-001,
    3.749989509080821E-001
    };

#pragma acc declare copyin( JPJ1[0:8], MO1J1[0:8], PH1J1[0:8])

#pragma acc routine seq
static
float cephes_j1f(float xx)
{

    float x, w, z, p, q, xn;

    const float Z1 = 1.46819706421238932572E1;


    // 2017-05-18 PAK - mathematica and mpmath use J1(-x) = -J1(x)
    x = xx;
    if( x < 0 )
        x = -xx;

    if( x <= 2.0 ) {
        z = x * x;
        p = (z-Z1) * x * polevl( z, JPJ1, 4 );
        return( xx < 0. ? -p : p );
    }

    q = 1.0/x;
    w = sqrt(q);

    p = w * polevl( q, MO1J1, 7);
    w = q*q;
    // 2017-05-19 PAK improve accuracy using trig identies
    // original:
    //    const float THPIO4F =  2.35619449019234492885;    /* 3*pi/4 */
    //    xn = q * polevl( w, PH1J1, 7) - THPIO4F;
    //    p = p * cos(xn + x);
    //    return( xx < 0. ? -p : p );
    // expanding cos(a + b - 3 pi/4) is
    //    [sin(a)sin(b) + sin(a)cos(b) + cos(a)sin(b)-cos(a)cos(b)] / sqrt(2)
    xn = q * polevl( w, PH1J1, 7);
    float cos_xn, sin_xn;
    float cos_x, sin_x;
    SINCOS(xn, sin_xn, cos_xn);  // about xn and 1
    SINCOS(x, sin_x, cos_x);
    p *= M_SQRT1_2*(sin_xn*(sin_x+cos_x) + cos_xn*(sin_x-cos_x));

    return( xx < 0. ? -p : p );
}
#endif

#if FLOAT_SIZE>4
#define sas_J1 cephes_j1
#else
#define sas_J1 cephes_j1f
#endif

//Finally J1c function that equals 2*J1(x)/x
    
#pragma acc routine seq
static
double sas_2J1x_x(double x)
{
    return (x != 0.0 ) ? 2.0*sas_J1(x)/x : 1.0;
}
