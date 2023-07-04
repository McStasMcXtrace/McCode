/**
* Spherical Bessel function 3*j1(x)/x
*
* Used for low q to avoid cancellation error.
* Note that the values differ from sasview ~ 5e-12 rather than 5e-14, but
* in this case it is likely cancellation errors in the original expression
* using double precision that are the source.
*/
double sas_3j1x_x(double q);

// The choice of the number of terms in the series and the cutoff value for
// switching between series and direct calculation depends on the numeric
// precision.
//
// Point where direct calculation reaches machine precision:
//
//   single machine precision eps 3e-8 at qr=1.1 **
//   double machine precision eps 4e-16 at qr=1.1
//
// Point where Taylor series reaches machine precision (eps), where taylor
// series matches direct calculation (cross) and the error at that point:
//
//   prec   n eps  cross  error
//   single 3 0.28  0.4   6.2e-7
//   single 4 0.68  0.7   2.3e-7
//   single 5 1.18  1.2   7.5e-8
//   double 3 0.01  0.03  2.3e-13
//   double 4 0.06  0.1   3.1e-14
//   double 5 0.16  0.2   5.0e-15
//
// ** Note: relative error on single precision starts increase on the direct
// method at qr=1.1, rising from 3e-8 to 5e-5 by qr=1e3.  This should be
// safe for the sans range, with objects of 100 nm supported to a q of 0.1
// while maintaining 5 digits of precision.  For usans/sesans, the objects
// are larger but the q is smaller, so again it should be fine.
//
// See explore/sph_j1c.py for code to explore these ranges.

// Use 4th order series
#if FLOAT_SIZE>4
#define SPH_J1C_CUTOFF 0.1
#else
#define SPH_J1C_CUTOFF 0.7
#endif
#pragma acc routine seq
double sas_3j1x_x(double q)
{
    // 2017-05-18 PAK - support negative q
    if (fabs(q) < SPH_J1C_CUTOFF) {
        const double q2 = q*q;
        return (1.0 + q2*(-3./30. + q2*(3./840. + q2*(-3./45360.))));// + q2*(3./3991680.)))));
    } else {
        double sin_q, cos_q;
        SINCOS(q, sin_q, cos_q);
        return 3.0*(sin_q/q - cos_q)/(q*q);
    }
}
