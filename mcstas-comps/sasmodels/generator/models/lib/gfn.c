//
//     FUNCTION gfn4:    CONTAINS F(Q,A,B,MU)**2  AS GIVEN
//                       BY (53) & (58-59) IN CHEN AND
//                       KOTLARCHYK REFERENCE
//
//       <OBLATE ELLIPSOID>
// function gfn4 for oblate ellipsoids
#pragma acc routine seq
double
gfn4(double xx, double crmaj, double crmin, double trmaj, double trmin, double delpc, double delps, double qq);
double
gfn4(double xx, double crmaj, double crmin, double trmaj, double trmin, double delpc, double delps, double qq)
{
    // local variables
    const double aa = crmaj;
    const double bb = crmin;
    const double u2 = (bb*bb*xx*xx + aa*aa*(1.0-xx*xx));
    const double uq = sqrt(u2)*qq;
    // changing to more accurate sph_j1c since the following inexplicably fails on Radeon Nano.
    //const double siq = (uq == 0.0 ? 1.0 : 3.0*(sin(uq)/uq/uq - cos(uq)/uq)/uq);
    const double siq = sas_3j1x_x(uq);
    const double vc = M_4PI_3*aa*aa*bb;
    const double gfnc = siq*vc*delpc;

    const double ut2 = (trmin*trmin*xx*xx + trmaj*trmaj*(1.0-xx*xx));
    const double ut= sqrt(ut2)*qq;
    const double vt = M_4PI_3*trmaj*trmaj*trmin;
    //const double sit = (ut == 0.0 ? 1.0 : 3.0*(sin(ut)/ut/ut - cos(ut)/ut)/ut);
    const double sit = sas_3j1x_x(ut);
    const double gfnt = sit*vt*delps;

    const double tgfn = gfnc + gfnt;
    const double result = tgfn*tgfn;

    return result;
}
