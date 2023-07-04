static double
form_volume(double radius_polar, double radius_equatorial)
{
    return M_4PI_3*radius_polar*radius_equatorial*radius_equatorial;
}

static double
radius_from_volume(double radius_polar, double radius_equatorial)
{
    return cbrt(radius_polar*radius_equatorial*radius_equatorial);
}

static double
radius_from_curvature(double radius_polar, double radius_equatorial)
{
    // Trivial cases
    if (radius_polar == radius_equatorial) return radius_polar;
    if (radius_polar * radius_equatorial == 0.)  return 0.;

    // see equation (26) in A.Isihara, J.Chem.Phys. 18(1950)1446-1449
    const double ratio = (radius_polar < radius_equatorial
                          ? radius_polar / radius_equatorial
                          : radius_equatorial / radius_polar);
    const double e1 = sqrt(1.0 - ratio*ratio);
    const double b1 = 1.0 + asin(e1) / (e1 * ratio);
    const double bL = (1.0 + e1) / (1.0 - e1);
    const double b2 = 1.0 + 0.5 * ratio * ratio / e1 * log(bL);
    const double delta = 0.75 * b1 * b2;
    const double ddd = 2.0 * (delta + 1.0) * radius_polar * radius_equatorial * radius_equatorial;
    return 0.5 * cbrt(ddd);
}

static double
radius_effective(int mode, double radius_polar, double radius_equatorial)
{
    switch (mode) {
    default:
    case 1: // average curvature
        return radius_from_curvature(radius_polar, radius_equatorial);
    case 2: // equivalent volume sphere
        return radius_from_volume(radius_polar, radius_equatorial);
    case 3: // min radius
        return (radius_polar < radius_equatorial ? radius_polar : radius_equatorial);
    case 4: // max radius
        return (radius_polar > radius_equatorial ? radius_polar : radius_equatorial);
    }
}


static void
Fq(double q,
    double *F1,
    double *F2,
    double sld,
    double sld_solvent,
    double radius_polar,
    double radius_equatorial)
{
    // Using ratio v = Rp/Re, we can implement the form given in Guinier (1955)
    //     i(h) = int_0^pi/2 Phi^2(h a sqrt(cos^2 + v^2 sin^2) cos dT
    //          = int_0^pi/2 Phi^2(h a sqrt((1-sin^2) + v^2 sin^2) cos dT
    //          = int_0^pi/2 Phi^2(h a sqrt(1 + sin^2(v^2-1)) cos dT
    // u-substitution of
    //     u = sin, du = cos dT
    //     i(h) = int_0^1 Phi^2(h a sqrt(1 + u^2(v^2-1)) du
    const double v_square_minus_one = square(radius_polar/radius_equatorial) - 1.0;
    // translate a point in [-1,1] to a point in [0, 1]
    // const double u = GAUSS_Z[i]*(upper-lower)/2 + (upper+lower)/2;
    const double zm = 0.5;
    const double zb = 0.5;
    double total_F2 = 0.0;
    double total_F1 = 0.0;
    for (int i=0;i<GAUSS_N;i++) {
        const double u = GAUSS_Z[i]*zm + zb;
        const double r = radius_equatorial*sqrt(1.0 + u*u*v_square_minus_one);
        const double f = sas_3j1x_x(q*r);
        total_F2 += GAUSS_W[i] * f * f;
        total_F1 += GAUSS_W[i] * f;
    }
    // translate dx in [-1,1] to dx in [lower,upper]
    total_F1 *= zm;
    total_F2 *= zm;
    const double s = (sld - sld_solvent) * form_volume(radius_polar, radius_equatorial);
    *F1 = 1e-2 * s * total_F1;
    *F2 = 1e-4 * s * s * total_F2;
}

static double
Iqac(double qab, double qc,
    double sld,
    double sld_solvent,
    double radius_polar,
    double radius_equatorial)
{
    const double qr = sqrt(square(radius_equatorial*qab) + square(radius_polar*qc));
    const double f = sas_3j1x_x(qr);
    const double s = (sld - sld_solvent) * form_volume(radius_polar, radius_equatorial);

    return 1.0e-4 * square(f * s);
}
