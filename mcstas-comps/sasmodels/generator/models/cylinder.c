static double
form_volume(double radius, double length)
{
    return M_PI*radius*radius*length;
}

static double
_fq(double qab, double qc, double radius, double length)
{
    return sas_2J1x_x(qab*radius) * sas_sinx_x(qc*0.5*length);
}

static double
radius_from_excluded_volume(double radius, double length)
{
    return 0.5*cbrt(0.75*radius*(2.0*radius*length
           + (radius + length)*(M_PI*radius + length)));
}

static double
radius_from_volume(double radius, double length)
{
    return cbrt(M_PI*radius*radius*length/M_4PI_3);
}

static double
radius_from_diagonal(double radius, double length)
{
    return sqrt(radius*radius + 0.25*length*length);
}

static double
radius_effective(int mode, double radius, double length)
{
    switch (mode) {
    default:
    case 1:
        return radius_from_excluded_volume(radius, length);
    case 2:
        return radius_from_volume(radius, length);
    case 3:
        return radius;
    case 4:
        return 0.5*length;
    case 5:
        return (radius < 0.5*length ? radius : 0.5*length);
    case 6:
        return (radius > 0.5*length ? radius : 0.5*length);
    case 7:
        return radius_from_diagonal(radius,length);
    }
}

static void
Fq(double q,
    double *F1,
    double *F2,
    double sld,
    double solvent_sld,
    double radius,
    double length)
{
    // translate a point in [-1,1] to a point in [0, pi/2]
    const double zm = M_PI_4;
    const double zb = M_PI_4;

    double total_F1 = 0.0;
    double total_F2 = 0.0;
    for (int i=0; i<GAUSS_N ;i++) {
        const double theta = GAUSS_Z[i]*zm + zb;
        double sin_theta, cos_theta; // slots to hold sincos function output
        // theta (theta,phi) the projection of the cylinder on the detector plane
        SINCOS(theta , sin_theta, cos_theta);
        const double form = _fq(q*sin_theta, q*cos_theta, radius, length);
        total_F1 += GAUSS_W[i] * form * sin_theta;
        total_F2 += GAUSS_W[i] * form * form * sin_theta;
    }
    // translate dx in [-1,1] to dx in [lower,upper]
    total_F1 *= zm;
    total_F2 *= zm;
    const double s = (sld - solvent_sld) * form_volume(radius, length);
    *F1 = 1e-2 * s * total_F1;
    *F2 = 1e-4 * s * s * total_F2;
}



static double
Iqac(double qab, double qc,
    double sld,
    double solvent_sld,
    double radius,
    double length)
{
    const double form = _fq(qab, qc, radius, length);
    const double s = (sld-solvent_sld) * form_volume(radius, length);
    return 1.0e-4 * square(s * form);
}

