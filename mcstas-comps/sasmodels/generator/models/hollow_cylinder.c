static double
shell_volume(double radius, double thickness, double length)
{
    return M_PI*length*(square(radius+thickness) - radius*radius);
}

static double
form_volume(double radius, double thickness, double length)
{
    return M_PI*length*square(radius+thickness);
}

static double
radius_from_excluded_volume(double radius, double thickness, double length)
{
    const double radius_tot = radius + thickness;
    return 0.5*cbrt(0.75*radius_tot*(2.0*radius_tot*length + (radius_tot + length)*(M_PI*radius_tot + length)));
}

static double
radius_from_volume(double radius, double thickness, double length)
{
    const double volume_outer_cyl = M_PI*square(radius + thickness)*length;
    return cbrt(volume_outer_cyl/M_4PI_3);
}

static double
radius_from_diagonal(double radius, double thickness, double length)
{
    return sqrt(square(radius + thickness) + 0.25*square(length));
}

static double
radius_effective(int mode, double radius, double thickness, double length)
{
    switch (mode) {
    default:
    case 1: // excluded volume
        return radius_from_excluded_volume(radius, thickness, length);
    case 2: // equivalent volume sphere
        return radius_from_volume(radius, thickness, length);
    case 3: // outer radius
        return radius + thickness;
    case 4: // half length
        return 0.5*length;
    case 5: // half outer min dimension
        return (radius + thickness < 0.5*length ? radius + thickness : 0.5*length);
    case 6: // half outer max dimension
        return (radius + thickness > 0.5*length ? radius + thickness : 0.5*length);
    case 7: // half outer diagonal
        return radius_from_diagonal(radius,thickness,length);
    }
}

static double
_fq(double qab, double qc,
    double radius, double thickness, double length)
{
    const double lam1 = sas_2J1x_x((radius+thickness)*qab);
    const double lam2 = sas_2J1x_x(radius*qab);
    const double gamma_sq = square(radius/(radius+thickness));
    //Note: lim_{thickness -> 0} psi = sas_J0(radius*qab)
    //Note: lim_{radius -> 0} psi = sas_2J1x_x(thickness*qab)
    const double psi = (lam1 - gamma_sq*lam2)/(1.0 - gamma_sq);    //SRK 10/19/00
    const double t2 = sas_sinx_x(0.5*length*qc);
    return psi*t2;
}

static void
Fq(double q, double *F1, double *F2, double radius, double thickness, double length,
    double sld, double solvent_sld)
{
    const double lower = 0.0;
    const double upper = 1.0;        //limits of numerical integral

    double total_F1 = 0.0;            //initialize intergral
    double total_F2 = 0.0;
    for (int i=0;i<GAUSS_N;i++) {
        const double cos_theta = 0.5*( GAUSS_Z[i] * (upper-lower) + lower + upper );
        const double sin_theta = sqrt(1.0 - cos_theta*cos_theta);
        const double form = _fq(q*sin_theta, q*cos_theta,
                                radius, thickness, length);
        total_F1 += GAUSS_W[i] * form;
        total_F2 += GAUSS_W[i] * form * form;
    }
    total_F1 *= 0.5*(upper-lower);
    total_F2 *= 0.5*(upper-lower);
    const double s = (sld - solvent_sld) * shell_volume(radius, thickness, length);
    *F1 = 1e-2 * s * total_F1;
    *F2 = 1e-4 * s*s * total_F2;
}


static double
Iqac(double qab, double qc,
    double radius, double thickness, double length,
    double sld, double solvent_sld)
{
    const double form = _fq(qab, qc, radius, thickness, length);
    const double s = (sld - solvent_sld) * shell_volume(radius, thickness, length);
    return 1.0e-4*square(s * form);
}
