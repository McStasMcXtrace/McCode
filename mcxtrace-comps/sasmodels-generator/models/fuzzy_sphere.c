static double form_volume(double radius, double fuzziness)
{
    return M_4PI_3*cube(radius);
}

static double
radius_effective(int mode, double radius, double fuzziness)
{
    switch (mode) {
    default:
    case 1: // radius
        return radius;
    case 2: // radius + fuzziness
        return radius + fuzziness;
    }
}

static void Fq(double q, double *F1, double *F2, double sld, double sld_solvent,
               double radius, double fuzziness)
{
    const double qr = q*radius;
    const double bes = sas_3j1x_x(qr);
    const double qf = exp(-0.5*square(q*fuzziness));
    const double contrast = (sld - sld_solvent);
    const double form = contrast * form_volume(radius,fuzziness) * bes * qf;
    *F1 = 1.0e-2*form;
    *F2 = 1.0e-4*form*form;
}
