static double form_volume(double radius)
{
    return M_4PI_3*cube(radius);
}

static double
radius_effective(int mode, double radius)
{
    // case 1: radius
    return radius;
}

static void Fq(double q, double *f1, double *f2, double sld, double sld_solvent, double radius)
{
    const double bes = sas_3j1x_x(q*radius);
    const double contrast = (sld - sld_solvent);
    const double form = contrast * form_volume(radius) * bes;
    *f1 = 1.0e-2*form;
    *f2 = 1.0e-4*form*form;
}
