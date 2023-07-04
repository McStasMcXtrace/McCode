static double
form_volume(double radius, double thickness)
{
    return M_4PI_3 * cube(radius + thickness);
}

static double
radius_effective(int mode, double radius, double thickness)
{
    switch (mode) {
    default:
    case 1: // outer radius
        return radius + thickness;
    case 2: // core radius
        return radius;
    }
}

static void
Fq(double q, double *F1, double *F2, double radius,
   double thickness, double core_sld, double shell_sld, double solvent_sld) {
    double form = core_shell_fq(q,
                              radius,
                              thickness,
                              core_sld,
                              shell_sld,
                              solvent_sld);
    *F1 = 1.0e-2*form;
    *F2 = 1.0e-4*form*form;
}
