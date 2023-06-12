static double
form_volume(double radius, double thickness)
{
    return M_4PI_3 * cube(radius + thickness);
}

static double
Iq(double q,
   double radius,
   double thickness,
   double core_sld,
   double shell_sld,
   double solvent_sld,
   double volfraction,
   double fractal_dim,
   double cor_length)
{
    //The radius for the building block of the core shell particle that is
    //needed by the Teixeira fractal S(q) is the radius of the whole particle.
    const double cs_radius = radius + thickness;
    const double sq = fractal_sq(q, cs_radius, fractal_dim, cor_length);
    const double fq = core_shell_fq(q, radius, thickness,
                                    core_sld, shell_sld, solvent_sld);

    return 1.0e-4 * volfraction * sq * fq * fq;
}
