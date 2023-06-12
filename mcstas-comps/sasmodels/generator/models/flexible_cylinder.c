static double
form_volume(double length, double kuhn_length, double radius)
{
    return 1.0;
}

static double
Iq(double q,
   double length,
   double kuhn_length,
   double radius,
   double sld,
   double solvent_sld)
{
    const double contrast = sld - solvent_sld;
    const double cross_section = sas_2J1x_x(q*radius);
    const double volume = M_PI*radius*radius*length;
    const double flex = Sk_WR(q, length, kuhn_length);
    return 1.0e-4 * volume * square(contrast*cross_section) * flex;
}
