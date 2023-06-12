static double
Iq(double q,
          double fractal_dim_mass,
          double fractal_dim_surf,
          double rg_cluster,
          double rg_primary)
{
     //computation
    const double Dm = 0.5*fractal_dim_mass;
    const double Dt = 0.5*(6.0 - (fractal_dim_mass + fractal_dim_surf));

    const double t1 = Dm==0. ? 1.0 : pow(1.0 + square(q*rg_cluster)/(3.0*Dm), -Dm);
    const double t2 = Dt==0. ? 1.0 : pow(1.0 + square(q*rg_primary)/(3.0*Dt), -Dt);
    const double form = t1*t2;

    return form;
}
