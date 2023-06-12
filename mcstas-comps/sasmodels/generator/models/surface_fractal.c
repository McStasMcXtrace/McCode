
double form_volume(double radius);

double Iq(double q,
          double radius,
          double fractal_dim_surf,
          double cutoff_length);

static double _surface_fractal_kernel(double q,
    double radius,
    double fractal_dim_surf,
    double cutoff_length)
{
    // calculate P(q)
    const double pq = square(sas_3j1x_x(q*radius));

    // calculate S(q)
    // Note: lim q->0 S(q) = -gamma(mmo) cutoff_length^mmo (mmo cutoff_length)
    // however, the surface fractal formula is invalid outside the range
    const double mmo = 5.0 - fractal_dim_surf;
    const double sq = sas_gamma(mmo) * pow(cutoff_length, mmo)
           * pow(1.0 + square(q*cutoff_length), -0.5*mmo)
           * sin(-mmo * atan(q*cutoff_length)) / q;

    // Empirically determined that the results are valid within this range.
    // Above 1/r, the form starts to oscillate;  below
    //const double result = (q > 5./(3-fractal_dim_surf)/cutoff_length) && q < 1./radius
    //                      ? pq * sq : 0.);

    double result = pq * sq;

    // exclude negative results
    return result > 0. ? result : 0.;
}
double form_volume(double radius)
{
    return M_4PI_3*cube(radius);
}

double Iq(double q,
    double radius,
    double fractal_dim_surf,
    double cutoff_length
    )
{
    return _surface_fractal_kernel(q, radius, fractal_dim_surf, cutoff_length);
}
