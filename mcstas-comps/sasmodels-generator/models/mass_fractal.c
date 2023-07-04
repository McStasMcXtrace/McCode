static double
Iq(double q, double radius, double fractal_dim_mass, double cutoff_length)
{
    //calculate P(q)
    const double pq = square(sas_3j1x_x(q*radius));

    //calculate S(q)
    // S(q) = gamma(D-1) sin((D-1)atan(q c))/q c^(D-1) (1+(q c)^2)^(-(D-1)/2)
    // lim D->1 [gamma(D-1) sin((D-1) a)] = a
    // lim q->0 [sin((D-1) atan(q c))/q] = (D-1) c
    // lim q,D->0,1 [gamma(D-1) sin((D-1)atan(q c))/q] = c
    double sq;
    if (q > 0. && fractal_dim_mass > 1.) {
        const double Dm1 = fractal_dim_mass - 1.0;
        const double t1 = sas_gamma(Dm1)*sin(Dm1*atan(q*cutoff_length));
        const double t2 = pow(cutoff_length, Dm1);
        const double t3 = pow(1.0 + square(q*cutoff_length), -0.5*Dm1);
        sq = t1 * t2 * t3 / q;
    } else if (q > 0.) {
        sq = atan(q*cutoff_length)/q;
    } else if (fractal_dim_mass > 1.) {
        const double D = fractal_dim_mass;
        sq = pow(cutoff_length, D) * sas_gamma(D);
    } else {
        sq = cutoff_length;
    }

    return pq * sq;
}
