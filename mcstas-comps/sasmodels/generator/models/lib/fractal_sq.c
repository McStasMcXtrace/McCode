#pragma acc routine seq
static double
fractal_sq(double q, double radius, double fractal_dim, double cor_length)
{
    //calculate S(q),  using Teixeira, Eq(15)
    // mathematica query to check limiting conditions:
    //    lim x->0 of [ x gamma(x-1) sin(arctan(q c (x-1))) (q r)^(-x) (1 + 1/(q c)^2)^((1-x)/2) ]
    // Note: gamma(x) may be unreliable for x<0, so the gamma(D-1) is risky.
    // We instead transform D*gamma(D-1) into gamma(D+1)/(D-1).
    double term;
    if (q == 0.) {
        const double D = fractal_dim;
        term = pow(cor_length/radius, D)*sas_gamma(D+1.);
    } else if (fractal_dim == 0.) {
        term = 1.0;
    } else if (fractal_dim == 1.) {
        term = atan(q*cor_length)/(q*radius);
    } else {
        // q>0, D>0
        const double D = fractal_dim;
        const double Dm1 = fractal_dim - 1.0;
        // Note: for large Dm1, sin(Dm1*atan(q*cor_length) can go negative
        const double t1 = sas_gamma(D+1.)/Dm1*sin(Dm1*atan(q*cor_length));
        const double t2 = pow(q*radius, -D);
        const double t3 = pow(1.0 + 1.0/square(q*cor_length), -0.5*Dm1);
        term = t1 * t2 * t3;
    }
    return 1.0 + term;
}
