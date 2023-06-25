// integral of sin(x)/x Taylor series approximated to w/i 0.1%
double sas_Si(double x);

    
#pragma acc routine seq
double sas_Si(double x)
{
    if (x >= M_PI*6.2/4.0) {
        const double xxinv = 1./(x*x);
        // Explicitly writing factorial values triples the speed of the calculation
        const double out_cos = (((-720.*xxinv + 24.)*xxinv - 2.)*xxinv + 1.)/x;
        const double out_sin = (((-5040.*xxinv + 120.)*xxinv - 6.)*xxinv + 1)*xxinv;

        double sin_x, cos_x;
        SINCOS(x, sin_x, cos_x);
        return M_PI_2 - cos_x*out_cos - sin_x*out_sin;
    } else {
        const double xx = x*x;
        // Explicitly writing factorial values triples the speed of the calculation
        return (((((-1./439084800.*xx
            + 1./3265920.)*xx
            - 1./35280.)*xx
            + 1./600.)*xx
            - 1./18.)*xx
            + 1.)*x;
    }
}
