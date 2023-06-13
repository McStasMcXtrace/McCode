    
double Iq(double q, double i_zero, double rg, double polydispersity);

#pragma acc routine seq
double Iq(double q, double i_zero, double rg, double polydispersity) {
    double u = polydispersity - 1.0;
    double z = q * q * (rg * rg / (1.0 + 2.0 * u));

    // need to trap the case of the polydispersity being 1 (ie, monodisperse!)
    double result;
    if (polydispersity == 1.0) {
        if (q == 0.0) {
            result = 1.0;
        } else {
            result = 2.0 * (expm1(-z) + z) / (q * q);
        }
    } else {
        // Taylor series around z=0 of (2*(1+uz)^(-1/u) + z - 1) / (z^2(u+1))
        double p[3] = {(+1 + 5 * u + 6 * u * u) / 12.0, (-1 - 2 * u) / 3.0, (+1)};
        result = 2.0 * (pow(1.0 + u * z, -1.0 / u) + z - 1.0) / (1.0 + u);
        if (z > 1e-4) {
            result /= (q * q * z * z);
        } else {
            result = p[0] * z * z + p[1] * z + p[2];
        }
    }
    return i_zero * result;
}

