#include <math.h>

double Iq(double q, double rg, double s, double porod_exp);

#pragma acc routine seq
double Iq(double q, double rg, double s, double porod_exp) {
    double n = 3.0 - s;
    double ms = 0.5 * (porod_exp - s);  // =(n-3+porod_exp)/2
    double iq = 0.0;

    // Take care of the singular points
    if (rg <= 0.0 || ms <= 0.0) {
        return iq;
    }

    // Do the calculation and return the function value
    if (q < sqrt(n * ms) / rg) {
        iq = pow(q, -s) * exp(-pow(q * rg, 2) / n);
    } else {
        iq = pow(q, -porod_exp) * (exp(-ms) * pow(n * ms / pow(rg, 2), ms));
    }

    return iq;
}

