#include <math.h>
#define NEED_TGAMMA

double tgammainc(double a, double x);
double Iq(double q, double rg, double porod_exp);

#pragma acc routine seq
double tgammainc(double a, double x)
{
    const double eps = 1e-14;  // desired precision

    // Initialize the result and the current term
    double result = 0.0;
    double term = pow(x, a) * exp(-x) / tgamma(a + 1.0);
    int n = 1;

    // Sum terms until convergence or maximum number of iterations
    while (fabs(term) > eps && n <= 1000) {
        result += term;
        term = pow(x, a + n) * exp(-x) / tgamma(a + n + 1.0);
        n++;
    }

    return result;
}
    
#pragma acc routine seq
double Iq(double q, double rg, double porod_exp) {
    double usub = pow(q * rg, 2) * (2.0 / porod_exp + 1.0) * (2.0 / porod_exp + 2.0) / 6.0;
    double upow = pow(usub, -0.5 * porod_exp);
    double gamma_1 = tgamma(0.5 * porod_exp);
    double gamma_2 = tgamma(porod_exp);
    double gammainc_1 = tgammainc(0.5 * porod_exp, usub);
    double gammainc_2 = tgammainc(porod_exp, usub);
    double result = porod_exp * upow * (gamma_1 * gammainc_1 - upow * gamma_2 * gammainc_2);
    if (q <= 0) {
        result = 1.0;
    }
    return result;
}

