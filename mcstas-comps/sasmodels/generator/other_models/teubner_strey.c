#include <math.h>

#define PI 3.14159265358979323846

double Iq(double q, double volfraction_a, double sld_a, double sld_b, double d, double xi);

#pragma acc routine seq
double Iq(double q, double volfraction_a, double sld_a, double sld_b, double d, double xi) {
    double drho = sld_a - sld_b;
    double k = 2.0 * PI * xi / d;
    double a2 = pow(1.0 + k * k, 2.0);
    double c1 = 2.0 * xi * xi * (1.0 - k * k);
    double c2 = xi * xi * xi * xi;
    double prefactor = 8.0 * PI * volfraction_a * (1.0 - volfraction_a) * drho * drho * c2 / xi;
    return 1.0e-4 * prefactor / (c2 * q * q + c1 * q + a2);
}

