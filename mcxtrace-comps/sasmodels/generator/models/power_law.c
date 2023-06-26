#include <math.h>

double Iq(double q, double power);

#pragma acc routine seq
double Iq(double q, double power) {
    double result = pow(q, -power);
    return result;
}

