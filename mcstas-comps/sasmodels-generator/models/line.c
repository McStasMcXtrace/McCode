#include <math.h>

double Iq(double q, double intercept, double slope);

#pragma acc routine seq
double Iq(double q, double intercept, double slope) {
    double inten = intercept + slope * q;
    return inten;
}

