#include <math.h>
    
double Iq(double q, double peak_pos, double peak_hwhm);

#pragma acc routine seq
double Iq(double q, double peak_pos, double peak_hwhm) {
    double inten = 1.0 / (1.0 + pow((q - peak_pos) / peak_hwhm, 2.0));
    return inten;
}

