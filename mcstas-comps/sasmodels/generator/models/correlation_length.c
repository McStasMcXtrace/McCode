#include <math.h>

double Iq(double q, double lorentz_scale, double porod_scale, double cor_length, double porod_exp, double lorentz_exp);

#pragma acc routine seq
double Iq(double q, double lorentz_scale, double porod_scale, double cor_length, double porod_exp, double lorentz_exp) {
    double porod, lorentz, inten;

    porod = porod_scale / pow(q, porod_exp);
    lorentz = lorentz_scale / (1.0 + pow(q * cor_length, lorentz_exp));
    inten = porod + lorentz;

    return inten;
}

