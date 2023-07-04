#include <math.h>

double Iq(double q,
          double lorentz_scale_1,
          double lorentz_length_1,
          double lorentz_exp_1,
          double lorentz_scale_2,
          double lorentz_length_2,
          double lorentz_exp_2);

#pragma acc routine seq
double Iq(double q,
          double lorentz_scale_1,
          double lorentz_length_1,
          double lorentz_exp_1,
          double lorentz_scale_2,
          double lorentz_length_2,
          double lorentz_exp_2) {
    double intensity = lorentz_scale_1 / (1.0 + pow(q * lorentz_length_1, lorentz_exp_1));
    intensity += lorentz_scale_2 / (1.0 + pow(q * lorentz_length_2, lorentz_exp_2));
    return intensity;
}

