#include <math.h>
  
double Iq(double q);

#pragma acc routine seq
double Iq(double q) {
    return pow(q, -4);
}

