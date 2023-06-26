#include <math.h>
#include <stdio.h>
    
double Iq(double q, double coefficent_1, double crossover, double power_1, double power_2);

#pragma acc routine seq
double Iq(double q, double coefficent_1, double crossover, double power_1, double power_2) {
    double result;
    if (q <= crossover) {
        result = coefficent_1 * pow(q, -power_1);
    } else {
        double coefficent_2 = coefficent_1 * pow(crossover, power_2 - power_1);
        result = coefficent_2 * pow(q, -power_2);
    }
    return result;
}

