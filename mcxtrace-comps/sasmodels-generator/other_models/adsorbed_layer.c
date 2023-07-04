#include <math.h>

double Iq(
    double q,
    double second_moment,
    double adsorbed_amount,
    double density_shell,
    double radius,
    double volfraction,
    double sld_shell,
    double sld_solvent
);

#pragma acc routine seq
double Iq(
    double q,
    double second_moment,
    double adsorbed_amount,
    double density_shell,
    double radius,
    double volfraction,
    double sld_shell,
    double sld_solvent
) {
    double aa = ((sld_shell - sld_solvent) / density_shell * adsorbed_amount) / q;
    double bb = q * second_moment;
    double inten = 6.0e-02 * PI * volfraction * pow(aa, 2) * exp(-pow(bb, 2)) / radius;
    return inten;
}
