static double
outer_radius(double fp_n_shells, double thickness[], double interface[])
{
    int n_shells= (int)(fp_n_shells + 0.5);
    double r = 0.0;
    for (int i=0; i < n_shells; i++) {
        r += thickness[i] + interface[i];
    }
    return r;
}

static double form_volume(
    double fp_n_shells,
    double thickness[],
    double interface[])
{
    return M_4PI_3*cube(outer_radius(fp_n_shells, thickness, interface));
}

static double
radius_effective(int mode, double fp_n_shells, double thickness[], double interface[])
{
    // case 1: outer radius
    return outer_radius(fp_n_shells, thickness, interface);
}

static double blend(int shape, double nu, double z)
{
    if (shape==0) {
        const double num = sas_erf(nu * M_SQRT1_2 * (2.0*z - 1.0));
        const double denom = 2.0 * sas_erf(nu * M_SQRT1_2);
        return num/denom + 0.5;
    } else if (shape==1) {
        return pow(z, nu);
    } else if (shape==2) {
        return 1.0 - pow(1.0 - z, nu);
    } else if (shape==3) {
        return expm1(-nu*z)/expm1(-nu);
    } else if (shape==4) {
        return expm1(nu*z)/expm1(nu);
    } else if (shape==5) {
        return 1.0 - pow(1.0 - z*z, (0.5*nu-2.0));        
    } else {
        return NAN;
    }
}

static double f_linear(double q, double r, double contrast, double slope)
{
    const double qr = q * r;
    const double qrsq = qr * qr;
    const double bes = sas_3j1x_x(qr);
    double sinqr, cosqr;
    SINCOS(qr, sinqr, cosqr);
    const double fun = 3.0*r*(2.0*qr*sinqr - (qrsq-2.0)*cosqr)/(qrsq*qrsq);
    const double vol = M_4PI_3 * cube(r);
    return vol*(bes*contrast + fun*slope);
}

static double Iq(
    double q,
    double fp_n_shells,
    double sld_solvent,
    double sld[],
    double thickness[],
    double interface[],
    double shape[],
    double nu[],
    double fp_n_steps)
{
    // iteration for # of shells + core + solvent
    int n_shells = (int)(fp_n_shells + 0.5);
    int n_steps = (int)(fp_n_steps + 0.5);
    double f=0.0;
    double r=0.0;
    for (int shell=0; shell<n_shells; shell++){
        const double sld_l = sld[shell];

        // uniform shell; r=0 => r^3=0 => f=0, so works for core as well.
        f -= M_4PI_3 * cube(r) * sld_l * sas_3j1x_x(q*r);
        r += thickness[shell];
        f += M_4PI_3 * cube(r) * sld_l * sas_3j1x_x(q*r);

        // iterate over sub_shells in the interface
        const double dr = interface[shell]/n_steps;
        const double delta = (shell==n_shells-1 ? sld_solvent : sld[shell+1]) - sld_l;
        const double nu_shell = fmax(fabs(nu[shell]), 1.e-14);
        const int shape_shell = (int)(shape[shell]);

        // if there is no interface the equations don't work
        if (dr == 0.) continue;

        double sld_in = sld_l;
        for (int step=1; step <= n_steps; step++) {
            // find sld_i at the outer boundary of sub-shell step
            //const double z = (double)step/(double)n_steps;
            const double z = (double)step/(double)n_steps;
            const double fraction = blend(shape_shell, nu_shell, z);
            const double sld_out = fraction*delta + sld_l;
            // calculate slope
            const double slope = (sld_out - sld_in)/dr;
            const double contrast = sld_in - slope*r;

            // iteration for the left and right boundary of the shells
            f -= f_linear(q, r, contrast, slope);
            r += dr;
            f += f_linear(q, r, contrast, slope);
            sld_in = sld_out;
        }
    }
    // add in solvent effect
    f -= M_4PI_3 * cube(r) * sld_solvent * sas_3j1x_x(q*r);

    const double f2 = f * f * 1.0e-4;
    return f2;
}

static void Fq(
    double q,
    double *F1,
    double *F2,
    double fp_n_shells,
    double sld_solvent,
    double sld[],
    double thickness[],
    double interface[],
    double shape[],
    double nu[],
    double fp_n_steps)
{
    // iteration for # of shells + core + solvent
    int n_shells = (int)(fp_n_shells + 0.5);
    int n_steps = (int)(fp_n_steps + 0.5);
    double f=0.0;
    double r=0.0;
    for (int shell=0; shell<n_shells; shell++){
        const double sld_l = sld[shell];

        // uniform shell; r=0 => r^3=0 => f=0, so works for core as well.
        f -= M_4PI_3 * cube(r) * sld_l * sas_3j1x_x(q*r);
        r += thickness[shell];
        f += M_4PI_3 * cube(r) * sld_l * sas_3j1x_x(q*r);

        // iterate over sub_shells in the interface
        const double dr = interface[shell]/n_steps;
        const double delta = (shell==n_shells-1 ? sld_solvent : sld[shell+1]) - sld_l;
        const double nu_shell = fmax(fabs(nu[shell]), 1.e-14);
        const int shape_shell = (int)(shape[shell]);

        // if there is no interface the equations don't work
        if (dr == 0.) continue;

        double sld_in = sld_l;
        for (int step=1; step <= n_steps; step++) {
            // find sld_i at the outer boundary of sub-shell step
            //const double z = (double)step/(double)n_steps;
            const double z = (double)step/(double)n_steps;
            const double fraction = blend(shape_shell, nu_shell, z);
            const double sld_out = fraction*delta + sld_l;
            // calculate slope
            const double slope = (sld_out - sld_in)/dr;
            const double contrast = sld_in - slope*r;

            // iteration for the left and right boundary of the shells
            f -= f_linear(q, r, contrast, slope);
            r += dr;
            f += f_linear(q, r, contrast, slope);
            sld_in = sld_out;
        }
    }
    // add in solvent effect
    f -= M_4PI_3 * cube(r) * sld_solvent * sas_3j1x_x(q*r);

    *F1 = 1e-2*f;
    *F2 = 1e-4*f*f;
}
