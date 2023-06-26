static double
shell_volume(double length_a, double b2a_ratio, double c2a_ratio)
{
    const double length_b = length_a * b2a_ratio;
    const double length_c = length_a * c2a_ratio;
    const double shell_volume = 2.0 * (length_a*length_b + length_a*length_c + length_b*length_c);
    return shell_volume;
}

static double
form_volume(double length_a, double b2a_ratio, double c2a_ratio)
{
    const double length_b = length_a * b2a_ratio;
    const double length_c = length_a * c2a_ratio;
    const double form_volume = length_a * length_b * length_c;
    return form_volume;
}

static double
radius_from_excluded_volume(double length_a, double b2a_ratio, double c2a_ratio)
{
    const double r_equiv = sqrt(length_a*length_a*b2a_ratio/M_PI);
    const double length_c = length_a*c2a_ratio;
    return 0.5*cbrt(0.75*r_equiv*(2.0*r_equiv*length_c + (r_equiv + length_c)*(M_PI*r_equiv + length_c)));
}

static double
radius_effective(int mode, double length_a, double b2a_ratio, double c2a_ratio)
{
    switch (mode) {
    default:
    case 1: // equivalent cylinder excluded volume
        return radius_from_excluded_volume(length_a, b2a_ratio, c2a_ratio);
    case 2: // equivalent outer volume sphere
        return cbrt(cube(length_a)*b2a_ratio*c2a_ratio/M_4PI_3);
    case 3: // half length_a
        return 0.5 * length_a;
    case 4: // half length_b
        return 0.5 * length_a*b2a_ratio;
    case 5: // half length_c
        return 0.5 * length_a*c2a_ratio;
    case 6: // equivalent outer circular cross-section
        return length_a*sqrt(b2a_ratio/M_PI);
    case 7: // half ab diagonal
        return 0.5*sqrt(square(length_a) * (1.0 + square(b2a_ratio)));
    case 8: // half diagonal
        return 0.5*sqrt(square(length_a) * (1.0 + square(b2a_ratio) + square(c2a_ratio)));
    }
}

static void
Fq(double q,
    double *F1,
    double *F2,
    double sld,
    double solvent_sld,
    double length_a,
    double b2a_ratio,
    double c2a_ratio)
{
    const double length_b = length_a * b2a_ratio;
    const double length_c = length_a * c2a_ratio;
    const double a_half = 0.5 * length_a;
    const double b_half = 0.5 * length_b;
    const double c_half = 0.5 * length_c;

   //Integration limits to use in Gaussian quadrature
    const double v1a = 0.0;
    const double v1b = M_PI_2;  //theta integration limits
    const double v2a = 0.0;
    const double v2b = M_PI_2;  //phi integration limits

    double outer_sum_F1 = 0.0;
    double outer_sum_F2 = 0.0;
    for(int i=0; i<GAUSS_N; i++) {
        const double theta = 0.5 * ( GAUSS_Z[i]*(v1b-v1a) + v1a + v1b );

        double sin_theta, cos_theta;
        double sin_c, cos_c;
        SINCOS(theta, sin_theta, cos_theta);
        SINCOS(q*c_half*cos_theta, sin_c, cos_c);

        // To check potential problems if denominator goes to zero here !!!
        const double termAL_theta = 8.0 * cos_c / (q*q*sin_theta*sin_theta);
        const double termAT_theta = 8.0 * sin_c / (q*q*sin_theta*cos_theta);

        double inner_sum_F1 = 0.0;
        double inner_sum_F2 = 0.0;
        for(int j=0; j<GAUSS_N; j++) {
            const double phi = 0.5 * ( GAUSS_Z[j]*(v2b-v2a) + v2a + v2b );

            double sin_phi, cos_phi;
            double sin_a, cos_a;
            double sin_b, cos_b;
            SINCOS(phi, sin_phi, cos_phi);
            SINCOS(q*a_half*sin_theta*sin_phi, sin_a, cos_a);
            SINCOS(q*b_half*sin_theta*cos_phi, sin_b, cos_b);

            // Amplitude AL from eqn. (7c)
            const double AL = termAL_theta
                * sin_a*sin_b / (sin_phi*cos_phi);

            // Amplitude AT from eqn. (9)
            const double AT = termAT_theta
                * ( cos_a*sin_b/cos_phi + cos_b*sin_a/sin_phi );

            inner_sum_F1 += GAUSS_W[j] * (AL+AT);
            inner_sum_F2 += GAUSS_W[j] * square(AL+AT);
        }

        inner_sum_F1 *= 0.5 * (v2b-v2a);
        inner_sum_F2 *= 0.5 * (v2b-v2a);
        outer_sum_F1 += GAUSS_W[i] * inner_sum_F1 * sin_theta;
        outer_sum_F2 += GAUSS_W[i] * inner_sum_F2 * sin_theta;
    }

    outer_sum_F1 *= 0.5*(v1b-v1a);
    outer_sum_F2 *= 0.5*(v1b-v1a);

    // Normalize as in Eqn. (15) without the volume factor (as cancels with (V*DelRho)^2 normalization)
    // The factor 2 is due to the different theta integration limit (pi/2 instead of pi)
    const double form_avg = outer_sum_F1/M_PI_2;
    const double form_squared_avg = outer_sum_F2/M_PI_2;

    // Multiply by contrast^2. Factor corresponding to volume^2 cancels with previous normalization.
    const double contrast = sld - solvent_sld;

    // Convert from [1e-12 A-1] to [cm-1]
    *F1 = 1e-2 * contrast * form_avg;
    *F2 = 1e-4 * contrast * contrast * form_squared_avg;
}
