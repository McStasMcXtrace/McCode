static double
form_volume(double length_a, double b2a_ratio, double c2a_ratio)
{
    return length_a * (length_a*b2a_ratio) * (length_a*c2a_ratio);
}

static double
radius_from_excluded_volume(double length_a, double b2a_ratio, double c2a_ratio)
{
    double const r_equiv   = sqrt(length_a*length_a*b2a_ratio/M_PI);
    double const length_c  = c2a_ratio*length_a;
    return 0.5*cbrt(0.75*r_equiv*(2.0*r_equiv*length_c + (r_equiv + length_c)*(M_PI*r_equiv + length_c)));
}

static double
radius_effective(int mode, double length_a, double b2a_ratio, double c2a_ratio)
{
    switch (mode) {
    default:
    case 1: // equivalent cylinder excluded volume
        return radius_from_excluded_volume(length_a,b2a_ratio,c2a_ratio);
    case 2: // equivalent volume sphere
        return cbrt(cube(length_a)*b2a_ratio*c2a_ratio/M_4PI_3);
    case 3: // half length_a
        return 0.5 * length_a;
    case 4: // half length_b
        return 0.5 * length_a*b2a_ratio;
    case 5: // half length_c
        return 0.5 * length_a*c2a_ratio;
    case 6: // equivalent circular cross-section
        return length_a*sqrt(b2a_ratio/M_PI);
    case 7: // half ab diagonal
        return 0.5*sqrt(square(length_a) * (1.0 + square(b2a_ratio)));
    case 8: // half diagonal
        return 0.5*sqrt(square(length_a) * (1.0 + square(b2a_ratio) + square(c2a_ratio)));
    }
}

static double
Iq(double q,
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

    double outer_sum = 0.0;
    for(int i=0; i<GAUSS_N; i++) {
        const double theta = 0.5 * ( GAUSS_Z[i]*(v1b-v1a) + v1a + v1b );
        double sin_theta, cos_theta;
        SINCOS(theta, sin_theta, cos_theta);

        const double termC = sas_sinx_x(q * c_half * cos_theta);

        double inner_sum = 0.0;
        for(int j=0; j<GAUSS_N; j++) {
            double phi = 0.5 * ( GAUSS_Z[j]*(v2b-v2a) + v2a + v2b );
            double sin_phi, cos_phi;
            SINCOS(phi, sin_phi, cos_phi);

            // Amplitude AP from eqn. (12), rewritten to avoid round-off effects when arg=0
            const double termA = sas_sinx_x(q * a_half * sin_theta * sin_phi);
            const double termB = sas_sinx_x(q * b_half * sin_theta * cos_phi);
            const double AP = termA * termB * termC;
            inner_sum += GAUSS_W[j] * AP * AP;
        }
        inner_sum = 0.5 * (v2b-v2a) * inner_sum;
        outer_sum += GAUSS_W[i] * inner_sum * sin_theta;
    }

    double answer = 0.5*(v1b-v1a)*outer_sum;

    // Normalize by Pi (Eqn. 16).
    // The term (ABC)^2 does not appear because it was introduced before on
    // the definitions of termA, termB, termC.
    // The factor 2 appears because the theta integral has been defined between
    // 0 and pi/2, instead of 0 to pi.
    answer /= M_PI_2; //Form factor P(q)

    // Multiply by contrast^2 and volume^2
    const double volume = length_a * length_b * length_c;
    answer *= square((sld-solvent_sld)*volume);

    // Convert from [1e-12 A-1] to [cm-1]
    answer *= 1.0e-4;

    return answer;
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
        SINCOS(theta, sin_theta, cos_theta);

        const double termC = sas_sinx_x(q * c_half * cos_theta);

        double inner_sum_F1 = 0.0;
        double inner_sum_F2 = 0.0;
        for(int j=0; j<GAUSS_N; j++) {
            double phi = 0.5 * ( GAUSS_Z[j]*(v2b-v2a) + v2a + v2b );
            double sin_phi, cos_phi;
            SINCOS(phi, sin_phi, cos_phi);

            // Amplitude AP from eqn. (12), rewritten to avoid round-off effects when arg=0
            const double termA = sas_sinx_x(q * a_half * sin_theta * sin_phi);
            const double termB = sas_sinx_x(q * b_half * sin_theta * cos_phi);
            const double AP = termA * termB * termC;
            inner_sum_F1 += GAUSS_W[j] * AP;
            inner_sum_F2 += GAUSS_W[j] * AP * AP;
        }
        inner_sum_F1 = 0.5 * (v2b-v2a) * inner_sum_F1;
        inner_sum_F2 = 0.5 * (v2b-v2a) * inner_sum_F2;
        outer_sum_F1 += GAUSS_W[i] * inner_sum_F1 * sin_theta;
        outer_sum_F2 += GAUSS_W[i] * inner_sum_F2 * sin_theta;
    }

    outer_sum_F1 *= 0.5*(v1b-v1a);
    outer_sum_F2 *= 0.5*(v1b-v1a);

    // Normalize by Pi (Eqn. 16).
    // The term (ABC)^2 does not appear because it was introduced before on
    // the definitions of termA, termB, termC.
    // The factor 2 appears because the theta integral has been defined between
    // 0 and pi/2, instead of 0 to pi.
    outer_sum_F1 /= M_PI_2;
    outer_sum_F2 /= M_PI_2;

    // Multiply by contrast and volume
    const double s = (sld-solvent_sld) * (length_a * length_b * length_c);

    // Convert from [1e-12 A-1] to [cm-1]
    *F1 = 1e-2 * s * outer_sum_F1;
    *F2 = 1e-4 * s * s * outer_sum_F2;
}


static double
Iqabc(double qa, double qb, double qc,
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

    // Amplitude AP from eqn. (13)
    const double termA = sas_sinx_x(qa * a_half);
    const double termB = sas_sinx_x(qb * b_half);
    const double termC = sas_sinx_x(qc * c_half);
    const double AP = termA * termB * termC;

    // Multiply by contrast and volume
    const double s = (sld-solvent_sld) * (length_a * length_b * length_c);

    // Convert from [1e-12 A-1] to [cm-1]
    return 1.0e-4 * square(s * AP);
}
