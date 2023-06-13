static double
sc_Zq(double qa, double qb, double qc, double dnn, double d_factor)
{
    // Equations from Matsuoka 9-10-11, multiplied by |q|
    const double a1 = qa;
    const double a2 = qb;
    const double a3 = qc;

    // Matsuoka 13-14-15
    //     Z_k numerator: 1 - exp(a)^2
    //     Z_k denominator: 1 - 2 cos(d a_k) exp(a) + exp(2a)
    // Rewriting numerator
    //         => -(exp(2a) - 1)
    //         => -expm1(2a)
    // Rewriting denominator
    //         => exp(a)^2 - 2 cos(d ak) exp(a) + 1)
    //         => (exp(a) - 2 cos(d ak)) * exp(a) + 1
    const double arg = -0.5*square(dnn*d_factor)*(a1*a1 + a2*a2 + a3*a3);
    const double exp_arg = exp(arg);
    const double Zq = -cube(expm1(2.0*arg))
        / ( ((exp_arg - 2.0*cos(dnn*a1))*exp_arg + 1.0)
          * ((exp_arg - 2.0*cos(dnn*a2))*exp_arg + 1.0)
          * ((exp_arg - 2.0*cos(dnn*a3))*exp_arg + 1.0));

    return Zq;
}

// occupied volume fraction calculated from lattice symmetry and sphere radius
static double
sc_volume_fraction(double radius, double dnn)
{
    return sphere_volume(radius/dnn);
}

static double
form_volume(double radius)
{
    return sphere_volume(radius);
}


static double
Iq(double q, double dnn,
    double d_factor, double radius,
    double sld, double solvent_sld)
{
    // translate a point in [-1,1] to a point in [0, 2 pi]
    const double phi_m = M_PI_4;
    const double phi_b = M_PI_4;
    // translate a point in [-1,1] to a point in [0, pi]
    const double theta_m = M_PI_4;
    const double theta_b = M_PI_4;


    double outer_sum = 0.0;
    for(int i=0; i<GAUSS_N; i++) {
        double inner_sum = 0.0;
        const double theta = GAUSS_Z[i]*theta_m + theta_b;
        double sin_theta, cos_theta;
        SINCOS(theta, sin_theta, cos_theta);
        const double qc = q*cos_theta;
        const double qab = q*sin_theta;
        for(int j=0;j<GAUSS_N;j++) {
            const double phi = GAUSS_Z[j]*phi_m + phi_b;
            double sin_phi, cos_phi;
            SINCOS(phi, sin_phi, cos_phi);
            const double qa = qab*cos_phi;
            const double qb = qab*sin_phi;
            const double form = sc_Zq(qa, qb, qc, dnn, d_factor);
            inner_sum += GAUSS_W[j] * form;
        }
        inner_sum *= phi_m;  // sum(f(x)dx) = sum(f(x)) dx
        outer_sum += GAUSS_W[i] * inner_sum * sin_theta;
    }
    outer_sum *= theta_m;
    const double Zq = outer_sum/M_PI_2;
    const double Pq = sphere_form(q, radius, sld, solvent_sld);

    return sc_volume_fraction(radius, dnn) * Pq * Zq;
}

static double
Iqabc(double qa, double qb, double qc,
    double dnn, double d_factor, double radius,
    double sld, double solvent_sld)
{
    const double q = sqrt(qa*qa + qb*qb + qc*qc);
    const double Pq = sphere_form(q, radius, sld, solvent_sld);
    const double Zq = sc_Zq(qa, qb, qc, dnn, d_factor);
    return sc_volume_fraction(radius, dnn) * Pq * Zq;
}