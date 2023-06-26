double form_volume(double radius, double thickness, double alpha, double beta);

double Iq(double q,
          double radius,
          double thickness,
          double alpha,
          double beta,
          double sld,
          double sld_solvent);


static
void _integrate_bessel(
    double radius,
    double alpha,
    double beta,
    double q_sin_psi,
    double q_cos_psi,
    double n,
    double *Sn,
    double *Cn)
{
    // translate gauss point z in [-1,1] to a point in [0, radius]
    const double zm = 0.5*radius;
    const double zb = 0.5*radius;

    // evaluate at Gauss points
    double sumS = 0.0;		// initialize integral
    double sumC = 0.0;		// initialize integral
    double r;
    for (int i=0; i < GAUSS_N; i++) {
        r = GAUSS_Z[i]*zm + zb;

        const double qrs = r*q_sin_psi;
        const double qrrc = r*r*q_cos_psi;

        double y = GAUSS_W[i] * r * sas_JN(n, beta*qrrc) * sas_JN(2*n, qrs);
        double S, C;
        SINCOS(alpha*qrrc, S, C);
        sumS += y*S;
        sumC += y*C;
    }

    *Sn = zm*sumS / (radius*radius);
    *Cn = zm*sumC / (radius*radius);
}

static
double _sum_bessel_orders(
    double radius,
    double alpha,
    double beta,
    double q_sin_psi,
    double q_cos_psi)
{
    //calculate sum term from n = -3 to 3
    //Note 1:
    //    S_n(-x) = (-1)^S_n(x)
    //    => S_n^2(-x) = S_n^2(x),
    //    => sum_-k^k Sk = S_0^2 + 2*sum_1^kSk^2
    //Note 2:
    //    better precision to sum terms from smaller to larger
    //    though it doesn't seem to make a difference in this case.
    double Sn, Cn, sum;
    sum = 0.0;
    for (int n=3; n>0; n--) {
      _integrate_bessel(radius, alpha, beta, q_sin_psi, q_cos_psi, n, &Sn, &Cn);
      sum += 2.0*(Sn*Sn + Cn*Cn);
    }
    _integrate_bessel(radius, alpha, beta, q_sin_psi, q_cos_psi, 0, &Sn, &Cn);
    sum += Sn*Sn+ Cn*Cn;
    return sum;
}

static
double _integrate_psi(
    double q,
    double radius,
    double thickness,
    double alpha,
    double beta)
{
    // translate gauss point z in [-1,1] to a point in [0, pi/2]
    const double zm = M_PI_4;
    const double zb = M_PI_4;

    double sum = 0.0;
    for (int i = 0; i < GAUSS_N; i++) {
        double psi = GAUSS_Z[i]*zm + zb;
        double sin_psi, cos_psi;
        SINCOS(psi, sin_psi, cos_psi);
        double bessel_term = _sum_bessel_orders(radius, alpha, beta, q*sin_psi, q*cos_psi);
        double sinc_term = square(sas_sinx_x(q * thickness * cos_psi / 2.0));
        double pringle_kernel = 4.0 * sin_psi * bessel_term * sinc_term;
        sum += GAUSS_W[i] * pringle_kernel;
    }

    return zm * sum;
}

double form_volume(double radius, double thickness, double alpha, double beta)
{
    return M_PI*radius*radius*thickness;
}

static double
radius_from_excluded_volume(double radius, double thickness)
{
    return 0.5*cbrt(0.75*radius*(2.0*radius*thickness + (radius + thickness)*(M_PI*radius + thickness)));
}

static double
radius_effective(int mode, double radius, double thickness, double alpha, double beta)
{
    switch (mode) {
    default:
    case 1: // equivalent cylinder excluded volume
        return radius_from_excluded_volume(radius, thickness);
    case 2: // equivalent volume sphere
        return cbrt(M_PI*radius*radius*thickness/M_4PI_3);
    case 3: // radius
        return radius;
    }
}

double Iq(
    double q,
    double radius,
    double thickness,
    double alpha,
    double beta,
    double sld,
    double sld_solvent)
{
    double form = _integrate_psi(q, radius, thickness, alpha, beta);
    double contrast = sld - sld_solvent;
    double volume = M_PI*radius*radius*thickness;
    return 1.0e-4*form * square(contrast * volume);
}
