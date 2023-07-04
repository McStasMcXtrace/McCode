static double
form_volume(double length_a, double exponent_p)
{
  double g1 = sas_gamma(1.0 / (2.0 * exponent_p));
  double g3 = sas_gamma(3.0 / (2.0 * exponent_p));
  return cube(length_a) / 12.0 / square(exponent_p) * cube(g1) / g3;
}

static double
radius_from_excluded_volume(double length_a, double exponent_p)
{
  double g1 = sas_gamma(1.0 / (2.0 * exponent_p));
  double g3 = sas_gamma(3.0 / (2.0 * exponent_p));
  double g5 = sas_gamma(5.0 / (2.0 * exponent_p));

  return length_a * g3 * sqrt(3.0 / 10.0 / g1 / g5);
}

static double

radius_effective(int mode, double length_a, double exponent_p)

{
  switch (mode)
  {
  default:
  case 1: // radius of gyration
    return radius_from_excluded_volume(length_a, exponent_p);
  case 2: // equivalent volume sphere
    return cbrt(form_volume(length_a, exponent_p) / M_4PI_3);
  case 3: // half length_a
    return 0.5 * length_a;
  }
}

static double oriented_superball(
    double qx,
    double qy,
    double qz,
    double length_a,
    double exponent_p)
{
  // oriented superball form factor

  // outer integral for x
  const double radius = length_a / 2.0; // superball radius
  const double inverse_2p = 1.0 / (2.0 * exponent_p);

  double outer_integral = 0.0; //initialize integral

  for (int i_x = 0; i_x < GAUSS_N; i_x++)
  {
    const double x = 0.5 * (GAUSS_Z[i_x] + 1.0); // integrate 0, 1
    const double x2p = pow(x, 2.0 * exponent_p);
    const double gamma = pow(1.0 - x2p, inverse_2p);

    // inner integral for y
    double inner_integral = 0.0; //initialize integral
    for (int i_y = 0; i_y < GAUSS_N; i_y++)
    {
      const double y = 0.5 * gamma * (GAUSS_Z[i_y] + 1.0); // integrate 0, gamma
      const double y2p = pow(y, 2.0 * exponent_p);
      const double zeta = pow(1.0 - x2p - y2p, inverse_2p);
      const double cos1 = cos(radius * qy * y);
      const double sinc2 = qz == 0 ? radius * zeta : sin(radius * qz * zeta) / qz;
      const double fq = cos1 * sinc2;
      inner_integral += GAUSS_W[i_y] * fq;
    }

    const double co = cos(radius * qx * x);

    // integration factor for -1,1 quadrature to 0, gamma: gamma/2
    const double integration_factor = 0.5 * gamma;

    // Eq. 21 in [Dresen2021]
    outer_integral += GAUSS_W[i_x] * integration_factor * inner_integral * co * 2.0 * square(length_a);

  }
// Needed to normalise the oriented form factor, but would be reverted later with s = SLD contrast * volume
// outer_integral /= form_volume(length_a, exponent_p); 

  // integration factor for -1,1 quadrature to 0, 1: 1/2
  return 0.5 * outer_integral;
}

static void
Fq(double q,
   double *F1,
   double *F2,
   double sld,
   double solvent_sld,
   double length_a,
   double exponent_p)
{

  // translate a point in [-1,1] to a point in [0, pi/2]
  const double zm = M_PI_4;
  const double zb = M_PI_4;

  double orient_averaged_outer_total_F1 = 0.0; //initialize integral
  double orient_averaged_outer_total_F2 = 0.0; //initialize integral
  // phi integral
  for (int i_phi = 0; i_phi < GAUSS_N; i_phi++)
  {

    const double phi = GAUSS_Z[i_phi]*zm +zb; // integrate 0 .. pi/2

    double sin_phi, cos_phi;
    SINCOS(phi, sin_phi, cos_phi);

    double orient_averaged_inner_total_F1 = 0.0; //initialize integral
    double orient_averaged_inner_total_F2 = 0.0; //initialize integral
    // theta integral
    for (int i_theta = 0; i_theta < GAUSS_N; i_theta++)
    {

      const double cos_theta = GAUSS_Z[i_theta]*0.5 + 0.5; // integrate 0, 1
      const double sin_theta = sqrt( 1.0 - square(cos_theta) );


      const double qx = q * cos_phi * sin_theta;
      const double qy = q * sin_phi * sin_theta;
      const double qz = q * cos_theta;

      const double f_oriented = oriented_superball(qx, qy, qz, length_a, exponent_p);


      orient_averaged_inner_total_F1 += GAUSS_W[i_theta] * f_oriented;
      orient_averaged_inner_total_F2 += GAUSS_W[i_theta] * square(f_oriented);

    }
    orient_averaged_outer_total_F1 += GAUSS_W[i_phi] * orient_averaged_inner_total_F1;
    orient_averaged_outer_total_F2 += GAUSS_W[i_phi] * orient_averaged_inner_total_F2;
  }


  // integration factors for phi and theta integral, divided by solid angle of pi/2
  orient_averaged_outer_total_F1 *= 0.25;
  orient_averaged_outer_total_F2 *= 0.25;
  // Multiply by contrast^2 and convert from [1e-12 A-1] to [cm-1]
  const double s =  (sld - solvent_sld) ;

  *F1 = 1.0e-2 * s * orient_averaged_outer_total_F1;
  *F2 = 1.0e-4 * s * s * orient_averaged_outer_total_F2;
}

static double
Iqabc(double qa, double qb, double qc,
      double sld,
      double solvent_sld,
      double length_a,
      double exponent_p)
{
  const double f_oriented = oriented_superball(qa, qb, qc, length_a, exponent_p);

  const double s = (sld - solvent_sld); 


  const double form = square(s * f_oriented);
  // Square and convert from [1e-12 A-1] to [cm-1]
  return 1.0e-4 * form;
}
