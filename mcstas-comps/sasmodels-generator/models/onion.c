
static double
f_exp(double q, double r, double sld_in, double sld_out,
    double thickness, double A, double side)
{
  const double vol = M_4PI_3 * cube(r);
  const double qr = q * r;
  const double bes = sas_3j1x_x(qr);
  const double alpha = A * r/thickness;
  double result;
  if (qr == 0.0) {
    result = 1.0;
  } else if (fabs(A) > 0.0) {
    const double qrsq = qr * qr;
    const double alphasq = alpha * alpha;
    const double sumsq = alphasq + qrsq;
    double sinqr, cosqr;
    SINCOS(qr, sinqr, cosqr);
    const double t1 = (alphasq - qrsq)*sinqr/qr - 2.0*alpha*cosqr;
    const double t2 = alpha*sinqr/qr - cosqr;
    const double fun = -3.0*(t1/sumsq - t2)/sumsq;
    const double slope = (sld_out - sld_in)/expm1(A);
    const double contrast = slope*exp(A*side);
    result = contrast*fun + (sld_in-slope)*bes;
  } else {
    result = sld_in*bes;
  }
  return vol * result;
}

static double
outer_radius(double radius_core, double n_shells, double thickness[])
{
  int n = (int)(n_shells+0.5);
  double r = radius_core;
  for (int i=0; i < n; i++) {
    r += thickness[i];
  }
  return r;
}

static double
form_volume(double radius_core, double n_shells, double thickness[])
{
  return M_4PI_3*cube(outer_radius(radius_core, n_shells, thickness));
}

static double
radius_effective(int mode, double radius_core, double n_shells, double thickness[])
{
  // case 1: outer radius
  return outer_radius(radius_core, n_shells, thickness);
}

static void
Fq(double q, double *F1, double *F2, double sld_core, double radius_core, double sld_solvent,
    double n_shells, double sld_in[], double sld_out[], double thickness[],
    double A[])
{
  int n = (int)(n_shells+0.5);
  double r_out = radius_core;
  double f = f_exp(q, r_out, sld_core, 0.0, 0.0, 0.0, 0.0);
  for (int i=0; i < n; i++){
    const double r_in = r_out;
    r_out += thickness[i];
    f -= f_exp(q, r_in, sld_in[i], sld_out[i], thickness[i], A[i], 0.0);
    f += f_exp(q, r_out, sld_in[i], sld_out[i], thickness[i], A[i], 1.0);
  }
  f -= f_exp(q, r_out, sld_solvent, 0.0, 0.0, 0.0, 0.0);

  *F1 = 1e-2 * f;
  *F2 = 1e-4 * f * f;
}
