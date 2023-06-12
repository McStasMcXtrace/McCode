static double Iq(double q,
          double guinier_scale,
          double lorentz_scale,
          double rg,
          double fractal_dim,
          double cor_length)
{
    // Lorentzian Term
    ////////////////////////double a(x[i]*x[i]*zeta*zeta);
    double lorentzian_term = square(q*cor_length);
    lorentzian_term = 1.0 + ((fractal_dim + 1.0)/3.0)*lorentzian_term;
    lorentzian_term = pow(lorentzian_term, fractal_dim/2.0 );

    // Exponential Term
    ////////////////////////double d(x[i]*x[i]*rg*rg);
    double exp_term = square(q*rg);
    exp_term = exp(-exp_term/3.0);

    // Scattering Law
    double result = lorentz_scale/lorentzian_term + guinier_scale*exp_term;
    return result;
}
