double Iq(double q, double peak_pos, double sigma)
{
    double scaled_dq = (q - peak_pos)/sigma;
    return exp(-0.5*scaled_dq*scaled_dq); //sqrt(2*M_PI*sigma*sigma);
}