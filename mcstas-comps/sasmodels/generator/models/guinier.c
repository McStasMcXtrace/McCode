double Iq(double q, double rg)
{
    double exponent = fabs(rg)*rg*q*q/3.0;
    double value = exp(-exponent);
    return value;
}