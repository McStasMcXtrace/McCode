double Iq(double q, double cor_length)
{
    double denominator = 1 + (q*cor_length)*(q*cor_length);
    return 1/denominator;
}