double Iq(double q, double cor_length)
{
    double numerator   = cube(cor_length);
    double denominator = square(1 + square(q*cor_length));

    return numerator / denominator ;
}