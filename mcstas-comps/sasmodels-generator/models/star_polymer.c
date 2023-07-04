double form_volume(void);

double Iq(double q, double radius2, double arms);

static double star_polymer_kernel(double q, double radius2, double arms)
{

    double u_2 = radius2 * q * q;
    double v = u_2 * arms / (3.0 * arms - 2.0);

    double term1 = v + expm1(-v);
    double term2 = ((arms - 1.0)/2.0) * square(expm1(-v));

    return (2.0 * (term1 + term2)) / (arms * v * v);

}

double form_volume(void)
{
    return 1.0;
}

double Iq(double q, double radius2, double arms)
{
    return star_polymer_kernel(q, radius2, arms);
}
