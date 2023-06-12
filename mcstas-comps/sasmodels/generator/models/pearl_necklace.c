double form_volume(double radius, double edge_sep,
    double thick_string, double fp_num_pearls);
double Iq(double q, double radius, double edge_sep,
    double thick_string, double fp_num_pearls, double sld,
    double string_sld, double solvent_sld);

// From Igor library
static double
pearl_necklace_kernel(double q, double radius, double edge_sep, double thick_string,
    int num_pearls, double sld_pearl, double sld_string, double sld_solv)
{
    // number of string segments
    const int num_strings = num_pearls - 1;

    //each masses: contrast * volume
    const double contrast_pearl = sld_pearl - sld_solv;
    const double contrast_string = sld_string - sld_solv;
    const double string_vol = edge_sep * M_PI_4 * thick_string * thick_string;
    const double pearl_vol = M_4PI_3 * radius * radius * radius;
    const double m_string = contrast_string * string_vol;
    const double m_pearl = contrast_pearl * pearl_vol;

    // center to center distance between the neighboring pearls
    const double A_s = edge_sep + 2.0 * radius;

    //sine functions of a pearl
    // Note: lim_(q->0) Si(q*a)/(q*b) = a/b
    // So therefore:
    //    beta = q==0. ? 1.0 : (Si(q*(A_s-radius)) - Si(q*radius))/q_edge;
    //    gamma = q==0. ? 1.0 : Si(q_edge)/q_edge;
    // But there is a 1/(1-sinc) term below which blows up so don't bother
    const double q_edge = q * edge_sep;
    const double beta = (sas_Si(q*(A_s-radius)) - sas_Si(q*radius)) / q_edge;
    const double gamma = sas_Si(q_edge) / q_edge;
    const double psi = sas_3j1x_x(q*radius);

    // Precomputed sinc terms
    const double si = sas_sinx_x(q*A_s);
    const double omsi = 1.0 - si;
    const double pow_si = pown(si, num_pearls);

    // form factor for num_pearls
    const double sss = 2.0*square(m_pearl*psi) * (
        - si * (1.0 - pow_si) / (omsi*omsi)
        + num_pearls / omsi
        - 0.5 * num_pearls
        );

    // form factor for num_strings (like thin rods)
    const double srr = m_string * m_string * (
        - 2.0 * (1.0 - pow_si/si)*beta*beta / (omsi*omsi)
        + 2.0 * num_strings*beta*beta / omsi
        + num_strings * (2.0*gamma - square(sas_sinx_x(q_edge/2.0)))
        );

    // form factor for correlations
    const double srs = 4.0 * m_string * m_pearl * beta * psi * (
        - si * (1.0 - pow_si/si) / (omsi*omsi)
        + num_strings / omsi
        );

    const double form = sss + srr + srs;

    return 1.0e-4 * form;
}

double form_volume(double radius, double edge_sep, double thick_string, double fp_num_pearls)
{
    const int num_pearls = (int)(fp_num_pearls + 0.5); //Force integer number of pearls
    const int num_strings = num_pearls - 1;
    const double string_vol = edge_sep * M_PI_4 * thick_string * thick_string;
    const double pearl_vol = M_4PI_3 * radius * radius * radius;
    const double volume = num_strings*string_vol + num_pearls*pearl_vol;

    return volume;
}

static double
radius_from_volume(double radius, double edge_sep, double thick_string, double fp_num_pearls)
{
    const double vol_tot = form_volume(radius, edge_sep, thick_string, fp_num_pearls);
    return cbrt(vol_tot/M_4PI_3);
}

static double
radius_effective(int mode, double radius, double edge_sep, double thick_string, double fp_num_pearls)
{
    switch (mode) {
    default:
    case 1:
        return radius_from_volume(radius, edge_sep, thick_string, fp_num_pearls);
    }
}

double Iq(double q, double radius, double edge_sep,
    double thick_string, double fp_num_pearls, double sld,
    double string_sld, double solvent_sld)
{
    const int num_pearls = (int)(fp_num_pearls + 0.5); //Force integer number of pearls
    const double form = pearl_necklace_kernel(q, radius, edge_sep,
        thick_string, num_pearls, sld, string_sld, solvent_sld);

    return form;
}
