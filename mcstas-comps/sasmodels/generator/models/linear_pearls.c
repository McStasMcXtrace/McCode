double form_volume(double radius, double num_pearls);

double Iq(double q,
            double radius,
            double edge_sep,
            double fp_num_pearls,
            double pearl_sld,
            double solvent_sld);

double linear_pearls_kernel(double q,
            double radius,
            double edge_sep,
            int num_pearls,
            double pearl_sld,
            double solvent_sld);


double form_volume(double radius, double fp_num_pearls)
{
    int num_pearls = (int)(fp_num_pearls + 0.5);
    // Pearl volume
    double pearl_vol = M_4PI_3 * cube(radius);
    // Return total volume
    return num_pearls * pearl_vol;;
}

double linear_pearls_kernel(double q,
            double radius,
            double edge_sep,
            int num_pearls,
            double pearl_sld,
            double solvent_sld)
{
    //relative sld
    double contrast_pearl = pearl_sld - solvent_sld;
    //each volume
    double pearl_vol = M_4PI_3 * cube(radius);
    //total volume
    double tot_vol = num_pearls * pearl_vol;
    //mass
    double m_s = contrast_pearl * pearl_vol;
    //center to center distance between the neighboring pearls
    double separation = edge_sep + 2.0 * radius;

    //sine functions of a pearl
    double psi = sas_3j1x_x(q * radius);

    // N pearls interaction terms
    double structure_factor = (double)num_pearls;
    for(int num=1; num<num_pearls; num++) {
        structure_factor += 2.0*(num_pearls-num)*sas_sinx_x(q*separation*num);
    }
    // form factor for num_pearls
    double form_factor = 1.0e-4 * structure_factor * square(m_s*psi) / tot_vol;

    return form_factor;
}

double Iq(double q,
            double radius,
            double edge_sep,
            double fp_num_pearls,
            double pearl_sld,
            double solvent_sld)
{

    int num_pearls = (int)(fp_num_pearls + 0.5);
	double result = linear_pearls_kernel(q,
                    radius,
                    edge_sep,
                    num_pearls,
                    pearl_sld,
                    solvent_sld);

	return result;
}
