double Iq(double q,
        double ndensity,
        double v_core,
        double v_corona,
        double solvent_sld,
        double core_sld,
        double corona_sld,
        double radius_core,
        double rg,
        double d_penetration,
        double n_aggreg);

static double micelle_spherical_kernel(double q,
        double ndensity,
        double v_core,
        double v_corona,
        double solvent_sld,
        double core_sld,
        double corona_sld,
        double radius_core,
        double rg,
        double d_penetration,
        double n_aggreg)
{
    const double rho_solv = solvent_sld;     // sld of solvent [1/A^2]
    const double rho_core = core_sld;        // sld of core [1/A^2]
    const double rho_corona = corona_sld;    // sld of corona [1/A^2]

    const double beta_core = v_core * (rho_core - rho_solv);
    const double beta_corona = v_corona * (rho_corona - rho_solv);

    // Self-correlation term of the core
    const double bes_core = sas_3j1x_x(q*radius_core);
    const double term1 = square(n_aggreg*beta_core*bes_core);

    // Self-correlation term of the chains
    const double qrg2 = square(q*rg);
    const double debye_chain = (qrg2 == 0.0) ? 1.0 : 2.0*(expm1(-qrg2)+qrg2)/(qrg2*qrg2);
    const double term2 = n_aggreg * beta_corona * beta_corona * debye_chain;

    // Interference cross-term between core and chains
    const double chain_ampl = (qrg2 == 0.0) ? 1.0 : -expm1(-qrg2)/qrg2;
    const double bes_corona = sas_sinx_x(q*(radius_core + d_penetration * rg));
    const double term3 = 2.0 * n_aggreg * n_aggreg * beta_core * beta_corona *
                 bes_core * chain_ampl * bes_corona;

    // Interference cross-term between chains
    const double term4 = n_aggreg * (n_aggreg - 1.0)
                 * square(beta_corona * chain_ampl * bes_corona);

    // I(q)_micelle : Sum of 4 terms computed above
    double i_micelle = term1 + term2 + term3 + term4;

    // rescale from [A^2] to [cm^2]
    i_micelle *= 1.0e-13;

    // "normalize" by number density --> intensity in [cm-1]
    i_micelle *= ndensity;

    return(i_micelle);

}

double Iq(double q,
        double ndensity,
        double v_core,
        double v_corona,
        double solvent_sld,
        double core_sld,
        double corona_sld,
        double radius_core,
        double rg,
        double d_penetration,
        double n_aggreg)
{
    return micelle_spherical_kernel(q,
            ndensity,
            v_core,
            v_corona,
            solvent_sld,
            core_sld,
            corona_sld,
            radius_core,
            rg,
            d_penetration,
            n_aggreg);
}
