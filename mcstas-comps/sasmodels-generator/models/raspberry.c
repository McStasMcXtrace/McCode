double form_volume(double radius_lg, double radius_sm, double penetration);

double Iq(double q,
          double sld_lg, double sld_sm, double sld_solvent,
          double volfraction_lg, double volfraction_sm, double surf_fraction,
          double radius_lg, double radius_sm, double penetration);

double form_volume(double radius_lg, double radius_sm, double penetration)
{
    //Because of the complex structure, volume normalization must
    //happen in the Iq code below.  Thus the form volume is set to 1.0 here
    double volume=1.0;
    return volume;
}

static double
radius_effective(int mode, double radius_lg, double radius_sm, double penetration)
{
    switch (mode) {
    default:
    case 1: // radius_large
        return radius_lg;
    case 2: // radius_outer
        return radius_lg + 2.0*radius_sm - penetration;
    }
}

double Iq(double q,
          double sld_lg, double sld_sm, double sld_solvent,
          double volfraction_lg, double volfraction_sm, double surface_fraction,
          double radius_lg, double radius_sm, double penetration)
{
    // Ref: J. coll. inter. sci. (2010) vol. 343 (1) pp. 36-41.


    double vfL, rL, sldL, vfS, rS, sldS, deltaS, delrhoL, delrhoS, sldSolv;
    double VL, VS, Np, f2, fSs;
    double psiL,psiS;
    double sfLS,sfSS;
    double slT;

    vfL = volfraction_lg;
    rL = radius_lg;
    sldL = sld_lg;
    vfS = volfraction_sm;
    fSs = surface_fraction;
    rS = radius_sm;
    sldS = sld_sm;
    deltaS = penetration;
    sldSolv = sld_solvent;

    delrhoL = fabs(sldL - sldSolv);
    delrhoS = fabs(sldS - sldSolv);

    VL = M_4PI_3*rL*rL*rL;
    VS = M_4PI_3*rS*rS*rS;

    //Number of small particles per large particle
    Np = vfS*fSs*VL/vfL/VS;

    //Total scattering length difference
    slT = delrhoL*VL + Np*delrhoS*VS;

    //Form factors for each particle
    psiL = sas_3j1x_x(q*rL);
    psiS = sas_3j1x_x(q*rS);

    //Cross term between large and small particles
    sfLS = psiL*psiS*sas_sinx_x(q*(rL+deltaS*rS));
    //Cross term between small particles at the surface
    sfSS = psiS*psiS*sas_sinx_x(q*(rL+deltaS*rS))*sas_sinx_x(q*(rL+deltaS*rS));

    //Large sphere form factor term
    f2 = delrhoL*delrhoL*VL*VL*psiL*psiL;
    //Small sphere form factor term
    f2 += Np*delrhoS*delrhoS*VS*VS*psiS*psiS;
    //Small particle - small particle cross term
    f2 += Np*(Np-1)*delrhoS*delrhoS*VS*VS*sfSS;
    //Large-small particle cross term
    f2 += 2*Np*delrhoL*delrhoS*VL*VS*sfLS;
    //Normalise by total scattering length difference
    if (f2 != 0.0){
        f2 = f2/slT/slT;
        }

    //I(q) for large-small composite particles
    f2 = f2*(vfL*delrhoL*delrhoL*VL + vfS*fSs*Np*delrhoS*delrhoS*VS);
    //I(q) for free small particles
    f2+= vfS*(1.0-fSs)*delrhoS*delrhoS*VS*psiS*psiS;

    // normalize to single particle volume and convert to 1/cm
    f2 *= 1.0e8;        // [=] 1/cm
    f2 *= 1.0e-12;      // convert for (1/A^-6)^2 to (1/A)^2

    return f2;
}
