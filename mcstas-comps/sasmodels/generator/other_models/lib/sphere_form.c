double sphere_volume(double radius);
double sphere_form(double q, double radius, double sld, double solvent_sld);

    
#pragma acc routine seq
double sphere_volume(double radius)
{
    return M_4PI_3*cube(radius);
}
    
#pragma acc routine seq
double sphere_form(double q, double radius, double sld, double solvent_sld)
{
    const double fq = sphere_volume(radius) * sas_3j1x_x(q*radius);
    const double contrast = (sld - solvent_sld);
    return 1.0e-4*square(contrast * fq);
}

