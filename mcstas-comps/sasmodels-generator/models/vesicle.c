static double
shell_volume(double radius, double thickness)
{
    return M_4PI_3 * (cube(radius+thickness) - cube(radius));
}

static double
form_volume(double radius, double thickness)
{
    return M_4PI_3 * cube(radius+thickness);
}


static double
radius_effective(int mode, double radius, double thickness)
{
    // case 1: outer radius
    return radius + thickness;
}

static void
Fq(double q,
    double *F1,
    double *F2,
    double sld,
    double sld_solvent,
    double volfraction,
    double radius,
    double thickness)

/*
   scattering from a unilamellar vesicle.
   same functional form as the core-shell sphere, but more intuitive for
   a vesicle
*/

{
    double vol,contrast,f;

    // core first, then add in shell
    contrast = sld_solvent-sld;
    vol = M_4PI_3*cube(radius);
    f = vol * sas_3j1x_x(q*radius) * contrast;

    //now the shell. No volume normalization as this is done by the caller
    contrast = sld-sld_solvent;
    vol = M_4PI_3*cube(radius+thickness);
    f += vol * sas_3j1x_x(q*(radius+thickness)) * contrast;

    //rescale to [cm-1].
    // With volume fraction as part of the model in the dilute limit need
    // to return F2 = Vf <fq^2>.  In order for beta approx. to work correctly
    // need F1^2/F2 equal to <fq>^2 / <fq^2>.  By returning F1 = sqrt(Vf) <fq>
    // and F2 = Vf <fq^2> both conditions are satisfied.
    // Since Vf is the volume fraction of vesicles of all radii, it is
    // constant when averaging F1 and F2 over radii and so pops out of the
    // polydispersity loop, so it is safe to apply it inside the model
    // (albeit conceptually ugly).
    *F1 = 1e-2 * sqrt(volfraction) * f;
    *F2 = 1.0e-4 * volfraction * f * f;
}
