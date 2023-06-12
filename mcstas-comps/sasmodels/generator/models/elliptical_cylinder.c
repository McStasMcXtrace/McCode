static double
form_volume(double radius_minor, double r_ratio, double length)
{
    return M_PI * radius_minor * radius_minor * r_ratio * length;
}

static double
radius_from_excluded_volume(double radius_minor, double r_ratio, double length)
{
    const double r_equiv = sqrt(radius_minor*radius_minor*r_ratio);
    return 0.5*cbrt(0.75*r_equiv*(2.0*r_equiv*length + (r_equiv + length)*(M_PI*r_equiv + length)));
}

static double
radius_from_volume(double radius_minor, double r_ratio, double length)
{
    const double volume_ellcyl = form_volume(radius_minor,r_ratio,length);
    return cbrt(volume_ellcyl/M_4PI_3);
}

static double
radius_from_min_dimension(double radius_minor, double r_ratio, double hlength)
{
    const double rad_min = (r_ratio > 1.0 ? radius_minor : r_ratio*radius_minor);
    return (rad_min < hlength ? rad_min : hlength);
}

static double
radius_from_max_dimension(double radius_minor, double r_ratio, double hlength)
{
    const double rad_max = (r_ratio < 1.0 ? radius_minor : r_ratio*radius_minor);
    return (rad_max > hlength ? rad_max : hlength);
}

static double
radius_from_diagonal(double radius_minor, double r_ratio, double length)
{
    const double radius_max = (r_ratio > 1.0 ? radius_minor*r_ratio : radius_minor);
    return sqrt(radius_max*radius_max + 0.25*length*length);
}

static double
radius_effective(int mode, double radius_minor, double r_ratio, double length)
{
    switch (mode) {
    default:
    case 1: // equivalent cylinder excluded volume
        return radius_from_excluded_volume(radius_minor, r_ratio, length);
    case 2: // equivalent volume sphere
        return radius_from_volume(radius_minor, r_ratio, length);
    case 3: // average radius
        return 0.5*radius_minor*(1.0 + r_ratio);
    case 4: // min radius
        return (r_ratio > 1.0 ? radius_minor : r_ratio*radius_minor);
    case 5: // max radius
        return (r_ratio < 1.0 ? radius_minor : r_ratio*radius_minor);
    case 6: // equivalent circular cross-section
        return sqrt(radius_minor*radius_minor*r_ratio);
    case 7: // half length
        return 0.5*length;
    case 8: // half min dimension
        return radius_from_min_dimension(radius_minor,r_ratio,0.5*length);
    case 9: // half max dimension
        return radius_from_max_dimension(radius_minor,r_ratio,0.5*length);
    case 10: // half diagonal
        return radius_from_diagonal(radius_minor,r_ratio,length);
    }
}

static void
Fq(double q, double *F1, double *F2, double radius_minor, double r_ratio, double length,
   double sld, double solvent_sld)
{
    // orientational average limits
    const double va = 0.0;
    const double vb = 1.0;
    // inner integral limits
    const double vaj=0.0;
    const double vbj=M_PI;

    const double radius_major = r_ratio * radius_minor;
    const double rA = 0.5*(square(radius_major) + square(radius_minor));
    const double rB = 0.5*(square(radius_major) - square(radius_minor));

    //initialize integral
    double outer_sum_F1 = 0.0;
    double outer_sum_F2 = 0.0;
    for(int i=0;i<GAUSS_N;i++) {
        //setup inner integral over the ellipsoidal cross-section
        const double cos_val = ( GAUSS_Z[i]*(vb-va) + va + vb )/2.0;
        const double sin_val = sqrt(1.0 - cos_val*cos_val);
        //const double arg = radius_minor*sin_val;
        double inner_sum_F1 = 0.0;
        double inner_sum_F2 = 0.0;
        for(int j=0;j<GAUSS_N;j++) {
            const double theta = ( GAUSS_Z[j]*(vbj-vaj) + vaj + vbj )/2.0;
            const double r = sin_val*sqrt(rA - rB*cos(theta));
            const double be = sas_2J1x_x(q*r);
            inner_sum_F1 += GAUSS_W[j] * be;
            inner_sum_F2 += GAUSS_W[j] * be * be;
        }
        //now calculate the value of the inner integral
        inner_sum_F1 *= 0.5*(vbj-vaj);
        inner_sum_F2 *= 0.5*(vbj-vaj);

        //now calculate outer integral
        const double si = sas_sinx_x(q*0.5*length*cos_val);
        outer_sum_F1 += GAUSS_W[i] * inner_sum_F1 * si;
        outer_sum_F2 += GAUSS_W[i] * inner_sum_F2 * si * si;
    }
    // correct limits and divide integral by pi
    outer_sum_F1 *= 0.5*(vb-va)/M_PI;
    outer_sum_F2 *= 0.5*(vb-va)/M_PI;

    // scale by contrast and volume, and convert to to 1/cm units
    const double volume = form_volume(radius_minor, r_ratio, length);
    const double contrast = sld - solvent_sld;
    const double s = contrast*volume;
    *F1 = 1.0e-2*s*outer_sum_F1;
    *F2 = 1.0e-4*s*s*outer_sum_F2;
}


static double
Iqabc(double qa, double qb, double qc,
     double radius_minor, double r_ratio, double length,
     double sld, double solvent_sld)
{
    // Compute:  r = sqrt((radius_major*cos_nu)^2 + (radius_minor*cos_mu)^2)
    // Given:    radius_major = r_ratio * radius_minor
    const double qr = radius_minor*sqrt(square(r_ratio*qb) + square(qa));
    const double be = sas_2J1x_x(qr);
    const double si = sas_sinx_x(qc*0.5*length);
    const double fq = be * si;
    const double contrast = sld - solvent_sld;
    const double volume = form_volume(radius_minor, r_ratio, length);
    return 1.0e-4 * square(contrast * volume * fq);
}
