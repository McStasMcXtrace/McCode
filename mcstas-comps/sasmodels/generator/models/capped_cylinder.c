// Integral over a convex lens kernel for t in [h/R,1].  See the docs for
// the definition of the function being integrated.
//   q is the magnitude of the q vector.
//   h is the length of the lens "inside" the cylinder.  This negative wrt the
//       definition of h in the docs.
//   radius_cap is the radius of the lens
//   length is the cylinder length, or the separation between the lens halves
//   theta is the angle of the cylinder wrt q.
static double
_cap_kernel(double qab, double qc, double h, double radius_cap,
    double half_length)
{
    // translate a point in [-1,1] to a point in [lower,upper]
    const double upper = 1.0;
    const double lower = -h/radius_cap; // integral lower bound
    const double zm = 0.5*(upper-lower);
    const double zb = 0.5*(upper+lower);

    // cos term in integral is:
    //    cos (q (R t - h + L/2) cos(theta))
    // so turn it into:
    //    cos (m t + b)
    // where:
    //    m = q R cos(theta)
    //    b = q(L/2-h) cos(theta)
    const double m = radius_cap*qc; // cos argument slope
    const double b = (half_length+h)*qc; // cos argument intercept
    const double qab_r = radius_cap*qab; // Q*R*sin(theta)
    double total = 0.0;
    for (int i=0; i<GAUSS_N; i++) {
        const double t = GAUSS_Z[i]*zm + zb;
        const double radical = 1.0 - t*t;
        const double bj = sas_2J1x_x(qab_r*sqrt(radical));
        const double Fq = cos(m*t + b) * radical * bj;
        total += GAUSS_W[i] * Fq;
    }
    // translate dx in [-1,1] to dx in [lower,upper]
    const double integral = total*zm;
    const double cap_Fq = 2.0*M_PI*cube(radius_cap)*integral;
    return cap_Fq;
}

static double
_fq(double qab, double qc, double h, double radius_cap, double radius, double half_length)
{
    const double cap_Fq = _cap_kernel(qab, qc, h, radius_cap, half_length);
    const double bj = sas_2J1x_x(radius*qab);
    const double si = sas_sinx_x(half_length*qc);
    const double cyl_Fq = 2.0*M_PI*radius*radius*half_length*bj*si;
    const double Aq = cap_Fq + cyl_Fq;
    return Aq;
}

static double
form_volume(double radius, double radius_cap, double length)
{
    // cap radius should never be less than radius when this is called
    const double h = -sqrt(square(radius_cap) - square(radius));
    const double slice = M_PI*(square(radius_cap)*h - cube(h)/3.0);
    const double hemisphere = 2.0*M_PI/3.0*cube(radius_cap);
    const double rod = M_PI*square(radius)*length;
    // h < 0 so slice is subtracted from hemisphere
    return rod + 2.0*(hemisphere + slice);
}

static double
radius_from_excluded_volume(double radius, double radius_cap, double length)
{
    const double h = -sqrt(square(radius_cap) - square(radius));
    const double length_tot = length + 2.0*(radius_cap + h);
    // Use cylinder excluded volume with length' = length + caps and
    // radius' = cylinder radius since the lens is smaller than the cylinder.
    return 0.5*cbrt(0.75*radius*(2.0*radius*length_tot
           + (radius + length_tot)*(M_PI*radius + length_tot)));
}

static double
radius_from_volume(double radius, double radius_cap, double length)
{
    const double vol_cappedcyl = form_volume(radius,radius_cap,length);
    return cbrt(vol_cappedcyl/M_4PI_3);
}

static double
radius_from_totallength(double radius, double radius_cap, double length)
{
    const double h = -sqrt(square(radius_cap) - square(radius));
    const double half_length = 0.5*length;
    return half_length + radius_cap - h;
}

static double
radius_effective(int mode, double radius, double radius_cap, double length)
{
    switch (mode) {
    default:
    case 1: // equivalent cylinder excluded volume
        return radius_from_excluded_volume(radius, radius_cap, length);
    case 2: // equivalent volume sphere
        return radius_from_volume(radius, radius_cap, length);
    case 3: // radius
        return radius;
    case 4: // half length
        return 0.5*length;
    case 5: // half total length
        return radius_from_totallength(radius, radius_cap,length);
    }
}

static void
Fq(double q,double *F1, double *F2, double sld, double solvent_sld,
    double radius, double radius_cap, double length)
{
    const double h = -sqrt(square(radius_cap) - square(radius));
    const double half_length = 0.5*length;

    // translate a point in [-1,1] to a point in [0, pi/2]
    const double zm = M_PI_4;
    const double zb = M_PI_4;
    double total_F1 = 0.0;
    double total_F2 = 0.0;
    for (int i=0; i<GAUSS_N ;i++) {
        const double theta = GAUSS_Z[i]*zm + zb;
        double sin_theta, cos_theta; // slots to hold sincos function output
        SINCOS(theta, sin_theta, cos_theta);
        const double qab = q*sin_theta;
        const double qc = q*cos_theta;
        const double Aq = _fq(qab, qc, h, radius_cap, radius, half_length);
        // scale by sin_theta for spherical coord integration
        total_F1 += GAUSS_W[i] * Aq * sin_theta;
        total_F2 += GAUSS_W[i] * Aq * Aq * sin_theta;
    }
    // translate dx in [-1,1] to dx in [lower,upper]
    const double form_avg = total_F1 * zm;
    const double form_squared_avg = total_F2 * zm;

    // Contrast
    const double s = (sld - solvent_sld);
    *F1 = 1.0e-2 * s * form_avg;
    *F2 = 1.0e-4 * s * s * form_squared_avg;
}


static double
Iqac(double qab, double qc,
    double sld, double solvent_sld, double radius,
    double radius_cap, double length)
{
    const double h = -sqrt(square(radius_cap) - square(radius));
    const double Aq = _fq(qab, qc, h, radius_cap, radius, 0.5*length);

    // Multiply by contrast^2 and convert to cm-1
    const double s = (sld - solvent_sld);
    return 1.0e-4 * square(s * Aq);
}
