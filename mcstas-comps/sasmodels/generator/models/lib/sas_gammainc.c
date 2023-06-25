#if FLOAT_SIZE>4  // double precision
// based on cephes/double/igam.c
double gammaln(double x);
double gammainc(double a, double x);
double gammaincc(double a, double x);

double cephes_igamc(double a, double x);
double cephes_igam(double a, double x);
double cephes_lgam(double x);
double cephes_lgam2(double x);

#pragma acc routine seq
double gammainc(double a, double x)
{
    if ((x <= 0) || ( a <= 0)) return 0.0;
    if ((x > 1.0) && (x > a)) return 1.0 - cephes_igamc(a, x);
    return cephes_igam(a, x);
}

#pragma acc routine seq
double gammaincc(double a, double x)
{
    if ((x <= 0) || (a <= 0)) return 1.0;
    if ((x < 1.0) || (x < a)) return 1.0 - cephes_igam(a, x);
    return cephes_igamc(a, x);
}

#pragma acc routine seq
double gammaln(double x)
{
    if (isnan(x)) return(x);
    if (!isfinite(x)) return(INFINITY);
    return cephes_lgam(x);
}

#pragma acc routine seq
double cephes_igamc(double a, double x)
{
    const double MACHEP = 1.11022302462515654042E-16; // IEEE 2**-53
    const double MAXLOG = 7.09782712893383996843E2; // IEEE log(2**1024) denormalized
    const double BIG = 4.503599627370496e15;
    const double BIGINV = 2.22044604925031308085e-16;
    double ans, ax, c, yc, r, t, y, z;
    double pk, pkm1, pkm2, qk, qkm1, qkm2;

    /* Compute  x**a * exp(-x) / gamma(a)  */
    ax = a * log(x) - x - cephes_lgam(a);
    if (ax < -MAXLOG) return 0.0;  // underflow
    ax = exp(ax);

    /* continued fraction */
    y = 1.0 - a;
    z = x + y + 1.0;
    c = 0.0;
    pkm2 = 1.0;
    qkm2 = x;
    pkm1 = x + 1.0;
    qkm1 = z * x;
    ans = pkm1/qkm1;

    do {
        c += 1.0;
        y += 1.0;
        z += 2.0;
        yc = y * c;
        pk = pkm1 * z  -  pkm2 * yc;
        qk = qkm1 * z  -  qkm2 * yc;
        if (qk != 0) {
            r = pk/qk;
            t = fabs( (ans - r)/r );
            ans = r;
        } else {
            t = 1.0;
        }
        pkm2 = pkm1;
        pkm1 = pk;
        qkm2 = qkm1;
        qkm1 = qk;
        if (fabs(pk) > BIG) {
            pkm2 *= BIGINV;
            pkm1 *= BIGINV;
            qkm2 *= BIGINV;
            qkm1 *= BIGINV;
        }
    } while( t > MACHEP );

    return( ans * ax );
}

#pragma acc routine seq
double cephes_igam(double a, double x)
{
    const double MACHEP = 1.11022302462515654042E-16; // IEEE 2**-53
    const double MAXLOG = 7.09782712893383996843E2; // IEEE log(2**1024) denormalized
    double ans, ax, c, r;

    /* Compute  x**a * exp(-x) / gamma(a)  */
    ax = a * log(x) - x - cephes_lgam(a);
    if (ax < -MAXLOG) return 0.0; // underflow
    ax = exp(ax);

    /* power series */
    r = a;
    c = 1.0;
    ans = 1.0;

    do {
        r += 1.0;
        c *= x/r;
        ans += c;
    } while (c/ans > MACHEP);

    return ans * ax/a;
}

/* Logarithm of gamma function */

#pragma acc routine seq
double cephes_lgam(double x)
{
    const double LOGPI = 1.14472988584940017414; // log(pi)
    int sgngam = 1;
    double p, q, w, z;
    int i;

    if (x < -34.0) {
        q = -x;
        w = cephes_lgam2(q);
        p = floor(q);
        if (p == q) return INFINITY;
        i = p;
        sgngam = ((i&1) == 0) ? -1 : 1;
        z = q - p;
        if (z > 0.5) {
            p += 1.0;
            z = p - q;
        }
        z = q * sin(M_PI * z);
        if (z == 0.0) return INFINITY;
        z = LOGPI - log(z) - w;
        return z;
    } else {
        return cephes_lgam2(x);
    }
}
#pragma acc routine seq
double cephes_lgam2(double x)
{
    const double LS2PI = 0.91893853320467274178; // log(sqrt(2*pi))
    const double MAXLGM = 2.556348e305;
    int sgngam = 1;
    double p, q, u, z;
    double A, B, C;

    if (x < 13.0) {
        z = 1.0;
        p = 0.0;
        u = x;
        while (u >= 3.0) {
            p -= 1.0;
            u = x + p;
            z *= u;
        }
        while (u < 2.0) {
            if (u == 0.0) return INFINITY;
            z /= u;
            p += 1.0;
            u = x + p;
        }
        if (z < 0.0) {
            sgngam = -1;
            z = -z;
        } else {
            sgngam = 1;
        }
        if (u == 2.0) return log(z);
        p -= 2.0;
        x = x + p;
        B = ((((((
            -1.37825152569120859100E3)*x
            -3.88016315134637840924E4)*x
            -3.31612992738871184744E5)*x
            -1.16237097492762307383E6)*x
            -1.72173700820839662146E6)*x
            -8.53555664245765465627E5);
        C = ((((((
            /* 1.00000000000000000000E0)* */x
            -3.51815701436523470549E2)*x
            -1.70642106651881159223E4)*x
            -2.20528590553854454839E5)*x
            -1.13933444367982507207E6)*x
            -2.53252307177582951285E6)*x
            -2.01889141433532773231E6);
        p = x * B / C;
        return log(z) + p;
    }

    if (x > MAXLGM) return sgngam * INFINITY;

    q = (x - 0.5) * log(x) - x + LS2PI;
    if (x > 1.0e8) return q;

    p = 1.0/(x*x);
    if (x >= 1000.0) {
        q += ((7.9365079365079365079365e-4 * p
            - 2.7777777777777777777778e-3) * p
            + 0.0833333333333333333333) / x;
    } else {
        A = (((((
            +8.11614167470508450300E-4)*p
            -5.95061904284301438324E-4)*p
            +7.93650340457716943945E-4)*p
            -2.77777777730099687205E-3)*p
            +8.33333333333331927722E-2);
        q += A / x;
    }
    return q;
}

#else // single precision

// based on cephes/float/igam.c
float gammalnf(float x);
float gammaincf(float a, float x);
float gammainccf(float a, float x);

float cephes_igamcf(float a, float x);
float cephes_igamf(float a, float x);
float cephes_lgamf(float x);
float cephes_lgam2f(float x);

// Note: original uses logf, fabsf, floorf and sinf
#pragma acc routine seq
float gammaincf(float a, float x)
{
    if ((x <= 0) || ( a <= 0)) return 0.0;
    if ((x > 1.0) && (x > a)) return 1.0 - cephes_igamcf(a, x);
    return cephes_igamf(a, x);
}

#pragma acc routine seq
float gammainccf(float a, float x)
{
    if ((x <= 0) || (a <= 0)) return 1.0;
    if ((x < 1.0) || (x < a)) return 1.0 - cephes_igamf(a, x);
    return cephes_igamcf(a, x);
}

#pragma acc routine seq
float gammalnf(float x)
{
    if (isnan(x)) return(x);
    if (!isfinite(x)) return(INFINITY);
    return cephes_lgamf(x);
}

#pragma acc routine seq
float cephes_igamcf(float a, float x)
{
    const float MAXLOGF = 88.72283905206835;
    const float MACHEPF = 5.9604644775390625E-8;
    const float BIG = 16777216.;
    float ans, c, yc, ax, y, z;
    float pk, pkm1, pkm2, qk, qkm1, qkm2;
    float r, t;

    ax = a * log(x) - x - cephes_lgamf(a);
    if (ax < -MAXLOGF) return 0.0; // underflow
    ax = expf(ax);

    /* continued fraction */
    y = 1.0 - a;
    z = x + y + 1.0;
    c = 0.0;
    pkm2 = 1.0;
    qkm2 = x;
    pkm1 = x + 1.0;
    qkm1 = z * x;
    ans = pkm1/qkm1;

    do {
        c += 1.0;
        y += 1.0;
        z += 2.0;
        yc = y * c;
        pk = pkm1 * z  -  pkm2 * yc;
        qk = qkm1 * z  -  qkm2 * yc;
        if (qk != 0) {
            r = pk/qk;
            t = fabs((ans - r)/r);
            ans = r;
        } else {
            t = 1.0;
        }
        pkm2 = pkm1;
        pkm1 = pk;
        qkm2 = qkm1;
        qkm1 = qk;
        if (fabs(pk) > BIG) {
            pkm2 *= MACHEPF;
            pkm1 *= MACHEPF;
            qkm2 *= MACHEPF;
            qkm1 *= MACHEPF;
        }
    } while (t > MACHEPF);

    return ans * ax;
}

#pragma acc routine seq
float cephes_igamf(float a, float x)
{
    const float MAXLOGF = 88.72283905206835;
    const float MACHEPF = 5.9604644775390625E-8;
    float ans, ax, c, r;

    /* Compute  x**a * exp(-x) / gamma(a)  */
    ax = a * log(x) - x - cephes_lgamf(a);
    if (ax < -MAXLOGF) return 0.0; // underflow
    ax = expf(ax);

    /* power series */
    r = a;
    c = 1.0;
    ans = 1.0;

    do {
        r += 1.0;
        c *= x/r;
        ans += c;
    } while (c/ans > MACHEPF);

    return ans * ax/a;
}

#pragma acc routine seq
float cephes_lgamf(float x)
{
    const float PIINV =  0.318309886183790671538;
    float p, q, w, z;
    int i;
    int sgngamf;

    if (x < 0.0) {
        q = -x;
        w = cephes_lgam2f(q);
        p = floor(q);
        if (p == q) return INFINITY;
        i = p;
        sgngamf = ((i&1) == 0) ? -1 : 1;
        z = q - p;
        if (z > 0.5) {
            p += 1.0;
            z = p - q;
        }
        z = q * sin(M_PI * z);
        if (z == 0.0) return sgngamf * INFINITY;
        z = -log(PIINV*z) - w;
        return z;
    } else {
        return cephes_lgam2f(x);
    }
}

#pragma acc routine seq
float cephes_lgam2f(float x)
{
    const float LS2PI = 0.91893853320467274178; // log(sqrt(2*pi))
    const float MAXLGM = 2.035093e36;
    float p, q, z;
    float nx, tx;
    int direction;
    int sgngamf = 1;

    if (x < 6.5) {
        direction = 0;
        z = 1.0;
        tx = x;
        nx = 0.0;
        if (x >= 1.5) {
            while (tx > 2.5) {
                nx -= 1.0;
                tx = x + nx;
                z *=tx;
            }
            x += nx - 2.0;
        } else if (x >= 1.25) {
            z *= x;
            x -= 1.0; /* x + 1 - 2 */
            direction = 1;
        } else if (x >= 0.75) {
            x -= 1.0;
            /* log gamma(x+1), -.25 < x < .25 */
            // p = x * polevlf( x, C, 7 );
            p = ((((((((
                +1.369488127325832E-001)*x
                -1.590086327657347E-001)*x
                +1.692415923504637E-001)*x
                -2.067882815621965E-001)*x
                +2.705806208275915E-001)*x
                -4.006931650563372E-001)*x
                +8.224670749082976E-001)*x
                -5.772156501719101E-001)*x;
            q = 0.0;
            return p + q;
        } else {
            while (tx < 1.5) {
                if (tx == 0.0) return sgngamf * INFINITY;
                z *=tx;
                nx += 1.0;
                tx = x + nx;
            }
            direction = 1;
            x += nx - 2.0;
        }

        /* log gamma(x+2), -.5 < x < .5 */
        // p = x * polevlf( x, B, 7 );
        p = ((((((((
            +6.055172732649237E-004)*x
            -1.311620815545743E-003)*x
            +2.863437556468661E-003)*x
            -7.366775108654962E-003)*x
            +2.058355474821512E-002)*x
            -6.735323259371034E-002)*x
            +3.224669577325661E-001)*x
            +4.227843421859038E-001)*x;

        if (z < 0.0) {
            sgngamf = -1;
            z = -z;
        } else {
            sgngamf = 1;
        }
        q = log(z);
        if (direction) q = -q;
        return p + q;
    } else if (x > MAXLGM) {
        return sgngamf * INFINITY;
    } else {
        /* Note, though an asymptotic formula could be used for x >= 3,
        * there is cancellation error in the following if x < 6.5.
        */
        q = LS2PI - x;
        q += (x - 0.5) * log(x);

        if (x <= 1.0e4) {
            z = 1.0/x;
            p = z * z;
            q += ((6.789774945028216E-004 * p
                - 2.769887652139868E-003 ) * p
                +  8.333316229807355E-002 ) * z;
        }
        return q;
    }
}

#endif // !single precision

#if FLOAT_SIZE>4
#define sas_gammaln gammaln
#define sas_gammainc gammainc
#define sas_gammaincc gammaincc
#else
#define sas_gammaln gammalnf
#define sas_gammainc gammaincf
#define sas_gammaincc gammainccf
#endif
