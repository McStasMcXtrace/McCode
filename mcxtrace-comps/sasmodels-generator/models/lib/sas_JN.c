/*							jn.c
 *
 *	Bessel function of integer order
 *
 *
 *
 * SYNOPSIS:
 *
 * int n;
 * double x, y, jn();
 *
 * y = jn( n, x );
 *
 *
 *
 * DESCRIPTION:
 *
 * Returns Bessel function of order n, where n is a
 * (possibly negative) integer.
 *
 * The ratio of jn(x) to j0(x) is computed by backward
 * recurrence.  First the ratio jn/jn-1 is found by a
 * continued fraction expansion.  Then the recurrence
 * relating successive orders is applied until j0 or j1 is
 * reached.
 *
 * If n = 0 or 1 the routine for j0 or j1 is called
 * directly.
 *
 *
 *
 * ACCURACY:
 *
 *                      Absolute error:
 * arithmetic   range      # trials      peak         rms
 *    DEC       0, 30        5500       6.9e-17     9.3e-18
 *    IEEE      0, 30        5000       4.4e-16     7.9e-17
 *
 *
 * Not suitable for large n or x. Use jv() instead.
 *
 */

/*							jn.c
Cephes Math Library Release 2.8:  June, 2000
Copyright 1984, 1987, 2000 by Stephen L. Moshier
*/

#if FLOAT_SIZE > 4

double cephes_jn( int n, double x );

    
#pragma acc routine seq
double cephes_jn( int n, double x ) {

    // PAK: seems to be machine epsilon/2
    const double MACHEP = 1.11022302462515654042E-16;
    double pkm2, pkm1, pk, xk, r, ans;
    int k, sign;

    if( n < 0 ) {
        n = -n;
        if( (n & 1) == 0 )	/* -1**n */
            sign = 1;
        else
            sign = -1;
    } else {
        sign = 1;
    }

    if( x < 0.0 ) {
        if( n & 1 )
            sign = -sign;
    x = -x;
    }

    if( n == 0 )
        return( sign * cephes_j0(x) );
    if( n == 1 )
        return( sign * cephes_j1(x) );
    if( n == 2 )
        return( sign * (2.0 * cephes_j1(x) / x  -  cephes_j0(x)) );

    if( x < MACHEP )
        return( 0.0 );

    k = 53;
    pk = 2 * (n + k);
    ans = pk;
    xk = x * x;

    do {
        pk -= 2.0;
        ans = pk - (xk/ans);
    } while( --k > 0 );

    /* backward recurrence */

    pk = 1.0;

    ans = x/ans;
    pkm1 = 1.0/ans;

    k = n-1;
    r = 2 * k;

    do {
        pkm2 = (pkm1 * r  -  pk * x) / x;
        pk = pkm1;
        pkm1 = pkm2;
        r -= 2.0;
    } while( --k > 0 );

    if( fabs(pk) > fabs(pkm1) )
        ans = cephes_j1(x)/pk;
    else
        ans = cephes_j0(x)/pkm1;

    return( sign * ans );
}

#else

float cephes_jnf(int n, float x);

    
#pragma acc routine seq
float cephes_jnf(int n, float x)
{
    // PAK: seems to be machine epsilon/2
    const double MACHEP = 5.9604645e-08;
    float pkm2, pkm1, pk, xk, r, ans;
    int k, sign;

    if( n < 0 ) {
        n = -n;
        if( (n & 1) == 0 ) /* -1**n */
            sign = 1;
        else
            sign = -1;
    } else {
        sign = 1;
    }

    if( x < 0.0 ) {
        if( n & 1 )
            sign = -sign;
        x = -x;
    }

    if( n == 0 )
        return( sign * cephes_j0f(x) );
    if( n == 1 )
        return( sign * cephes_j1f(x) );
    if( n == 2 )
        return( sign * (2.0 * cephes_j1f(x) / x  -  cephes_j0f(x)) );

    if( x < MACHEP )
        return( 0.0 );

    k = 24;
    pk = 2 * (n + k);
    ans = pk;
    xk = x * x;

    do {
        pk -= 2.0;
        ans = pk - (xk/ans);
    } while( --k > 0 );

    /* backward recurrence */

    pk = 1.0;

    const float xinv = 1.0/x;
    pkm1 = ans * xinv;
    k = n-1;
    r = (float )(2 * k);

    do {
        pkm2 = (pkm1 * r  -  pk * x) * xinv;
        pk = pkm1;
        pkm1 = pkm2;
        r -= 2.0;
    } while( --k > 0 );

    r = pk;
    if( r < 0 )
        r = -r;
    ans = pkm1;
    if( ans < 0 )
        ans = -ans;

    if( r > ans )  /* if( fabs(pk) > fabs(pkm1) ) */
        ans = sign * cephes_j1f(x)/pk;
    else
        ans = sign * cephes_j0f(x)/pkm1;
    return( ans );
}
#endif

#if FLOAT_SIZE>4
#define sas_JN cephes_jn
#else
#define sas_JN cephes_jnf
#endif
