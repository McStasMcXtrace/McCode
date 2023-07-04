#ifndef _CL_COMPLEX_H
#define _CL_COMPLEX_H
/*
Simple complex library for OpenCL

TODO: use more robust algorithms for complex math functions
TODO: implement inverse trig functions for complex values

declare:     cdouble x
define:      cplx(real, imag)
x.real:      creal(x)
x.imag:      cimag(x)
1j:          I
real + x:    radd(real, x)
real - x:    rsub(real, x)
real * x:    rmul(real, x)
real / x:    rdiv(real, x)
x + real:    radd(real, x)
x - real:    radd(-real, x)
x * real:    rmul(real, x)
x / real:    rmul(1.0/real, x)
x + y:       cadd(x,y)
x - y:       csub(x,y)
x * y:       cmul(x,y)
x / y:       cdiv(x,y)
-x:          cneg(x)
abs(x):      cabs(x)
angle(x):    carg(x)

Math functions:  f(x) -> cf(x)
  sqrt, exp, pow, log, log10
  sin, cos, tan
  sinh, cosh, tanh

*/

// C99 environments without <complex.h> should define __STDC_NO_COMPLEX__
// https://en.cppreference.com/w/c/numeric/complex
#if !defined(USE_OPENCL) && !defined(__STD_NO_COMPLEX__)
   #define HAVE_COMPLEX_H
#endif

#ifdef HAVE_COMPLEX_H

  // Use C99 complex support for all operations.
  #include <complex.h>
  // Note: leave two spaces between double and cdouble because double->float
  // in generate._convert_type is not clever enough.
  typedef complex double  cdouble;
  inline cdouble cplx(const double x, const double y) { return x + y*I; }
  inline cdouble cneg(const cdouble a) { return -a; }
  inline cdouble radd(const double a, const cdouble b) { return a+b; }
  inline cdouble rmul(const double a, const cdouble b) { return a*b; }
  inline cdouble cadd(const cdouble a, const cdouble b) { return a+b; }
  inline cdouble csub(const cdouble a, const cdouble b) { return a-b; }
  inline cdouble cmul(const cdouble a, const cdouble b) { return a*b; }
  inline cdouble cdiv(const cdouble a, const cdouble b) { return a/b; }

#else

// two component vector to hold the real and imaginary parts of a complex number:
#if 0 // Use double2 vector to represent complex numbers
// double2 should be faster than using a structure for simple add and rmul

typedef double2  cdouble; // Note: extra space needed by double->float in cl_util.py
inline cdouble cplx(const double x, const double y) { return (cdouble)(x,y); }
inline double cabs(const cdouble a) { return length(a); }
inline cdouble cneg(const cdouble a) { return -a; }
inline cdouble radd(const double a, const cdouble b) { return a+b; }
inline cdouble rmul(const double a, const cdouble b) { return a*b; }
inline cdouble cadd(const cdouble a, const cdouble b) { return a+b; }
inline cdouble csub(const cdouble a, const cdouble b) { return a-b; }

#else // Use simple structure to represent complex numbers

typedef struct { double x,y; } cdouble;
inline cdouble cplx(const double x, const double y) { cdouble z = {x,y}; return z; }
inline double cabs(const cdouble a) { return sqrt(a.x*a.x + a.y*a.y); }
inline cdouble cneg(const cdouble a) { return cplx(-a.x, -a.y); }
inline cdouble rmul(const double a, const cdouble b) { return cplx( a*b.x, a*b.y ); }
inline cdouble cadd(const cdouble a, const cdouble b) { return cplx( a.x+b.x, a.y+b.y ); }
inline cdouble csub(const cdouble a, const cdouble b) { return cplx( a.x-b.x, a.y-b.y ); }

#endif


#define I (cplx(0.0, 1.0))

inline double creal(const cdouble a) { return a.x; }
inline double cimag(const cdouble a) { return a.y; }
inline double carg(const cdouble a) { return atan2(a.y, a.x); }

inline cdouble cpolar(const double r, const double theta) {
#ifdef USE_SINCOS
    double si, ci;
    SINCOS(theta, si, ci);
    return cplx(r*ci, r*si);
#else
    return cplx(r*cos(theta), r*sin(theta));
#endif
}

inline cdouble  cmul(const cdouble a, const cdouble b) {
    return cplx( a.x*b.x - a.y*b.y, a.x*b.y + a.y*b.x );
}

inline cdouble radd(const double a, const cdouble b) { return cplx( a+b.x, b.y );  }
inline cdouble rsub(const double a, const cdouble b) { return cplx( a-b.x, -b.y ); }
inline cdouble addr(const cdouble a, const double b) { return cplx( a.x+b, a.y );  }
inline cdouble subr(const cdouble a, const double b) { return cplx( a.x-b, a.y ); }


// Complex division
#if 1   // Simple calculation
  inline cdouble cdiv(const cdouble a, const cdouble b) {
    const double scale = 1.0/(b.x*b.x  + b.y*b.y);
    return cplx(scale*(a.x*b.x+a.y*b.y), scale*(a.y*b.x - a.x*b.y));
  }
  inline cdouble rdiv(const double a, const cdouble b) {
    const double scale = a/(b.x*b.x  + b.y*b.y);
    return cplx(scale*b.x, -scale*b.y);
  }
#else  // Stable calculation
  inline cdouble cdiv(const cdouble a, const cdouble b) {
    /* Robert L. Smith, Algorithm 116: Complex division,
     * Communications of the ACM, v.5 n.8, p.435, Aug. 1962
     * */
    if (fabs(b.x) >= fabs(b.y)) {
        const double t = b.y/b.x;
        const double den = b.x + b.y*t;
        const double u = (a.x + a.y*t)/den;
        const double v = (a.y - a.x*t)/den;
        return cplx(u,v);
    } else {
        const double t = b.x/b.y;
        const double den = b.x*t + b.y;
        const double u = (a.x*t + a.y)/den;
        const double v = (a.y*t - a.x)/den;
        return cplx(u,v);
    }
  }
  inline cdouble rdiv(const double a, const cdouble b) {
    if (fabs(b.x) >= fabs(b.y)) {
        const double t = b.y/b.x;
        const double den = b.x + b.y*t;
        const double u = a/den;
        return cplx(u,-t*u);
    } else {
        const double t = b.x/b.y;
        const double den = b.x*t + b.y;
        const double v = a/den;
        return cplx(t*v,-v);
    }
  }
#endif

/*
 *  Square root of complex number.
 *  Although a complex number has two square roots, numerically we will
 *  only determine one of them -the principal square root, see wikipedia
 *  for more info:
 *  http://en.wikipedia.org/wiki/Square_root#Principal_square_root_of_a_complex_number
 */
#if 1 // simple calculation
  inline cdouble csqrt(const cdouble a) {
    const double mod = sqrt(cabs(a));
    const double arg = 0.5*carg(a);
    return cpolar(mod, arg);
  }
#else // stable calculation, with different branch for -0+1j
  inline cdouble csqrt(const cdouble z) {
    const double a = z.x;
    const double b = z.y;
    if (a == 0.0) {
        const double real = sqrt(0.5*fabs(b));
        const double imag = a < 0.0 ? -real : real;
        return cplx(real, imag);
    } else {
        const double t = sqrt(2.0 * (cabs(z) + fabs(a)));
        const double u = 0.5*t;
        if (a > 0.0) {
            return cplx(u,b/t);
        } else {
            const double real = fabs(b)/t;
            const double imag = (b<0.0 ? -u : u);
            return cplx(real,imag);
        }
    }
  }
#endif

inline cdouble csin(const cdouble z) {
    const double a = z.x, b = z.y;
    return cplx(sin(a)*cosh(b), -cos(a)*sinh(b));
}
inline cdouble csinh(const cdouble z) {
    const double a = z.x, b = z.y;
    return cplx(sinh(a)*cos(b), -cosh(a)*sin(b));
}
inline cdouble ccos(const cdouble z) {
    const double a = z.x, b = z.y;
    return cplx(cos(a)*cosh(b), -sin(a)*sinh(b));
}
inline cdouble ccosh(const cdouble z) {
    const double a = z.x;
    const double b = z.y;
    return cplx(cosh(a)*cos(b), -sinh(a)*sin(b));
}
inline cdouble ctan(const cdouble z) { return cdiv(csin(z),ccos(z)); }
inline cdouble ctanh(const cdouble z) { return cdiv(csinh(z),ccosh(z)); }
inline cdouble cexp(const cdouble z) { return cpolar(exp(z.x), z.y); }
// z = re^iw => log(z) = log(r) + iw
inline cdouble clog(const cdouble z) { return cplx(log(cabs(z)), carg(z)); }
inline cdouble clog10(const cdouble z) { return cplx(log10(cabs(z)), carg(z)/2.30258509299405); }
inline cdouble cpow(const cdouble a, const cdouble b) { return cexp(cmul(b, clog(a))); }
inline cdouble cpowr(const cdouble a, const double b) { return cexp(rmul(b, clog(a))); }
inline cdouble crpow(const double a, const cdouble b) { return cexp(rmul(log(a), b)); }

#endif /* USE_OPENCL */
#endif /* _CL_COMPLEX_H */
