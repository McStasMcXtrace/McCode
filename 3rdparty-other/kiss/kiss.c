

/*
 From: http://www.helsbreth.org/random/rng_kiss.html
 Scott Nelson 1999

 Based on Marsaglia's KISS or (KISS+SWB) <http://www.cs.yorku.ca/~oz/marsaglia-
rng.html>

 KISS - Keep it Simple Stupid PRNG

 the idea is to use simple, fast, individually promising
 generators to get a composite that will be fast, easy to code
 have a very long period and pass all the tests put to it.
 The three components of KISS are
        x(n)=a*x(n-1)+1 mod 2^32
        y(n)=y(n-1)(I+L^13)(I+R^17)(I+L^5),
        z(n)=2*z(n-1)+z(n-2) +carry mod 2^32
 The y's are a shift register sequence on 32bit binary vectors
 period 2^32-1;
 The z's are a simple multiply-with-carry sequence with period
 2^63+2^32-1.  The period of KISS is thus
      2^32*(2^32-1)*(2^63+2^32-1) > 2^127
*/

/* the KISS state is stored as a vector of 7 unsigned long        */
/*   0  1  2  3  4      5  6   */
/* [ x, y, z, w, carry, k, m ] */

#include <stdio.h>
#include <math.h>
#include <stdint.h>
#ifndef ULONG_MAX
#define ULONG_MAX ((unsigned long)0xffffffffffffffffUL)
#endif
#define MC_RAND_MAX ULONG_MAX
#define random  kiss_random
#define srandom kiss_srandom

unsigned long *kiss_srandom(unsigned long state[7], unsigned long seed) {
  if (seed == 0) seed = 1;
  state[0] = seed | 1; // x
  state[1] = seed | 2; // y
  state[2] = seed | 4; // z
  state[3] = seed | 8; // w
  state[4] = 0;        // carry
  return 0;
}

unsigned long kiss_random(unsigned long state[7]) {
    state[0] = state[0] * 69069 + 1;
    state[1] ^= state[1] << 13;
    state[1] ^= state[1] >> 17;
    state[1] ^= state[1] << 5;
    state[5] = (state[2] >> 2) + (state[3] >> 3) + (state[4] >> 2);
    state[6] = state[3] + state[3] + state[2] + state[4];
    state[2] = state[3];
    state[3] = state[6];
    state[4] = state[5] >> 30;
    return state[0] + state[1] + state[3];
}

double uniform_double(unsigned long state[7]) {
    unsigned long a = kiss_random(state) >> 6;
    unsigned long b = kiss_random(state) >> 5;
    double x = (a * 134217728.0 + b) / 9007199254740992.0;
    return x;
}

double rand01(unsigned long state[7]) {
	double randnum;
	randnum = (double) kiss_random(state);
  // TODO: can we mult instead of div?
	randnum /= (double) MC_RAND_MAX +1;
	return randnum;
}

double randnorm(unsigned long state[7])
{
  static double v1, v2, s; /* removing static breaks comparison with McStas <= 2.5 */
  static int phase = 0;
  double X, u1, u2;

  if(phase == 0)
  {
    do
    {
      u1 = rand01(state);
      u2 = rand01(state);
      v1 = 2*u1 - 1;
      v2 = 2*u2 - 1;
      s = v1*v1 + v2*v2;
    } while(s >= 1 || s == 0);
    X = v1*sqrt(-2*log(s)/s);
  }
  else
    X = v2*sqrt(-2*log(s)/s);

  phase = 1 - phase;
  return X;
}

double randnorm2(unsigned long state[7]) {
    double x, y, r;
    do {
        x = 2.0 * rand01(state) - 1.0;
        y = 2.0 * rand01(state) - 1.0;
        r = x*x + y*y;
    } while (r == 0.0 || r >= 1.0);
    return x * sqrt((-2.0 * log(r)) / r);
}


/* test the KISS algo */
void main() {
  printf("geting state \n");
  unsigned long state[7];
  kiss_srandom(state, 123454);
  printf("starting loop\n");
  for (long index=0; index<10000; index++) {
    double g1 = kiss_random(state);
    printf("kiss_random: %g\n", g1);
  }
  for (long index=0; index<10000; index++) {
    double g2 = rand01(state);
    printf("rand01: %g\n", g2);
  }
  for (long index=0; index<10000; index++) {
    double g3 = randnorm(state);
    printf("randnorm: %g\n", g3);
  }
  printf("ending loop\n");
}
