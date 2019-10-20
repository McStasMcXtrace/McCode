#include <math.h>
#include <stdint.h>
#include <stdio.h>

//////////////////////////////////////////////////////////////////////////////
// Thomas Mueller hash for initialization of rngs
// http://stackoverflow.com/questions/664014/
//        what-integer-hash-function-are-good-that-accepts-an-integer-hash-key
//////////////////////////////////////////////////////////////////////////////

#define hash(x) (nvidia_hash(x))
#define state_t uint32_t

state_t mueller_hash(state_t x) {
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = ((x >> 16) ^ x);
    return x;
}


state_t nvidia_hash(state_t x) {

    x = (x + 0x7ed55d16) + (x << 12);
    x = (x ^ 0xc761c23c) ^ (x >> 19);
    x = (x + 0x165667b1) + (x <<  5);
    x = (x + 0xd3a2646c) ^ (x <<  9);
    x = (x + 0xfd7046c5) + (x <<  3);
    x = (x ^ 0xb55a4f09) ^ (x >> 16);

    return x;
}

//////////////////////////////////////////////////////////////////////////////
// linear congruential generator
//////////////////////////////////////////////////////////////////////////////

// lcg state is a pointer to single long

state_t lcg(state_t *state) {
    state[0] = 314527869*state[0]+1234567;
    return state[0];
}

state_t *lcg_seed(state_t *state, state_t seed) {

    state_t num_iters=8;

    state[0] = !seed ? 4294967295 : seed;
    for (state_t iter = 0; iter < num_iters; iter++)
        state[0] = hash(state[0]);

    return state;
}

//////////////////////////////////////////////////////////////////////////////
// keep it simple stupid generator
//////////////////////////////////////////////////////////////////////////////

// kiss state is a pointer to 4 long [x,y,z,w]

state_t kiss(state_t *state) {

    // lcg
    state[0] = 314527869*state[0]+1234567;

    // xorshift
    state[1] ^= state[1] << 5;
    state[1] ^= state[1] >> 7;
    state[1] ^= state[1] << 22;

    // carry and multiply
    uint64_t t = 4294584393ULL*state[2]+state[3];
    state[3] = t;
    state[3] = t >> 32;

    // combine
    return state[0]+state[1]+state[2];
}

state_t *kiss_seed(state_t *state, state_t seed) {

    state_t num_iters=8;

    state[3] = !seed ? 4294967295 : seed;
    for (state_t iter = 0; iter < num_iters; iter++) {
        state[0] = hash(state[3]);
        state[1] = hash(state[0]);
        state[2] = hash(state[1]);
        state[3] = hash(state[2]);
    }

    return state;
}

//////////////////////////////////////////////////////////////////////////////
// fast keep it simple stupid generator
//////////////////////////////////////////////////////////////////////////////

// fast kiss state is a pointer to 5 long [x,y,z,w,c]

state_t fast_kiss(state_t * state) {

    state[1] ^= state[1] << 5;
    state[1] ^= state[1] >> 7;
    state[1] ^= state[1] << 22;

    state_t t = state[2]+state[3]+state[4];
    state[2]  = state[3];
    state[4]  = t < 0;
    state[3]  = t & 2147483647;
    state[0] += 1411392427;

    // combine
    return state[0]+state[1]+state[3];
}

state_t * fast_kiss_seed(state_t * state, state_t seed) {

    kiss_seed(state, seed);
    state[4] = 0;

    return state;
}

//////////////////////////////////////////////////////////////////////////////
// xoshiro256+ random number generator (c) 2018 Blackman - Vigna (vigna@acm.org)
//////////////////////////////////////////////////////////////////////////////

// xoshiro256 state is a pointer to 4 long [x,y,z,w]
// WARNING: only works with state_t == uint64_t

#define xoshiro256_rotl(x,k) ((x) << (k)) | ((x) >> (64 - (k)));

state_t xoshiro256(state_t *s) {
	const uint64_t result = s[0] + s[3];

	const uint64_t t = s[1] << 17;

	s[2] ^= s[0];
	s[3] ^= s[1];
	s[1] ^= s[2];
	s[0] ^= s[3];

	s[2] ^= t;

	s[3] = xoshiro256_rotl(s[3], 45);

	return result;
}

state_t * xoshiro256_seed(state_t *s, state_t seed) {
  return kiss_seed(s, seed);
}

//////////////////////////////////////////////////////////////////////////////
// meta function for uniform sampling in [0, 1)
//////////////////////////////////////////////////////////////////////////////

double uniform_double(state_t rng(state_t*), state_t * state) {

    state_t a = rng(state) >> 6;
    state_t b = rng(state) >> 5;
    double x = (a * 134217728.0 + b) / 9007199254740992.0;

    return x;
}

//////////////////////////////////////////////////////////////////////////////
// meta functions for normal sampling
//////////////////////////////////////////////////////////////////////////////
 
double gaussian_double(state_t rng(state_t*), state_t * state) {

    double x, y, r;

    do {
        x = 2.0 * uniform_double(rng, state) - 1.0;
        y = 2.0 * uniform_double(rng, state) - 1.0;
        r = x*x + y*y;
    } while (r == 0.0 || r >= 1.0);

    return x * sqrt((-2.0 * log(r)) / r);
}

// -----------------------------------------------------------------------------

// available RNG: lcg, kiss, fast_kiss, xoshiro256
#define random  fast_kiss
#define srandom fast_kiss_seed
#define rand01(s)   uniform_double(random, s)
#define randnorm(s) gaussian_double(random, s)

void main() {
  state_t state[7];       // the RNG state vector
  srandom(state, 123454); // set the seed
  
  // use RNG to cast a few numbers
  for (long index=0; index<1000; index++) {
    double g = rand01(state);
    printf("random: %g\n", g);
  }
    
}
