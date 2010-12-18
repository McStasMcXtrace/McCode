// -*- C++ -*-

#ifndef H_McStas_compact_RANDOM
#define H_McStas_compact_RANDOM


#ifndef MC_RAND_ALG
#define MC_RAND_ALG 1
#endif



namespace McStas {

  typedef int mc_int32_t;
  mc_int32_t mc_random(void);
  void mc_srandom (unsigned int x);
  unsigned long mt_random(void);
  void mt_srandom (unsigned long x);


#if MC_RAND_ALG == 0
   /* Use system random() (not recommended). */
#  define MC_RAND_MAX RAND_MAX
#elif MC_RAND_ALG == 1
   /* "Mersenne Twister", by Makoto Matsumoto and Takuji Nishimura. */
#  define MC_RAND_MAX ((unsigned long)0xffffffff)
#  define random mt_random
#  define srandom mt_srandom
#elif MC_RAND_ALG == 2
   /* Algorithm used in McStas 1.1 and earlier (not recommended). */
#  define MC_RAND_MAX 0x7fffffff
#  define random mc_random
#  define srandom mc_srandom
#else
#  error "Bad value for random number generator choice."
#endif


  inline double rand01() { return ((double)random())/((double)MC_RAND_MAX+1) ;}
  inline double randpm1() { return ((double)random()) / (((double)MC_RAND_MAX+1)/2) - 1 ;}
  inline double rand0max(double max) { return ((double)random()) / (((double)MC_RAND_MAX+1)/(max)) ;}
  inline double randminmax(double min, double max) { return rand0max((max)-(min)) + (min) ;}
} //McStas


#endif // H_McStas_compact_RANDOM
