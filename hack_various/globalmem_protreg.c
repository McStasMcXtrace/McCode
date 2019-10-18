#include <stdio.h>
#include <sys/time.h>
#include <openacc.h>
#include <openacc_curand.h>
#include <accelmath.h>

#define N 1000
#define n 10

int main() {
  float arr[N];
  float hist[n];

  int i, j, k = 0;

  struct timeval tm;
  gettimeofday(&tm, NULL);
  long seed = (long) tm.tv_sec*1000000 + tm.tv_usec;

  #pragma acc data copy(arr[0:N], hist[0:n])
  {

    #pragma acc parallel loop
    for (int i=0; i<N; i++) {
      curandState_t state;
      curand_init(seed, i, 0ULL, &state);
      arr[i] = curand_uniform(&state);
    }

    #pragma acc parallel loop
    for (int j=0; j<n; j++) {
      hist[j] = 0;
    }

    #pragma acc parallel loop
    for (int j=0; j<n; j++) {
      #pragma acc loop seq
      for (int k=N/n*j; k<N/n*(j+1); k++) {
        //#pragma acc atomic capture {
        #pragma acc atomic update {
          hist[j] += arr[k];
        }
      }
    }
  }

  printf("\n");
  for (int i=0; i<n; i++) {
    printf("%f\n", hist[i]);
  }

  printf("\n");
  for (int j=0; j<n; j++) {
    printf("%f\n", arr[N/n*j]);
  }
}
