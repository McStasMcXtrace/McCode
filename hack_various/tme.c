#include <stdio.h>
#include <sys/time.h>

#define N 1000
#define n 10

void somefunc(){}

int main() {
  float arr[N];
  float hist[n];

  int i, j, k = 0;

  struct timeval tm;
  gettimeofday(&tm, NULL);

  printf("%ld\n", (long) tm.tv_sec*1000000+tm.tv_usec);
}
