int main(int argc, char* argv[]){
  size_t n = 100;
  curandGenerator_t gen;
  float *devData, *hostData;
  hostData = (float*) calloc(n, sizeof(float));
  cudaMalloc((void**) &devData, n * sizeof(float));
  /* Create pseudo-random number generator */
  curandCreateGenerator(&gen, CURAND_RNG_PSEUDO_DEFAULT);
  curandSetPseudoRandomGeneratorSeed(gen, 1234ULL); /* Set seed */

  curandGenerateUniform(gen, devData, n);
  cudaMemcpy(hostData, devData, n * sizeof(float), cudaMemcpyDeviceToHost));
  for(int i = 0; i < n; i++) { printf("%1.4f ", hostData[i]); }
  printf("\n");

  curandDestroyGenerator(gen); /* Cleanup */
  cudaFree(devData);
  free(hostData);
  return EXIT_SUCCESS;
}
