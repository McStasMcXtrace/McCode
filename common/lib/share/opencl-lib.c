
//////////////////////////////////////////////////////////////////////////////
//OpenCL lilbrary file for McStas
//
// global variables assigned to the OpenCL mechanism
// only initialised once
//Created by Jinyan LIU
//Date july 2014
//////////////////////////////////////////////////////////////////////////////

#ifdef USE_OPENCL
#ifndef OPENCL_LIB_C
#define OPENCL_LIB_C

//////////////////////////////////////////////////////////////////////////////
//! fileparts: return the name of the file between '/' and '.'
//!
//! @return the name of the file if succeeded, 0 otherwise
//! @param name      filename maybe with '/' , '\' or '.'
//! 
//////////////////////////////////////////////////////////////////////////////

#if defined(WIN32) || defined(_WIN32) || defined(_WIN64)
#define PATHSEP_C '\\'
#define PATHSEP_S "\\"
#else  /* !WIN32 */
#define PATHSEP_C '/'
#define PATHSEP_S "/"
#endif /* !WIN32 */

char* fileparts(char *name)
{
  char *Name=NULL;

  if (name && strlen(name)) {
    char *dot_pos    = NULL;
    char *path_pos   = NULL;
    char *end_pos    = NULL;
    char *name_pos   = NULL;
    size_t  name_length= 0;

    end_pos = name+strlen(name);  /* end of file name */

    /* extract path: searches for last file separator */
    path_pos= strrchr(name, PATHSEP_C);  /* last PATHSEP */

    if (!path_pos) {
      path_pos   =name;
      name_pos   =name;
    } else {
      name_pos    = path_pos+1; /* from start to path+sep */
    }

    /* extract ext: now looks for the 'dot' */
    dot_pos = strrchr(name_pos, '.');           /* last dot */
    if (dot_pos <= name_pos)
      dot_pos = end_pos;

    /* extract Name (without extension) */
    name_length = dot_pos - name_pos; /* from path to dot */
    if (name_length) {
      Name = (char*)malloc(name_length);
      if (Name) strncpy(Name, name_pos, name_length);
      Name[name_length]='\0';
    }
  } /* if (name) */
  return (Name);

} /* fileparts */


//////////////////////////////////////////////////////////////////////////////
//! oclLoadProgSource: Loads a Program file and prepends the cPreamble to the code.
//!
//! @return the source string if succeeded, 0 otherwise
//! @param cFilename        program filename
//! @param cPreamble        code that is prepended to the loaded file, typically a set of #defines or a header
//! @param szFinalLength    returned length of the code string
//////////////////////////////////////////////////////////////////////////////

char* oclLoadProgSource(const char* cFilename, const char* cPreamble, size_t* szFinalLength)
{
    // locals 
    FILE* pFileStream = NULL;
    size_t szSourceLength;

    // open the OpenCL source code file
    #ifdef _WIN32   // Windows version
        if(fopen_s(&pFileStream, cFilename, "rb") != 0) 
        {       
            return NULL;
        }
    #else           // Linux version
        pFileStream = fopen(cFilename, "rb");
        if(pFileStream == 0) 
        {       
            return NULL;
        }
    #endif

    size_t szPreambleLength = strlen(cPreamble);

    // get the length of the source code
    fseek(pFileStream, 0, SEEK_END); 
    szSourceLength = ftell(pFileStream);
    fseek(pFileStream, 0, SEEK_SET); 

    // allocate a buffer for the source code string and read it in
    char* cSourceString = (char *)malloc(szSourceLength + szPreambleLength + 1); 
    memcpy(cSourceString, cPreamble, szPreambleLength);
    if (fread((cSourceString) + szPreambleLength, szSourceLength, 1, pFileStream) != 1)
    {
        fclose(pFileStream);
        free(cSourceString);
        return NULL;
    }

    // close the file and return the total length of the combined (preamble + source) string
    fclose(pFileStream);
    if(szFinalLength != 0)
    {
        *szFinalLength = szSourceLength + szPreambleLength;
    }
    cSourceString[szSourceLength + szPreambleLength] = '\0';

    return cSourceString;
} /* end oclLoadProgSource */

//////////////////////////////////////////////////////////////////////////////
//! oclGetPlatformID: Gets the platform ID for NVIDIA if available, otherwise default
//!
//! @return the id 
//! @param clSelectedPlatformID         OpenCL plateform ID
//////////////////////////////////////////////////////////////////////////////

cl_int oclGetPlatformID(cl_platform_id* clSelectedPlatformID)
{
    char chBuffer[1024];
    cl_uint num_platforms,i; 
    cl_platform_id* clPlatformIDs;
    cl_int ciErrNum;
    *clSelectedPlatformID = NULL;

    // Get OpenCL platform count
    ciErrNum = clGetPlatformIDs (0, NULL, &num_platforms);
    if (ciErrNum != CL_SUCCESS)
    {
        printf(" Error %i in clGetPlatformIDs Call !!!\n\n", ciErrNum);
        return -1000;
    }
    else 
    {
        if(num_platforms == 0)
        {
            printf("No OpenCL platform found!\n\n");
            return -2000;
        }
        else 
        {
            // if there's a platform or more, make space for ID's
            if ((clPlatformIDs = (cl_platform_id*)malloc(num_platforms * sizeof(cl_platform_id))) == NULL)
            {
                printf("Failed to allocate memory for cl_platform ID's!\n\n");
                return -3000;
            }

            // get platform info for each platform and trap the NVIDIA platform if found
            ciErrNum = clGetPlatformIDs (num_platforms, clPlatformIDs, NULL);
	    printf("We found %i CL platforms, selecting the first of them:\n",num_platforms);
	    
            ciErrNum = clGetPlatformInfo (clPlatformIDs[0], CL_PLATFORM_NAME, 1024, &chBuffer, NULL);
            if(ciErrNum == CL_SUCCESS)
            {
		printf("Platform: %s\n",chBuffer);
                *clSelectedPlatformID = clPlatformIDs[0];
            }

            free(clPlatformIDs);
        }
    }

    return CL_SUCCESS;
} /* end oclGetPlatformID */

//////////////////////////////////////////////////////////////////////////////
//! oclInit: initialize 'nDevice' GPU's
//!
//! @return error code or CL_SUCCESS
//! @param nDevice         nb of devices requested OpenCL
//
// This function should initialize cxGPUContext and cqCommandQueue
//////////////////////////////////////////////////////////////////////////////


struct opencl_context oclInitKernel(char *filename, cl_uint nDevice) {

  cl_program cpProgram;                           // OpenCL program
  
  cl_platform_id cpPlatform; 

  cl_device_id* cdDevices;                        // OpenCL device list   
  
  cl_int ciErr1, ciErr2;                          // Error code var
  
  char *basename = NULL;
  const int nPerRng = 5860;                       // # of recurrence steps, must be even if do Box-Muller transformation
  const int nRand = MT_RNG_COUNT * nPerRng;       // Output size  

  cl_uint i;
  
  struct opencl_context oclContext;
  oclContext.GPUContext = NULL;
  oclContext.Kernel     = NULL;
  
  ciErr1 = oclGetPlatformID(&cpPlatform);
  if(ciErr1 != CL_SUCCESS) 
    return(oclContext);
  ciErr1 = clGetDeviceIDs(NULL, CL_DEVICE_TYPE_GPU, 0, NULL, &nDevice);

  cdDevices = (cl_device_id *)malloc(nDevice * sizeof(cl_device_id) );
  ciErr1 =clGetDeviceIDs(NULL, CL_DEVICE_TYPE_GPU,  nDevice, cdDevices, NULL);
  
  
  oclContext.GPUContext = clCreateContext(0, nDevice, cdDevices, NULL, NULL, &ciErr1);
  if(ciErr1 != CL_SUCCESS) 
    return(oclContext);
        
  for (i = 0; i < nDevice; i++) 
  {
      oclContext.CommandQueue[i] = clCreateCommandQueue(oclContext.GPUContext, cdDevices[i], 0, &ciErr1);
  }
  
  if (!oclContext.GPUContext) return (oclContext);

  /* load mersenne twister kernel source from disk */
  size_t szKernelLength; // Byte size of kernel code
  char   *cKernel = oclLoadProgSource(filename, "// My comment\n", &szKernelLength);
  
  if(cKernel == NULL) {
    printf("%s:%u: OpenCL error: Failed to open the OpenCL program source file %s\n", __FILE__, __LINE__, filename);
    return(oclContext);
  }
  
  cpProgram = clCreateProgramWithSource(oclContext.GPUContext, 1, (const char **)&cKernel, &szKernelLength, &ciErr1);
  if(ciErr1 != CL_SUCCESS) 
    return(oclContext);  
       
  if (cpProgram== NULL) {
    printf("%s:%u: OpenCL error: Failed to create %s OpenCL program\n", __FILE__, __LINE__, filename);
    return(oclContext);
  }  
       
  ciErr1 |= clBuildProgram(cpProgram, 0, NULL, "-cl-fast-relaxed-math", NULL, NULL);
  if (ciErr1 !=CL_SUCCESS){
    printf("%s:%u: OpenCL error: Failed to build %s kernel\n", __FILE__, __LINE__, filename);
    for (i=0; i < nDevice; i++) {
      // Determine the size of the log
      size_t len;
      clGetProgramBuildInfo(cpProgram, cdDevices[i], CL_PROGRAM_BUILD_LOG, 0, NULL, &len); 
      // Allocate memory for the log
      char *build_log = (char*)malloc(len);
      // Get the log
      ciErr1 |= clGetProgramBuildInfo(cpProgram, cdDevices[i], CL_PROGRAM_BUILD_LOG, len, build_log, NULL);
      // Print the log
      printf("-------Build log-----------\n");
      printf("\nBuildInfo:%s\n", build_log);
      free(build_log);
      clFinish(oclContext.CommandQueue[i]);
    }
    return(oclContext);
  }
  
  basename = fileparts(filename);
  
  oclContext.Kernel = clCreateKernel(cpProgram, basename, &ciErr1);
  
  free(basename);

  if (oclContext.Kernel == NULL || ciErr1 !=CL_SUCCESS) {
    printf("%s:%u: OpenCL error: Failed to create %s kernel\n", __FILE__, __LINE__, filename);
    return(oclContext);
  }

  for (i = 0; i < nDevice; i++)
    {
      clFinish(oclContext.CommandQueue[i]);
    }
    
  clReleaseProgram(cpProgram); 
  free(cKernel);
  free(cdDevices);
  
  return oclContext;
}

/* ========================================================================== */
/*        OpenCL implementation of random MersenneTwister generator           */
/* ========================================================================== */



unsigned int **mt_random_opencl_fill_buffer(cl_uint nDevice, struct opencl_context oclContext)
{

  cl_int ciErr1=0, ciErr2=0;                      // Error code var
  size_t globalWorkSize[1] = {MT_RNG_COUNT};      // 1D var for Total # of work items
  size_t localWorkSize[1] = {128};                // 1D var for # of work items in the work group

  const int nPerRng =5860;                        // # of recurrence steps, must be even if do Box-Muller transformation
  const int nRand = MT_RNG_COUNT * nPerRng;       // Output size   
  char      allocate_memory=0;
  cl_uint   iDevice,i;

  printf("Filling buffer\n");

  /* this section launches the kernel to fill the buffer (when counter = -1) */
  
  //  Using GPU(s)...
  if (oclContext.Kernel == NULL || oclContext.GPUContext==NULL)
    return(NULL);

  //  Initialization: load MT parameters and init host buffers
  if (!oclContext_mt_buffer) {
    // first call to this function: allocate memory
    allocate_memory = 1;
    oclContext_mt_buffer = (unsigned int**)malloc(nDevice*sizeof(unsigned int*));
    if (!oclContext_mt_buffer) return NULL;
    for (iDevice = 0; iDevice < nDevice; oclContext_mt_buffer[iDevice++]=NULL);
  }

  //  Allocate memory
  if (allocate_memory)
    for (iDevice = 0; iDevice < nDevice; iDevice++)
    {
       if (!oclContext_mt_buffer[iDevice])
           oclContext_mt_buffer[iDevice]  = (unsigned int*)malloc(sizeof(unsigned int)*nRand); // Host buffers for GPU output
       if (!oclContext_mt_buffer[iDevice]) return(NULL);
    }

  if (allocate_memory && !oclContext_mt_buffer_GPU) {
    oclContext_mt_buffer_GPU = (cl_mem*)malloc(nDevice*sizeof(cl_mem));
    if (!oclContext_mt_buffer_GPU) return NULL;
    for (iDevice = 0; iDevice < nDevice; oclContext_mt_buffer_GPU[iDevice++]=NULL);
  }
  
  if (allocate_memory)
    for (iDevice = 0; iDevice < nDevice; iDevice++)
    {
        if (!oclContext_mt_buffer_GPU[iDevice]) {
          oclContext_mt_buffer_GPU[iDevice] = clCreateBuffer(oclContext.GPUContext, CL_MEM_READ_WRITE, sizeof(cl_uint)*nRand, NULL, &ciErr2);
        
          if (ciErr2 !=CL_SUCCESS) return(NULL);
        }
        
    } /* for */
  
  for (iDevice = 0; iDevice < nDevice; iDevice++)
  {
      clFinish(oclContext.CommandQueue[iDevice]);
  }
  
  for (iDevice = 0; iDevice < nDevice; iDevice++)
  {
    time_t t;
    int seed=(int)time(&t);
    if (allocate_memory) {
      ciErr1 |= clSetKernelArg(oclContext.Kernel, 0, sizeof(cl_mem), (void*)&oclContext_mt_buffer_GPU[iDevice]);
      if (ciErr1 !=CL_SUCCESS) return(NULL);
        
      ciErr1 |= clSetKernelArg(oclContext.Kernel, 1, sizeof(int),    (void*)&nPerRng);
      if (ciErr1 !=CL_SUCCESS) return(NULL);
    }

    // Each call to fill buffer must use a different seed
    ciErr1 |= clSetKernelArg(oclContext.Kernel, 2, sizeof(int),    (void*)&seed);
      if (ciErr1 !=CL_SUCCESS) return(NULL);
      
    // call the Kernel with the queue
    ciErr1 |= clEnqueueNDRangeKernel(oclContext.CommandQueue[iDevice], oclContext.Kernel, 1, NULL, globalWorkSize, localWorkSize, 0, NULL, NULL);

    if (ciErr1 !=CL_SUCCESS) return (NULL);
  }
    
  for (iDevice = 0; iDevice < nDevice; iDevice++)
  {
      clFinish(oclContext.CommandQueue[iDevice]);
  }

  //  Read back results
  
  for (iDevice = 0; iDevice < nDevice; iDevice++)
  {
      ciErr1 = clEnqueueReadBuffer(oclContext.CommandQueue[iDevice], oclContext_mt_buffer_GPU[iDevice], CL_TRUE, 0, 
          sizeof(cl_uint) * nRand, oclContext_mt_buffer[iDevice], 0, NULL, NULL);
  
      if (ciErr1 !=CL_SUCCESS) return(NULL);
  }  

  return (unsigned int**) oclContext_mt_buffer;
} /* end mt_random_opencl_fill_buffer */



// main Mersenne random call ---------------------------------------------------
void mt_srandom_opencl(unsigned long s) {
  fprintf(stderr, "%s: The GPU implementation does not support manual seed setting. Using seed=clock().\n", __FILE__);
}

unsigned int mt_random_opencl(void) // Should be called by others 
{
  unsigned int nDevice = 1;                              // Use 1st opencl Device, > 0
  const int    nPerRng = 5860;                           // # of recurrence steps, must be even if do Box-Muller transformation
  const int    nRand = MT_RNG_COUNT * nPerRng;           // Output size
 
  if (oclContext_mt_counter >= nRand) {    
    printf("%s:%i: INFO: reached max (oclContext_mt_counter=%i)\n", __FILE__, __LINE__, oclContext_mt_counter); fflush(NULL);
    oclContext_mt_counter = -1;
  }  
  
  if (oclContext_mt_counter <= -2) {
    /* the first time we create the OCL kernel and the buffer */
    oclContext_mt = oclInitKernel("MersenneTwister.cl", nDevice);
    oclContext_mt_counter = -1;
  }
  
  // in case the OpenCL context has noot been initialized, default to serial MT
  if (oclContext_mt.Kernel == NULL) {
    return(mt_random());
  }
    
  if (oclContext_mt_counter <= -1 /*|| oclContext_mt_counter >= nRand*/ ) {
    /* when the buffer is just created or fully read, we re-fill the buffer */
    if (mt_random_opencl_fill_buffer(nDevice, oclContext_mt) == NULL) {
      fprintf(stderr, "%s: Could not find the OpenCL MT buffer. Using serial MT.\n", __FILE__);
      return(mt_random());
    }

    oclContext_mt_counter = 0;
  } 
  
  /* return the value of the random number read from the buffer */
  printf("Rng returned %u as %u\n",oclContext_mt_counter++,oclContext_mt_buffer[nDevice-1][oclContext_mt_counter++]);
  return (oclContext_mt_buffer[nDevice-1][oclContext_mt_counter++]);
} /* mt_random_opencl */

#endif
#endif

