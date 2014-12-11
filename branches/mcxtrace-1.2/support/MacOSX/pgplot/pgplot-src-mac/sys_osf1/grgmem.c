#include <stdio.h>
#include <stdlib.h>
/*
  Fortran callable memory allocator

  Called as :
	ier = grgmem (size,pointer)

  where : size is an integer size of memory to allocate
	  pointer is an integer to return the pointer into

*/

#ifdef PG_PPU
#define GRGMEM grgmem_
#define GRFMEM grfmem_
#else
#define GRGMEM grgmem
#define GRFMEM grfmem
#endif

int GRGMEM(size, pointer)
int  *size;
#if 0
int *pointer;
#else
char **pointer;
#endif
{
#if 0
  char *area = malloc(*size);
  *pointer = (int)area;
  if (area == NULL) return 0;
  return 1;
#else
  if (!(*pointer=malloc(*size)))
      return 0;
  else
      return 1;
#endif
}

/*
  Fortran callable memory deallocator

  Called as :
	ier = grfmem (size,pointer)

  where : size is an integer size of memory to deallocate (not used)
	  pointer is an integer that contains the pointer

*/

int GRFMEM(size, pointer)
int *size;
#if 0
int *pointer;
#else
char **pointer;
#endif
{
#if 0
  char *area = (char *)*pointer;
  free(area);
  return 1;
#else
  free(*pointer);
  return 1;
#endif
}


