#include <stdio.h>
#include <stdlib.h>
/*
  Fortran callable memory allocator (Cray version).

  Called as :
	ier = grgmem (size,pointer)

  where : size is an integer size of memory to allocate
	  pointer is an integer to return the pointer into

*/

int GRGMEM(size, pointer)
int *size, *pointer;
{
  char *area = malloc(*size);
  *pointer = (int)area;
  if (area == NULL) return 0;
  return 1;
}

/*
  Fortran callable memory deallocator

  Called as :
	ier = grfmem (size,pointer)

  where : size is an integer size of memory to deallocate (not used)
	  pointer is an integer that contains the pointer

*/

int GRFMEM(size, pointer)
int *size, *pointer;
{
  char *area = (char *)*pointer;
  free(area);
  return 1;
}
