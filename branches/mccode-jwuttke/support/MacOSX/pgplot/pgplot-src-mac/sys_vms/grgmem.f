C Fortran callable memory allocator (OpenVMS)
C
C Called as :
C	ier = grgmem (size,pointer)
C
C where : size is an integer size of memory to allocate
C	  pointer is an integer to return the pointer into

      INTEGER FUNCTION GRGMEM(SIZE, POINTER)
      INTEGER SIZE, POINTER
      INTEGER LIB$GET_VM
      GRGMEM = LIB$GET_VM(SIZE, POINTER)
      END

C Fortran callable memory deallocator
C
C Called as :
C	ier = grfmem (size,pointer)
C
C where : size is an integer size of memory to deallocate (not used)
C	  pointer is an integer that contains the pointer

      INTEGER FUNCTION GRFMEM(SIZE, POINTER)
      INTEGER SIZE, POINTER
      INTEGER LIB$FREE_VM
      GRFMEM = LIB$FREE_VM(SIZE, POINTER)
      END
