!!G Toolbox.finc
C  If you have a power mac version of LS fortran uncomment the
C next 5 lines and comment out "!!MP InLines.f"
C!!IFC NOT LSPOWERF
C!!MP 68KInlines
C!!ELSEC
C!!MP PPCInlines
C!!ENDC
C  If you have a 68K mac version of LS fortran comment out the 
C 5 lines above and uncomment the next line.
!!MP InLines.f
C  Fortran callable memory allocator

C  Called as :
C	ier = grgmem (size,pointer)

C  where : size is an integer size of memory to allocate
C	  pointer is an integer to return the pointer into

      Integer Function GRGMEM(size, pointer)
      Integer*4 size, pointer
      
      pointer = NewPtr(Size)
	  If (pointer .eq. 0) Then
	     grgmem = 0
	  Else
	     grgmem = 1
	  End if
      Return
	  End
	  
C  Fortran callable memory deallocator

C  Called as :
C	ier = grfmem (size,pointer)

C  where : size is an integer size of memory to deallocate (not used)
C	  pointer is an integer that contains the pointer


      Integer Function GRFMEM(size, pointer)
      Integer*4 size, pointer

      Call DisposPtr(Pointer)
	  grfmem = 1
	  Return
      End