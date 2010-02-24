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
C*GRUSER -- get user name (Macintosh)
C+
      SUBROUTINE GRUSER(STRING, L)
      CHARACTER*(*) STRING
      INTEGER L
C
C Return the name of the user running the program.
C On Macintosh get user name in from resource -16096.  This is set
C with the Sharing Setup control panel.
C
C Arguments:
C  STRING : receives user name, truncated or extended with
C           blanks as necessary.
C  L      : receives the number of characters in VALUE, excluding
C           trailing blanks.
C--
C-----------------------------------------------------------------------
	  Record /StringHandle/ UserName
	  
      UserName = GetString(int2(-16096))
      STRING = UserName.h^.P^
      L = len(trim(string))
      END
