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


C*GRGENV -- get value of PGPLOT environment parameter (MAC)
C+
      SUBROUTINE GRGENV(NAME, VALUE, L)
      CHARACTER*(*) NAME, VALUE
      INTEGER L
C
C Return the value of a PGPLOT environment parameter. In Sun/Convex-UNIX,
C environment parameters are UNIX environment variables; e.g. parameter
C ENVOPT is environment variable PGPLOT_ENVOPT. Translation is not
C recursive and is case-sensitive.
C
C Arguments:
C  NAME   : (input) the name of the parameter to evaluate.
C  VALUE  : receives the value of the parameter, truncated or extended
C           with blanks as necessary. If the parameter is undefined,
C           a blank string is returned.
C  L      : receives the number of characters in VALUE, excluding
C           trailing blanks. If the parameter is undefined, zero is
C           returned.
C
C On Macintosh, the environment variables are stored in file.  This subroutine
C first looks for the file PGPLOTENVNAMES, in the application directory.
C If it can't be found, a standard file dialog box will be displayed,
C so that you can find the file.  Once it is found, the name and location are 
C stored so that you will not be prompted again.
C--
C 19-Jan-1988
C 25-Sep-1995  Modified to work on mac with MPW Fortran 2.1.  All environment 
C              parameters are stored in the file.  The file can have any name 
C              but best thing to do is to put a file called pgplotenvnames
C              in the application directory. See Tech. Note 35 for more information
C              about Macintosh file system.
C 17-Jan-1996  Modified by Mike Burnett (mnb@ornl.gov) to search for the 
C              pgplotenvnames file in the preferences folder before putting
C              up dialog box.
C-----------------------------------------------------------------------       
      INTEGER LIN, LUN,LStart,VolRefNum, JVRefNum, myresult
      CHARACTER*32 TEST, Line*120, FilNam*120
      INTEGER*4 myDirID
      External JVRefNum
      Save FileName,VolRefNum
          

      include 'Folders.f'
          
C
      TEST = 'PGPLOT_'//NAME
      LIN = INDEX(TEST, ' ')-1
      Value = ' '
      L = 0
      Call GrgLun(LUN)

C  If volume reference number has been set, switch to that volume.  The
C  first time grgenv is called, volrefnum will not be set and the currect
C  directory is the application directory.  The volume reference number will
C  be set after pgplotenvnames is found.
      If (VolRefNum .lt. 0) Then
         Call  F_SETVOLUME(VolRefNum)
      End If

C  Try to open FilNam.  The first time that Grgenv is called Filnam will
C  be empty and the open will fail.  So try to open pgplotenvnames in the
C  current directory.  If that fails put up a standard file dialog box to
C  find pgplotenvnames.  If FilNam has been set then after assigning a 
C  unit number to the file reset the volume reference number to the application
C  directory.  
      Open(Unit = lun,File=FilNam,Status='OLD',Err = 10,Readonly)
      Call F_SETVOLUME(JVREFNUM(-1))
      Go to 1
          
10    Open(Unit = lun,File='pgplotenvnames',Status='OLD',Err = 15,Readonly)
      FilNam = 'pgplotenvnames'
      VolRefNum = JVREFNUM(Lun)
      Go to 1
      
15    myresult = FindFolder(kOnSystemDisk,kPreferencesFolderType,
     &           kDontCreateFolder,%REF (myVRefNum),%REF (myDirID))
      if (myresult.ne.0) go to 20
      myresult = HSetVol(NIL,myVRefNum,myDirID)
      if (myresult.ne.0) go to 20
      Open(Unit=lun,File='pgplotenvnames',Status='OLD',Err=20,Readonly)
      FilNam = 'pgplotenvnames'
      VolRefNum = JVREFNUM(Lun)
      Call F_SETVOLUME(JVREFNUM(-1))
      Go to 1

C Put up standard file dialog box.  Once found store the file name and volume
C reference number.
20    Call F_SETVOLUME(JVREFNUM(-1))  
      CALL GRWARN('Could not find file PGPLOTENVNAMES in current directory.')
      CALL GRWARN('A dialog box will come up allowing you to find the file with the')
      CALL GRWARN('environment variables. Hit return for the dialog box to appear.')
      Pause
      Open(Unit=lun,File=*,STATUS='OLD',err=100,Readonly)
      Inquire(Unit=LUN,Name=FilNam)
      VolRefNum = JVREFNUM(Lun)

C File has been found, so search for environmental variable and extract value.
1     Continue
             Read(Lun,'(A512)',End=2) Line 
             If (Test(:Lin) .EQ. Line(:Lin)) Then
                Lstart = index(Line,"'")+1
                L = index(Line(Lstart:),"'")-1
                Value = Line(LStart:LStart+L-1)
                        Close(Lun)
                Go to 2
             End If
          Go to 1
2     Close(LUN)
      Return
             

C     Could not find PGPLOTENVNAMES.
100   Close(LUN)
      CALL GRWARN('Cancelled dialog box to find PGPLOTENVNAMES')
      Return
      END
