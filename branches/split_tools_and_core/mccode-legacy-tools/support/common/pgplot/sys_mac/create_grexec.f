c        1         2         3         4         5         6         7
c2345678901234567890123456789012345678901234567890123456789012345678901234567890
      Program crgrexc
      Implicit None
      Integer I,Ndev,options(99),start
      Character Line*80,driver(99)*6
      
      Open(Unit=10,File='drivers.list',Status='OLD',Err=1000)
      Open(Unit=11,File='grexec.f',Status='Unknown',Err=3000)
      
      Ndev = 0
10    Read(10,'(A80)',End=2000)Line
      If (Line(1:1) .eq. '!') go to 10
         Ndev = Ndev + 1
         Driver(Ndev) = line(3:8)
         Read(line(10:10),'(I1)') options(Ndev)
         Go to 10
      Continue
2000  If (Ndev .eq. 0) Then
         Write(*,'('' Error:  No drivers were selected.  Select drivers '')')
         Write(*,'('' by removing ! at beginning of line.'')')
	     Call F_EndMPW(1)
	  Else If (Ndev .le. 99) Then 
         Write(11,'(''C*GREXEC -- PGPLOT device handler dispatch routine'')')
         Write(11,'(''C+'')')
         Write(11,'(
     +    ''      SUBROUTINE GREXEC(IDEV,IFUNC,RBUF,NBUF,CHR,LCHR)'')')
         Write(11,'(''      INTEGER IDEV, IFUNC, NBUF, LCHR'')')
         Write(11,'(''      REAL    RBUF(*)'')')
         Write(11,'(''      CHARACTER*(*) CHR'')')
         Write(11,'(''C---'')')
         Write(11,'(''      INTEGER NDEV'')')
         Write(11,'(''      PARAMETER (NDEV='',I3,'')'') ') Ndev
         Write(11,'(''      CHARACTER*10 MSG'')')
         Write(11,'(''C---'')')
         Line(1:14)='(''      GOTO('
         I = 0
         Start = 11
20          I = I + 1
            If (I .lt. Ndev) Then
               If (Start .lt. 60) Then
                  Start = Start + 3
               Else
                  line(start+3:start+5) = ''')'
                  Write(11,line(1:Start+5))
                  Line(1:14)='(''     +     '
                  Start = 14
               End If
               Write(Line(start:start+2),'(i2,'','')') I
               Go to 20
            Else
               Start = Start + 3
               Write(Line(start:start+7),'(i2,'') Idev'')') I
               Line(start+8:start+11) = ''')'
               Write(11,line(1:start+11))
            End if
c        1         2         3         4         5         6         7
c2345678901234567890123456789012345678901234567890123456789012345678901234567890
      Else
         Write(*,*) 'Error: Ndev > 99.  Increase Ndev and check program.'
         Call F_EndMPW(1)
      End If
      Write(11,'(''      IF (IDEV.EQ.0) THEN'')')
      Write(11,'(''          RBUF(1) = NDEV'')')
      Write(11,'(''          NBUF = 1'')')
      Write(11,'(''      ELSE'')')
      Write(11,*)'         WRITE (MSG,''(I10)'') IDEV'
      Write(11,*)'         CALL GRWARN(''Unknown device',
     +           ' code in GREXEC: ''//MSG)'
      Write(11,'(''      END IF'')')
      Write(11,'(''      RETURN'')')
      Write(11,'(''C---'')')
	  Do 30 I = 1, Ndev
	     If (Options(i) .eq. 0) Then 
	        Write(11,'(I2,''    CALL '',A6,
     +           ''(IFUNC,RBUF,NBUF,CHR,LCHR)'')')I,Driver(I)
	     Else
	        Write(11,'(I2,''    CALL '',A6,
     +           ''(IFUNC,RBUF,NBUF,CHR,LCHR,'',I2,'')'')')
     +           I,Driver(I),Options(i)
		 End If
		 Write(11,'(''      RETURN'')')
30    Continue
      Write(11,'(''C'')')
      Write(11,'(''      END'')')
      Write(*,*) 'Finished creating grexec.f'
	  Call F_EndMPW(0)

1000  Continue
      Write(*,*)' Error: Could not open drivers.list in current directory.'
      Write(*,*)' Copy drivers.list from drivers directory to here.'
      Call F_EndMPW(1)
            
3000  Continue
      Write(*,*) ' Error: Could not open grexec.f.'
      Call F_EndMPW(1)
      End