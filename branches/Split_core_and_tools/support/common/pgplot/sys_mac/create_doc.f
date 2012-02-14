      Program create_doc
      Implicit None
      Integer I,J,K,numsub,Unitnm(0:200),Sizehd(0:200),End(0:200),start,loc,M,
     +        locarr(50),starto,headar(50),ndum,match
      Character Line*100,header(100)*200,Asizeh(0:200)*3,lineout*200
	 
C Declarations needed to get arguments from MPW command line.
C Argument 0 is always create_doc and is ignored.  Arguments 2 and up are the
C actual files that will be searched to build the documentation. Argument
C 1 is the directory containing the files.
       
      include 'Types.f'
	
      STRUCTURE /Args/
		record /StringPtr/ arg(0:7)
      END STRUCTURE

      integer*4	ArgC
      pointer		/Args/ ArgV
	
      integer*4	JARGC			! ArgC = JArgC()
      EXTERNAL	JARGC			!  # of arguments+1
      integer*4	JARGV			! ArgV = JArgV()
      EXTERNAL 	JARGV			!  pointer to an array of string pointers
      record		/StringPtr/ P
      integer*4	p

      Data UnitNm/201*0/
      Open(Unit=1,File='pgplot.doc',Status='Unknown',Err=2000,
     +     carriagecontrol='Fortran')
      Open(Unit=2,File='pgplot.html',Status='Unknown',Err=3000,
     +     carriagecontrol='Fortran')

c        1         2         3         4         5         6         7
c2345678901234567890123456789012345678901234567890123456789012345678901234567890
      Write(1,'(''PGPLOT GRAPHICS SUBROUTINE LIBRARY Version 5.0'',//,
     +''PGPLOT is a Fortran subroutine package for drawing graphs on a variety'',/,
     +"of display devices. For more details, see the manual ``PGPLOT Graphics",/,
     +"Subroutine Library'''' available from T. J. Pearson",/,
     +''(tjp@astro.caltech.edu).'',//,
     +''INDEX OF ROUTINES'',/)')
	  
	  Write(2,'("<HTML>",/,
     +"<HEAD><TITLE>PGPLOT Subroutine Descriptions</TITLE></HEAD>",/,
     +"<BODY>",//,
     +"<H1>PGPLOT Subroutine Descriptions</H1>",//,
     +"<H2>Introduction</H2>",//,
     +"This appendix includes a list of all the PGPLOT subroutines,",/,
     +"and then gives detailed instructions for the use of each routine in",/,
     +"Fortran programs. The subroutine descriptions are in alphabetical order.",//,
     +"<H2>Arguments</H2>",//,
     +"The subroutine descriptions indicate the data type of each",/,
     +"argument. When arguments are described as ``input'''', they may be",/,
     +"replaced with constants or expressions in the <CODE>CALL</CODE>",/,
     +"statement, but make sure that the constant or expression has the",/,
     +"correct data type.",//,
     +"<DL><DT><CODE>INTEGER</CODE> arguments:",/,
     +"<DD>these should be declared",/,
     +"<CODE>INTEGER</CODE> or <CODE>INTEGER*4</CODE> in the calling program,",/,
     +"not <CODE>INTEGER*2</CODE>.",/)')
      Write(2,'("<DT><CODE>REAL</CODE> arguments:",/,
     +"<DD>these should be declared",/,
     +"<CODE>REAL</CODE> or <CODE>REAL*4</CODE> in the calling program, not",/,
     +"<CODE>REAL*8</CODE> or <CODE>DOUBLE PRECISION</CODE>.",//,
     +"<DT><CODE>LOGICAL</CODE> arguments:",/,
     +"<DD>these should be declared",/,
     +"<CODE>LOGICAL</CODE> or <CODE>LOGICAL*4</CODE> in the calling program.",//,
     +"<DT><CODE>CHARACTER</CODE> arguments:",/,
     +"<DD> any valid Fortran",/,
     +"<CODE>CHARACTER</CODE> variable may be used (declared",/,
     +"<CODE>CHARACTER*n</CODE> for some integer <CODE>n</CODE>).",//,
     +"</DL>",//,
     +"<H2>Index of Routines</H2>",//,
     +"<EM>Version 5.0</EM><P>",///,
     +"<UL>")')

C  Get the number of arguments and actual arguments

      ArgC = JARGC()
      ArgV = JARGV()
	
C	Test each file to see if it is used in pgplot.doc.  Files which
C   will be used to create the documentation have 'C*PG'.  Store the argument
C   number in Unitnm(numsub) and increment numsub.  This first do loop creates the
C   index.
      numsub = 0
      do i=2,ArgC-1
	     Open(10,file=ArgV^.arg(1).P^//ArgV^.arg(i).P^,Status='OLD')
5           Read(10,'(A100)',End=10) line
			If (line(1:4) .eq. 'C*PG') Then
			   numsub = numsub + 1
			   Unitnm(numsub) = i
C  Trim is a Language Systems function which removes the trailing spaces from
C  the argument and returns a String*255 data type.
			   sizehd(numsub) = len(trim(line(3:)))
			   header(numsub)(1:sizehd(numsub)) = line(3:sizehd(numsub)+2)
			   Write(Asizeh(numsub),'(I3)') sizehd(numsub)
 			   Write(1,'(T1,A'//Asizeh(numsub)//')') 
     +               header(numsub)(1:sizehd(numsub))
C The variable End(numsub) stores the location for the end of the subroutine
C name, while sizehd(numsub) stores the length of the header line.
               End(numsub) = index(header(numsub),' ') - 1
			   line = '<LI><A HREF=""#'//header(numsub)(1:end(numsub))//
     +                '"">'//header(numsub)(1:end(numsub))//'</A>'//
     +              header(numsub)(end(numsub)+1:sizehd(numsub))
			   Sizehd(0) = len(Trim(line))
			   Write(Asizeh(0),'(I3)') sizehd(0)
 	           Write(2,'(T1,A'//Asizeh(0)//')')line(1:sizehd(0))
			End If
			Go to 5
10       Continue
		 Close(10)
      enddo
	  Write(2,'(T1,''</UL>'')')

C  This second do loop creates the actual documentation.  First write the
C  header, then look for C+ and copy all the lines between C+ and C- to
C  pgplot.doc and pgplot.html.
	  Do 30 J = 1,numsub
	     Write(1,'(//,''----------------------------------------------'',
     +   ''--------------------------'')')
	     Write(1,'(T1,A8,A'//Asizeh(j)//')')
     +         'Module: ',header(J)(1:sizehd(j))
	     Write(1,'(''----------------------------------------------'',
     +   ''--------------------------'',/)')
	     line = '<H2><A NAME=""'//header(J)(1:end(J))//
     +          '"">'//header(J)(1:end(J))//'</A>'//
     +          header(J)(end(J)+1:sizehd(J))//'</H2>'
         Write(2,'(/,T1,''<HR>'')')
		 sizehd(0) = len(trim(line))
		 Write(asizeh(0),'(I3)') sizehd(0)
	     Write(2,'(T1,A'//Asizeh(0)//')')line(1:sizehd(0))
		 Write(2,'(T1,''<PRE>'')')
	     if (Unitnm(J-1) .ne. Unitnm(J)) then
            Open(10,file=ArgV^.arg(1).P^//ArgV^.arg(Unitnm(J)).P^,
     +      Status='OLD')
         End If		   
20       Continue
            Read(10,'(A100)',End=30) line
		 If (line(1:2) .ne. 'C+') Go to 20
25       Read(10,'(A100)',End=30) line
            If (line(1:3) .ne. 'C--') Then
			   sizehd(0) = len(trim(line))
	           If (line(1:1) .eq. ' ') Then
	   		      Write(Asizeh(0),'(I3)') sizehd(0) 
			      Write(1,'(T1,A'//Asizeh(0)//')') line(1:sizehd(0))
			      Write(2,'(T1,A'//Asizeh(0)//')') line(1:sizehd(0))
	              Go to 25
			   Else
			      Write(Asizeh(0),'(I3)') max(0,sizehd(0)-2)
			      Write(1,'(T1,A'//Asizeh(0)//')') line(3:sizehd(0))
			   End If
	           lineout = ''
			   starto = 1
			   I = 3
50             Continue
			      If (line(I:I) .eq. '<') Then
				     lineout(starto:starto+3) = '&lt;'
					 I = I + 1
					 starto = starto + 4
				  Else if (line(I:I) .eq. '>') then
				  	 lineout(starto:starto+3) = '&gt;'
					 I = I + 1
					 starto = starto + 4
				  Else if (line(I:I) .eq. '&') Then
				     lineout(starto:starto+4) = '&amp;'
					 I = I + 1
					 starto = starto + 5
				  Else If (line(I:I) .eq. 'P')  Then
				     If ((I .lt. sizehd(0)) .and. (line(I+1:I+1) .eq. 'G')) Then
					    match = 0
					    Do 55 K = 1, numsub
						   If (((sizehd(0) - I+1) .ge. end(K))
     +                       .and. (line(I+2:I-1+end(K)) .eq. header(K)(3:end(K))))Then
	                         match = Match + 1
							 headar(match) = K
						   End If
55                      Continue
                        If (match .ge. 1) Then
						   ndum = headar(1)
						   If (match .gt. 1) Then
							  Do 56 K = 1, Match - 1 
							     If (end(headar(K+1)) .gt. end(ndum)) Then
								    ndum = headar(K+1)
								 End If
56                            Continue
                           End If
						   lineout(starto:starto+15+2*end(ndum)) = '<A href=""#'//
     +                         header(ndum)(1:end(ndum))//'"">'//
     +                         header(ndum)(1:end(ndum))//'</A>'
	                       starto = starto + 16 + 2*end(ndum)
						   I = I + end(ndum)
	                    Else 
				           lineout(starto:starto) = line(I:I)
						   I = I + 1
					       starto = starto + 1
						End If
					 Else
				        lineout(starto:starto) = line(I:I)
						I = I + 1
					    starto = starto + 1
					 End If
				  Else
				     lineout(starto:starto) = line(I:I)
					 I = I + 1
					 starto = starto + 1
				  End If
               If (I .le. sizehd(0)) Go to 50
C  Write out line to pgplot.html.
			   sizehd(0) = len(trim(lineout))
			   Write(Asizeh(0),'(I3)') sizehd(0)
			   Write(2,'(T1,A'//Asizeh(0)//')')lineout(1:sizehd(0))
			   Go to 25
			Else
		       Write(2,'(T1,''</PRE>'')')
			End If
30    Continue

      Write(2,'(T1,''<HR>'',/,''</BODY></HTML>'')')
	  close(2)
	  Close(1)

      Call F_endMPW(0)
2000  Continue
      Write(*,*) ' Error: Could not open pgplot.doc.'
      Call F_EndMPW(1)
3000  Continue
      Write(*,*) ' Error: Could not open pgplot.html.'
      Call F_EndMPW(1)
      End
	  
	  Subroutine Sortar(Int1,Int2,N)
	  Implicit None
	  Integer Int1(*),Int2(*),N,I,ndum
	  Logical sorted
	  
5	  sorted = .True.
 	  I = 1
10       If (I .lt. N) Then
	        If (Int1(I+1) .lt. Int1(I)) Then
	   	       sorted = .False.
		       ndum = int1(I)
			   int1(I) = Int1(I+1)
			   int1(I+1) = ndum
		       ndum = int2(I)
			   int2(I) = Int2(I+1)
			   int2(I+1) = ndum
		    End If
			I = I + 1
			Go to 10
		 End If
	  If (.not. sorted) Go to 5
	  Return
	  End
	  