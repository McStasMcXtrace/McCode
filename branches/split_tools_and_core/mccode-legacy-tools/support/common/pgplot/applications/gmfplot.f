        PROGRAM GMFPLOT
C-----------------------------------------------------------------------
C Translate a metafile.
C-----------------------------------------------------------------------
	CHARACTER*8   TYPE
	CHARACTER*128 DEVICE,FILE
	INTEGER       GROPEN
	integer*2 chunk,i,j
	real xcp,ycp
	logical prompt, debug, grchkt
	common /metafile_status/ xcp, ycp, prompt, debug
	common /pltid/ id
C
C		Find and open the metafile.
C
	IER = LIB$GET_FOREIGN(FILE,'Input file: ',L)
	IF (IER.NE.1) CALL EXIT(IER)
	IF (L.LT.1) CALL EXIT
	I = 1
	DO WHILE (I.LE.L .AND. FILE(I:I).LE.' ')
		I = I+1
	END DO
	FILE = FILE(I:)
	I = 1
	DO WHILE (I.LE.L .AND. FILE(I:I).NE.' ' .AND. FILE(I:I).NE.'/')
		I = I+1
	END DO
	DEVICE = FILE(I:)
	FILE = FILE(:I-1)
	OPEN (UNIT=1,NAME=FILE,READONLY,SHARED,STATUS='OLD',
     1	      DEFAULTFILE='GRAPHICS.GMF',
     2	      FORM='UNFORMATTED',RECORDTYPE='FIXED',RECL=180,
     3	      IOSTAT=IER)
	IF (IER.NE.0) THEN
		CALL ERRSNS(,IER,IES,,IET)
		IF (IES.NE.0) CALL EXIT(IES)
		IF (IER.NE.0) CALL EXIT(IER)
		CALL EXIT(IET)
	END IF
C
C 		Find and open the output plot device.
C
	IER = 1
	IF (DEVICE.EQ.' ') IER = LIB$GET_INPUT(
     1		DEVICE,'Graphics device/type: ',L)
	IF (IER.NE.1) CALL EXIT(IER)
	IER = GROPEN(0,0,DEVICE,ID)
	IF (IER.NE.1) CALL EXIT(IER)
C
C!	call grinqtyp(type,prompt)
	call grqtyp(type,prompt)
	prompt = prompt .and. grchkt('sys$command')
	debug = type(1:4).eq.'NULL'
C
C		Read and translate the metafile.
C
	chunk = 0
	do while (chunk.ne.'8100'X)
	    call getchunk(chunk)
	    if (chunk.lt.0) then
		call dochunk(chunk)
	    else
		i = chunk
		xcp = i
		call getchunk(j)
		if (j.ge.0) then
		    ycp = j
D		    type *,'  MOVE',i,j
		    call grmova(xcp,ycp)
		else
		    j = ibclr(j,15)
		    ycp = j
D		    type *,'  DRAW',i,j
		    call grlina(xcp,ycp)
		end if
	    end if
	end do
	call grclos
	end

	subroutine dochunk(chunk)
C-----------------------------------------------------------------------
C GMFPLOT: interpret a non-positioning Metafile command. All
C non-positioning commands consist of a command chunk and zero or more
C parameter chunks. A non-positioning command has a '1' in the 
C high-order bit (15) of the command chunk. Bits 14-12 indicate one
C of 8 classes of Metafile commands; bits 11-8 indicate one of 16
C commands within the class; and bits 7-0 give the number of 16-bit
C parameter chunks that follow. DOCHUNK interprets the command chunk
C and reads and interprets the following parameter chunks. 
C
C Argument:
C   CHUNK (input, integer*2): the command chunk.
C
C T. J. Pearson, 4-Jun-1984.
C-----------------------------------------------------------------------
	implicit none
	character*4 bells
	parameter (bells=char(7)//char(7)//char(7)//char(7))
	integer*2 dummy,chunk,ci,cr,cg,cb,ix,iy
	integer attrib,c,i,j,k,l
	logical file_open, picture_open
	integer picture_number, marker
	character*4 nerd,junk
	real xcp,ycp
	real px(512),py(512)
	logical prompt,debug
        integer id
        real xscale
	common /metafile_status/ xcp, ycp, prompt, debug
	common /pltid/ id,xscale
C
 1000	format (1X,Z4.4,1X,A,T40,5I6)
C
C		Separate chunk into command class (c), command-index 
C		(i), and number of parameter chunks (j).
C
	c = ibits(chunk,12,3)
	i = ibits(chunk,8,4)
	j = ibits(chunk,0,8)
D	type '(1X,Z4.4,3I6)',chunk,c,i,j
	goto (100,101,102,103,104), c+1
	goto 900
C
C		Control commands --- class 0.
C
  100	if (i.eq.0) then		! BEGIN_METAFILE
	    j = j-1
	    call getchunk(dummy)
	    if (debug) write (6,1000) chunk,'BEGIN_METAFILE',dummy
	    if (dummy.ne.1) then
		call lib$put_output('%GMFPLOT, "BEGIN_METAFILE" '//
     1		     'command does not request 15-bit precision')
		call exit(44)
	    end if
	    if (file_open) then
		call lib$put_output('%GMFPLOT, "BEGIN_METAFILE" '//
     1		     'command is misplaced')
		call exit(44)
	    end if
	    file_open = .true.
	    call scale(32767,32767)
	else if (i.eq.1) then		! END_METAFILE
	    if (debug) write (6,1000) chunk,'END_METAFILE'
	    file_open = .false.
	else if (i.eq.2) then		! DEFINE_NDC_SPACE
	    j = j-3
	    call getchunk(ix)
	    call getchunk(iy)
	    call getchunk(dummy)
	    if (debug) write (6,1000) chunk,'DEFINE_NDC_SPACE',
     1			ix,iy,dummy
	    call scale(ix,iy)
	else if (i.eq.4) then		! NO_OPERATION
	    continue
	else
	    goto 900
	end if
	goto 800
C
C		Metafile picture commands --- class 1.
C
  101	if (i.eq.0) then		! BEGIN_PICTURE
	    j = j-1
	    call getchunk(dummy)
	    if (debug) write (6,1000) chunk,'BEGIN_PICTURE',dummy
	    picture_number = dummy
	    picture_open = .true.
	    if (prompt.and.picture_number.gt.1) then
	        call lib$get_input(junk,bells,L)
	    end if
	    call grpage
	    xcp = 0.0
	    ycp = 0.0
	    marker = 1
	else if (i.eq.1) then		! END_PICTURE
	    if (debug) write (6,1000) chunk,'END_PICTURE'
	    picture_open = .false.
	    call grterm
	else
	    goto 900
	end if
	goto 800
C
C		Mode and marker commands --- class 2.
C
  102	if (i.eq.0) then		! SET_2D_MODE
	    if (debug) write (6,1000) chunk,'SET_2D_MODE'
	    continue
	else if (i.eq.1) then		! SET_3D_MODE
	    if (debug) write (6,1000) chunk,'SET_3D_MODE'
	    call lib$put_output(
     1		'%GMFPLOT, SET_3D_MODE command not allowed')
	    call exit(44)
	else if (i.eq.2) then		! SET_MARKER_SYMBOL
	    j = j-1
	    call getchunk(dummy)
	    if (debug) write (6,1000) chunk,'SET_MARKER_SYMBOL',dummy
	    marker = dummy
	else if (i.eq.3) then		! OUTPUT_SELECTED_MARKER
	    if (debug) write (6,1000) chunk,'OUTPUT_SELECTED_MARKER'
C!	    call grmarker(marker,.false.,1,xcp,ycp)
	    call grmker(marker,.false.,1,xcp,ycp)
	else if (i.eq.4) then		! OUTPUT_SPECIFIC_MARKER
	    j = j-1
	    call getchunk(dummy)
	    if (debug) write (6,1000) chunk,'OUTPUT_SPECIFIC_MARKER',dummy
            attrib = dummy
C!	    call grmarker(attrib,.false.,1,xcp,ycp)
	    call grmker(attrib,.false.,1,xcp,ycp)
	else if (i.eq.8) then		! SET_MARKER_SIZE
	    j = j-1
	    call getchunk(dummy)
	    if (debug) write (6,1000) chunk,'SET_MARKER_SIZE',dummy
            call grsetc(id,xscale*dummy)
	else if (i.eq.7) then		! DRAW_POLYGON
	    j = j-1
	    call getchunk(dummy)
	    if (debug) write (6,1000) chunk,'DRAW_POLYGON',dummy
	    if (dummy.gt.512) then
		call lib$put_output('%GMFPLOT, "DRAW_POLYGON" '//
     1		     'command has more than 512 vertices')
	    end if
	    k = min(dummy,512)
	    do i=1,k
	    	call getchunk(ix)
	    	call getchunk(iy)
	    	px(i) = ibclr(ix,15)
	    	py(i) = ibclr(iy,15)
	    end do
	    call grfa(k,px,py)
	else
	    goto 900
	end if
	goto 800
C
C		Text commands --- class 3.
C
  103	if (i.eq.1) then		! SET_CHARACTER_FONT
	    j = j-1
	    call getchunk(dummy)
	    if (debug) write (6,1000) chunk,'SET_CHARACTER_FONT',dummy
	    attrib = dummy
	    call grsfnt(attrib)	
	else
	    goto 900
	end if
	goto 800
C
C		Attribute commands --- class 4.
C
  104	if (i.eq.0) then		! DEFINE_COLOR_INDEX
	    j = j-4
	    call getchunk(ci)
	    call getchunk(cr)
	    call getchunk(cg)
	    call getchunk(cb)
	    if (debug) write (6,1000) chunk,
     1			'DEFINE_COLOR_INDEX',ci,cr,cg,cb
            attrib = ci
	    call grscr(attrib,cr/32767.,cg/32767.,cb/32767.)
	else if (i.eq.1) then		! SET_COLOR
	    j = j-1
	    call getchunk(dummy)
	    attrib = dummy
	    if (debug) write (6,1000) chunk,'SET_COLOR',dummy
C!	    call grsetcol(attrib)
	    call grsci(attrib)
	else if (i.eq.2) then		! SET_INTENSITY
	    j = j-1
	    call getchunk(dummy)
	    if (debug) write (6,1000) chunk,'SET_INTENSITY',dummy
	    attrib = dummy
	    call grsetli(attrib)
	else if (i.eq.3) then		! SET_LINESTYLE
	    j = j-1
	    call getchunk(dummy)
	    if (debug) write (6,1000) chunk,'SET_LINESTYLE',dummy
	    attrib = dummy
C!	    call grsetls(attrib)
	    call grsls(attrib)
	else if (i.eq.4) then		! SET_LINEWIDTH
	    j = j-1
	    call getchunk(dummy)
	    if (debug) write (6,1000) chunk,'SET_LINEWIDTH',dummy
	    attrib = dummy
C!	    call grsetlw(attrib)
	    call grslw(attrib)
	else if (i.eq.5) then		! SET_PEN
	    j = j-1
	    call getchunk(dummy)
	    if (debug) write (6,1000) chunk,'SET_PEN',dummy
	    attrib = dummy
	    call grsetpen(attrib)
	else
	    goto 900
	end if
	goto 800
C
C		Report illegal command.
C
  900	call grterm
	write (nerd,'(Z4.4)') chunk
	call lib$put_output('%GMFPLOT, unrecognized command '//nerd)
	goto 800
C
C		Skip any additional chunks which have not been
C		decoded while interpreting the command.
C
  800	do k=1,j
	    call getchunk(dummy)
	end do
	return
	end 

	subroutine getchunk(chunk)
	integer*2 chunk,buffer(360)
	integer i
	data i/360/
	if (i.eq.360) then
	    read(unit=1,end=10) buffer
	    i = 1
	else
	    i = i+1
	end if
	chunk = buffer(i)
	return
10	chunk = '8100'X	! END_METAFILE
	return
	end

	subroutine scale (ix,iy)
C-----------------------------------------------------------------------
C GMFPLOT: scale output device so that a rectangle with metafile
C coordinates (0...ix), (0...iy) is mapped onto the largest possible
C rectangle with the same aspect ratio on the output device.
C-----------------------------------------------------------------------
	implicit none
	integer*2 ix,iy
	integer id
	real xdef,ydef,xmax,ymax,xperin,yperin
	real xsize_inches,ysize_inches,s
	real xorg,yorg,xscale,yscale
	common /pltid/ id,xscale
C
C		Obtain output device parameters.
C
	call grsize(id,xdef,ydef,xmax,ymax,xperin,yperin)
C
C		Size of output view surface in inches.
C
	xsize_inches = xdef/xperin
	ysize_inches = ydef/yperin
C
C		's' is the scale in inches-per-metafile-unit
C		which produces the largest plot which fits on the
C		output view surface.
C
	s = min(xsize_inches/ix,ysize_inches/iy)
C
C		'xscale' and 'yscale' are the corresponding scales
C		in device-units-per-metafile-unit.
C
	xscale = s*xperin
	yscale = s*yperin
C
C		'xorg' and 'yorg' are offsets to center the
C		plot on the view surface. One of these will be zero.
C
	xorg = xperin*(xsize_inches - ix*s)/2.0
	yorg = yperin*(ysize_inches - iy*s)/2.0
C
C		Set the transform parameters.
C
	call grtran(id,xorg,yorg,xscale,yscale)
D	type *,'Max size:   ',xsize_inches,ysize_inches
D	type *,'Actual size:',ix*s,iy*s
D	TYPE *,xorg,yorg,xscale,yscale
	return
	end
