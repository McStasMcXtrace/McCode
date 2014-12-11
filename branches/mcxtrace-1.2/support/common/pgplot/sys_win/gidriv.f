!C*GIDRIV -- PGPLOT GIF drivers
!C+
      SUBROUTINE GIDRIV (IFUNC, RBUF, NBUF, CHR, LCHR, MODE)
      IMPLICIT NONE
      INTEGER  IFUNC, NBUF, LCHR, MODE
      REAL     RBUF(*)
      CHARACTER*(*) CHR
!
! PGPLOT driver for Graphics Interchange Format (GIF) files.
!
!***********************************************************************
!                           CAUTION                                    *
!                                                                      *
! The GIF specification incorporates the Lempel-Zev-Welch (LZW)        *
! compression technology which is the subject of a patent awarded to   *
! Unisys. Use of this technology, and in particular creation of GIF    *
! format files using this PGPLOT device driver, may require a license  *
! from Unisys.                                                         *
!***********************************************************************
!
! Supported device: GIF87a file format
!
! Device type codes: /GIF or /VGIF
!
! Default device name: pgplot.gif.
!
! If you have more than one image to plot (i.e. use PGPAGE) with this
! device, subsequent pages will be named: pgplot2.gif, pgplot3.gif,
! etc, disrespective of the device name you specified.
! You can however bypass this by specifying a device name including a
! number sign (#), which will henceforth be replaced by the pagenumber.
! Example: page#.gif will produce files page1.gif, page2.gif, ...,
! page234.gif, etc.
!
! Default view surface dimensions are:
! - GIF  : 1200 x  900 pixels (translates to 8.0 x 6.0 inch).
! - VGIF :  900 x 1200 pixels (translates to 6.0 x 8.0 inch).
! with an assumed scale of 150 pixels/inch.
! Default width and height can be overridden by specifying environment
! variables
! PGPLOT_GIF_WIDTH  (default 1200)
! PGPLOT_GIF_HEIGHT (default 900)
!
! Color capability:
! Indices 0 to 255 are supported. Each of these indices can be assigned
! one color. Default colors for indices 0 to 15 are implemented.
!
! Obtaining hardcopy: Use a GIF viewer or converter.
!=
!  1-Aug-1994 - Created by Remko Scharroo
!  9-Aug-1994 - New scheme for line plotting
! 16-Aug-1994 - Provide multi-image plotting.
!  8-Sep-1994 - Add opcode 29 [TJP].
!  5-Nov-1994 - Adjust size of bitmap if necessary [TJP].
! 18-Jan-1995 - Attempt to prevent integer overflow on systems where
!               BYTE is signed [TJP].
! 28-Dec-1995 - prevent concurrent access [TJP].
! 10-Jul-1996 - Fortran (F90) version; make PIXMAP and WORK allocatable 
!               arrays; remove all pass-by-value; test ALLOCATED(PIXMAP);
!               direct-access binary file output; higher resolution [PAS]
! 24-Apr-1998 - CASE structure instead of computed GOTO; "!" comments [PAS]
! 28-Aug-1998 - FORM='BINARY' in OPEN statements (Digital Visual Fortran 5.0) 
!               *** Note: Not standard F90; remove if troublesome. [BRupp]
! 26-Dec-2000 - treat signed bytes differently (Digital Fortran 6.0) [PAS]
!-----------------------------------------------------------------------
      CHARACTER*(*) LTYPE, PTYPE, DEFNAM
      INTEGER DWD, DHT, BX, BY
      PARAMETER (LTYPE=                                                 &
     &'GIF   (Graphics Interchange Format file, landscape orientation)',&
     & PTYPE=                                                           &
     &'VGIF  (Graphics Interchange Format file, portrait orientation)') 
      PARAMETER (DEFNAM='pgplot.gif')
      PARAMETER (DWD=1200, DHT=900)
      REAL      XRES, YRES
      PARAMETER (XRES=150., YRES=XRES)
!
      INTEGER   UNIT, IC, NPICT, MAXIDX, STATE
      INTEGER   CTABLE(3,0:255), CDEFLT(3,0:15)
      INTEGER   IER, I, L, IOS, IX0, IY0, IX1, IY1
      INTEGER   GRTRIM
      CHARACTER*80 MSG, INSTR, FILENM
!
! Note: for 64-bit operating systems, change the following 
! declaration to INTEGER*8:
!
      INTEGER(1),ALLOCATABLE :: PIXMAP(:,:)
      INTEGER(2),ALLOCATABLE :: WORK(:,:)
!
      SAVE UNIT, IC, CTABLE, NPICT, MAXIDX, BX, BY, PIXMAP, FILENM
      SAVE CDEFLT, STATE
      DATA CDEFLT /000,000,000, 255,255,255, 255,000,000, 000,255,000,  &
     &             000,000,255, 000,255,255, 255,000,255, 255,255,000,  &
     &             255,128,000, 128,255,000, 000,255,128, 000,128,255,  &
     &             128,000,255, 255,000,128, 085,085,085, 170,170,170/
      DATA STATE /0/
!-----------------------------------------------------------------------
!
      SELECT CASE(IFUNC)
!
!--- IFUNC = 1, Return device name -------------------------------------
      CASE (1)
         SELECT CASE(MODE)
         CASE (1)
            CHR = LTYPE
            LCHR = LEN(LTYPE)
         CASE (2)
            CHR = PTYPE
            LCHR = LEN(PTYPE)
         CASE DEFAULT
            CALL GRWARN('Requested MODE not implemented in GIF driver')
         END SELECT
!
!--- IFUNC = 2, Return physical min and max for plot device, and range
!               of color indices ---------------------------------------
!     (Maximum size is set by GIF format to 2**16 pixels)
      CASE (2)
         RBUF(1) = 0
         RBUF(2) = 65536 
         RBUF(3) = 0
         RBUF(4) = 65536
         RBUF(5) = 0
         RBUF(6) = 255
         NBUF = 6
!
!--- IFUNC = 3, Return device resolution -------------------------------
      CASE (3)
         RBUF(1) = XRES
         RBUF(2) = YRES 
         RBUF(3) = 1
         NBUF = 3
!
!--- IFUNC = 4, Return misc device info --------------------------------
!    (This device is Hardcopy, supports rectangle fill, pixel 
!     primitives, and query color rep.)
      CASE (4)
         CHR = 'HNNNNRPNYN'
         LCHR = 10
!
!--- IFUNC = 5, Return default file name -------------------------------
      CASE (5)
         CHR = DEFNAM
         LCHR = LEN(DEFNAM)
!
!--- IFUNC = 6, Return default physical size of plot -------------------
      CASE (6)
         RBUF(1) = 0
         RBUF(2) = BX-1 
         RBUF(3) = 0
         RBUF(4) = BY-1
         NBUF = 4
!
!--- IFUNC = 7, Return misc defaults -----------------------------------
      CASE (7)
         RBUF(1) = 1
         NBUF=1
!
!--- IFUNC = 8, Select plot --------------------------------------------
      CASE (8)
!
!--- IFUNC = 9, Open workstation ---------------------------------------
      CASE (9)
!        -- check for concurrent access
         IF (STATE.EQ.1) THEN
            CALL GRWARN('a PGPLOT GIF file is already open')
            RBUF(1) = 0
            RBUF(2) = 0
            RETURN
         END IF
!        -- dimensions of plot buffer
         IF (MODE.EQ.1) THEN
!        -- Landscape
            BX = DWD
            BY = DHT
            CALL GRGENV('GIF_WIDTH', INSTR, L)
            IF (L.GT.0) READ(INSTR(:L),'(BN,I10)',IOSTAT=IOS) BX
            CALL GRGENV('GIF_HEIGHT', INSTR, L)
            IF (L.GT.0) READ(INSTR(:L),'(BN,I10)',IOSTAT=IOS) BY
         ELSE
!        -- Portrait
            BX = DHT
            BY = DWD
            CALL GRGENV('GIF_WIDTH', INSTR, L)
            IF (L.GT.0) READ(INSTR(:L),'(BN,I10)',IOSTAT=IOS) BY
            CALL GRGENV('GIF_HEIGHT', INSTR, L)
            IF (L.GT.0) READ(INSTR(:L),'(BN,I10)',IOSTAT=IOS) BX
         END IF
         NPICT=1
         MAXIDX=0
!        -- Initialize color table
         DO I=0,15
            CTABLE(1,I) = CDEFLT(1,I)
            CTABLE(2,I) = CDEFLT(2,I)
            CTABLE(3,I) = CDEFLT(3,I)
         END DO
         DO I=16,255
            CTABLE(1,I) = 128
            CTABLE(2,I) = 128
            CTABLE(3,I) = 128
         END DO
!
         FILENM = CHR(:LCHR)
         CALL GRGI10 (FILENM, NPICT, CHR)
         LCHR = GRTRIM(CHR)
         CALL GRGLUN(UNIT)
         NBUF = 2
         RBUF(1) = UNIT
         OPEN (UNIT,FILE=CHR(1:LCHR),STATUS='UNKNOWN',ACCESS='DIRECT',  &
     &         RECL=255, FORM='BINARY', IOSTAT=IER)
         IF (IER.NE.0) THEN
            MSG = 'Cannot open output file for GIF plot: '//CHR(:LCHR)
            CALL GRWARN(MSG)
            RBUF(2) = 0
            STATE = 0
            CALL GRFLUN(UNIT)
         ELSE
            RBUF(2) = 1
            STATE = 1
         END IF
!
!--- IFUNC=10, Close workstation ---------------------------------------
      CASE (10)
         STATE = 0
!
!--- IFUNC=11, Begin picture -------------------------------------------
      CASE (11)
         BX = NINT(RBUF(1))+1
         BY = NINT(RBUF(2))+1
         ALLOCATE (PIXMAP(BX,BY), STAT=IER)
         IF (IER.NE.0) THEN
            CALL GRWARN('Failed to allocate plot buffer.')
            BX = 0
            BY = 0
         ELSE
!        -- initialize to zero (background color)
            CALL GRGI03(1, 1, BX, BY, 0, BX, BY, PIXMAP)
            IF (NPICT.GT.1) THEN
               CALL GRGI10 (FILENM, NPICT, MSG)
               CALL GRGLUN(UNIT)
               OPEN (UNIT, FILE=MSG, STATUS='UNKNOWN', ACCESS='DIRECT', &
     &               RECL=255, FORM='BINARY', IOSTAT=IER)
               IF (IER.NE.0)                                            &
     &            CALL GRWARN('Cannot open output file for GIF plot')
            END IF
         END IF
!
!--- IFUNC=12, Draw line -----------------------------------------------
      CASE (12)
         IX0=NINT(RBUF(1))+1
         IX1=NINT(RBUF(3))+1
         IY0=BY-NINT(RBUF(2))
         IY1=BY-NINT(RBUF(4))
         IF (ALLOCATED(PIXMAP))                                         &
     &        CALL GRGI01(IX0, IY0, IX1, IY1, IC, BX, BY, PIXMAP)
!
!--- IFUNC=13, Draw dot ------------------------------------------------
      CASE (13)
         IX0=NINT(RBUF(1))+1
         IY0=BY-NINT(RBUF(2))
         IF (ALLOCATED(PIXMAP))                                         &
     &        CALL GRGI01(IX0, IY0, IX0, IY0, IC, BX, BY, PIXMAP)
!
!--- IFUNC=14, End picture ---------------------------------------------
      CASE (14)
         IF (UNIT.GE.0) THEN
            ALLOCATE (WORK(4098,256), STAT=IER)
            IF (IER.NE.0) THEN
               CALL GRWARN('Failed to allocate work array.')
            ELSE
               CALL GRGI06(UNIT, BX, BY, CTABLE, PIXMAP, MAXIDX, WORK)
            END IF
            CLOSE (UNIT)
            CALL GRFLUN(UNIT)
            DEALLOCATE (WORK, STAT=IER)
         END IF
         NPICT = NPICT+1
         DEALLOCATE (PIXMAP, STAT=IER)
         IF (IER.NE.0) THEN
            CALL GRWARN('Failed to deallocate plot buffer.')
         END IF
!
!--- IFUNC=15, Select color index --------------------------------------
      CASE (15)
         IC = RBUF(1)
         MAXIDX = MAX(MAXIDX, IC)
!
!--- IFUNC=16, Flush buffer. -------------------------------------------
!    (Not used.)
      CASE (16)
!
!--- IFUNC=18, Erase alpha screen. -------------------------------------
!    (Not implemented: no alpha screen)
      CASE (18)
!
!--- IFUNC=21, Set color representation. -------------------------------
      CASE (21)
         I = RBUF(1)
         CTABLE(1, I) = NINT(RBUF(2)*255)
         CTABLE(2, I) = NINT(RBUF(3)*255)
         CTABLE(3, I) = NINT(RBUF(4)*255)
!
!--- IFUNC=23, Escape --------------------------------------------------
!    (Not implemented: ignored)
      CASE (23)
!
!--- IFUNC=24, Rectangle fill ------------------------------------------
      CASE (24)
         IX0=NINT(RBUF(1))+1
         IX1=NINT(RBUF(3))+1
         IY1=BY-NINT(RBUF(2))
         IY0=BY-NINT(RBUF(4))
         IF (ALLOCATED(PIXMAP))                                         &
     &        CALL GRGI03(IX0, IY0, IX1, IY1, IC, BX, BY, PIXMAP)
!
!--- IFUNC=25, Not implemented -----------------------------------------
      CASE (25)
!
!--- IFUNC=26, Line of pixels ------------------------------------------
      CASE (26)
         CALL GRGI04(NBUF, RBUF, BX, BY, PIXMAP, MAXIDX)
!
!--- IFUNC=27, Not implemented -----------------------------------------
      CASE (27)
!
!--- IFUNC=28, Not implemented -----------------------------------------
      CASE (28)
!
!--- IFUNC=29, Query color representation. -----------------------------
      CASE (29)
         I = RBUF(1)
         RBUF(2) = CTABLE(1,I)/255.0
         RBUF(3) = CTABLE(2,I)/255.0
         RBUF(4) = CTABLE(3,I)/255.0
         NBUF = 4
!
!--- Unimplemented Function
      CASE DEFAULT
         WRITE (MSG, '(I10)') IFUNC
         CALL GRWARN('Unimplemented function in GIF device driver:'//   &
     &               MSG)
         NBUF = -1
      END SELECT
      RETURN
!-----------------------------------------------------------------------
      END
!
!*GRGI01 -- PGPLOT GIF driver, draw line
!+
      SUBROUTINE GRGI01 (IX0, IY0, IX1, IY1, ICOL, BX, BY, PIXMAP)
      INTEGER IX0, IY0, IX1, IY1
      INTEGER ICOL, BX, BY
      INTEGER(1) :: PIXMAP(BX,BY)
!
! Draw a straight-line segment from absolute pixel coordinates
! (IX0, IY0) to (IX1, IY1).
!
! Arguments:
!  ICOL            (input): Color index
!  PIXMAP   (input/output): The image data buffer.
!-----------------------------------------------------------------------
      INTEGER IX, IY, IS
      REAL    D
      INTEGER (1) :: VAL
!
      IF (ICOL.GT.127) THEN
         VAL = ICOL - 256
      ELSE
         VAL = ICOL
      END IF
      IF (IX0.EQ.IX1 .AND. IY0.EQ.IY1) THEN
         PIXMAP(IX0,IY0) = VAL
      ELSE IF (ABS(IY1-IY0) .GT. ABS(IX1-IX0)) THEN
         D  = FLOAT(IX1-IX0) / FLOAT(IY1-IY0)
         IS = 1
         IF (IY1 .LT. IY0) IS = -1
         DO IY=IY0,IY1,IS
            IX = NINT(IX0 + (IY-IY0)*D)
            PIXMAP(IX,IY) = VAL
	   END DO
      ELSE
         D  = FLOAT(IY1-IY0) / FLOAT(IX1-IX0)
         IS = 1
         IF (IX1 .LT. IX0) IS = -1
         DO IX=IX0,IX1,IS
            IY = NINT(IY0 + (IX-IX0)*D)
            PIXMAP(IX,IY) = VAL
	   END DO
      END IF
      END
!
!*GRGI03 -- PGPLOT GIF driver, fill rectangle
!+
      SUBROUTINE GRGI03 (IX0, IY0, IX1, IY1, ICOL, BX, BY, PIXMAP)
      INTEGER IX0, IY0, IX1, IY1
      INTEGER ICOL, BX, BY
      INTEGER(1) :: PIXMAP(BX,BY)
!
! Arguments:
!  IX0, IY0        (input): Lower left corner.
!  IX1, IY1        (input): Upper right corner.
!  ICOL            (input): Color value.
!  BX, BY          (input): dimensions of PIXMAP.
!  PIXMAP   (input/output): The image data buffer.
!-----------------------------------------------------------------------
      INTEGER IX, IY
      INTEGER(1) :: VAL
!
      IF (ICOL.GT.127) THEN
         VAL = ICOL - 256
      ELSE
         VAL = ICOL
      END IF
      DO IY=IY0,IY1
         DO IX=IX0,IX1
            PIXMAP(IX,IY) = VAL
            END DO
	END DO
	RETURN
      END
!
!*GRGI04 -- PGPLOT GIF driver, fill image line
!+
      SUBROUTINE GRGI04(NBUF,RBUF,BX,BY,PIXMAP,MAXIDX)
      INTEGER I,J,NBUF,BX,BY,N,IC,MAXIDX
      REAL    RBUF(NBUF)
      INTEGER(1) :: PIXMAP(BX,BY)
!-
      I = NINT(RBUF(1)) + 1
      J = BY-NINT(RBUF(2))
      DO N=3,NBUF
         IC     = RBUF(N)
         MAXIDX = MAX(MAXIDX, IC)
         IF (IC .GT. 127) IC = IC - 256
         PIXMAP(I+N-3,J) = IC
	END DO
	RETURN
      END
!
!*GRGI06 -- PGPLOT GIF driver, write GIF image
!+
      SUBROUTINE GRGI06 (UNIT, BX, BY, CTABLE, PIXMAP, MAXIDX, CODE)
      INTEGER   UNIT, BX, BY, MAXIDX
      INTEGER   CTABLE(3,0:255)
      INTEGER(1) :: PIXMAP(BX*BY)
      INTEGER*2 CODE(0:4097,0:255)
!
! Write GIF image to UNIT.
!
! Arguments:
! UNIT   (input): Output unit
! BX,BY  (input): `Screen' size
! CTABLE  (input): Color map
! PIXMAP (input): Image data
! MAXIDX (input): maximum color index used.
!--
! 16-Nov-94: fixed bug (BYTE is signed)
! 09-Jul-96: revised to use Fortran90 constructs and I/O [PAS]
! 26-Dec-00: treat signed bytes differently for Fortran v.6 [PAS]
!-----------------------------------------------------------------------
      CHARACTER GIF1*6
      INTEGER*4 BWIDTH, BSHIFT, BREST, BOUT, BREC
      INTEGER PIXEL, I, J, K, M, CLEAR, EOI, TABLE, IN, TOTAL, PRE, EXT
      INTEGER OLDPRE, BITS
      INTEGER (1) :: BLKOUT(0:266)
      COMMON  /GRGICO/ BWIDTH, BSHIFT, BREST, BOUT, BREC, BLKOUT
!
      BITS = 1
      DO WHILE (2**BITS .LE. MAXIDX)
         BITS = BITS + 1
      END DO
      PIXEL = MAX(BITS, 2)
      CLEAR = 2**PIXEL
      EOI   = CLEAR + 1
!
! Write Header.
!
! Write Logical Screen Descriptor (screen width, screen height,
! color data, background color index [0], pixel aspect ratio [0]).
      GIF1 = 'GIF87a'
      DO I=1,6
         BLKOUT(I-1) = ICHAR(GIF1(I:I))
      END DO
	CALL GRGI09(BX, BLKOUT(6))
	CALL GRGI09(BY, BLKOUT(8))
	I = ISHFT(BITS-1, 4)
      BLKOUT(10) = IOR(IOR(128, I), BITS-1)
      BLKOUT(11) = 0
      BLKOUT(12) = 0
      BOUT = 12
      BREC = 1
!
! Write Global Color Table.
      DO J=0,2**BITS-1
         BLKOUT(BOUT+1) = CTABLE(1,J)
         BLKOUT(BOUT+2) = CTABLE(2,J)
         BLKOUT(BOUT+3) = CTABLE(3,J)
         BOUT = BOUT + 3
         IF (BOUT .GE. 254) THEN
            WRITE (UNIT,REC=BREC) (BLKOUT(I),I=0,254)
            BOUT = BOUT - 255
            DO I=0,BOUT
               BLKOUT(I) = BLKOUT(I+255)
            END DO
            BREC = BREC + 1
         END IF
      END DO
!
! Write Image Descriptor.
      BLKOUT(BOUT+1) = ICHAR(',')
      BLKOUT(BOUT+2) = 0
      BLKOUT(BOUT+3) = 0
      BLKOUT(BOUT+4) = 0
      BLKOUT(BOUT+5) = 0
      CALL GRGI09(BX, BLKOUT(BOUT+6))
      CALL GRGI09(BY, BLKOUT(BOUT+8))
      BLKOUT(BOUT+10) = 0
      BLKOUT(BOUT+11) = PIXEL
      BOUT = BOUT + 11
      IF (BOUT .EQ. 253) THEN
!        This is a nasty surprise - make a dummy 1-byte sub-block
         BLKOUT(254) = 1
         BLKOUT(255) = CLEAR
         BOUT = 255
      END IF
!     Check if record already longer than 255 bytes
      IF (BOUT .GE. 254) THEN
         WRITE (UNIT,REC=BREC) (BLKOUT(I),I=0,254)
         BOUT = BOUT - 255
         DO I=0,BOUT
            BLKOUT(I) = BLKOUT(I+255)
         END DO
         BREC = BREC + 1
      END IF
!
! Write Table Based Image Data, in sub-blocks of up to 255 bytes.
!     How many bytes left for a sub-block in this direct-access record?
      BOUT = BOUT + 1
      BLKOUT(BOUT) = 254 - BOUT
!
! LZW-compression; initialize counters
! Start packing variable-size codes into 8-bit bytes.
!
      BREST  = 0
      BSHIFT = 0
! `Read' first character.
      IN    = 1
      TOTAL = BX * BY
      PRE   = PIXMAP(IN)
      IF (PRE .LT. 0) PRE = PRE + 256
!
      TABLE  = 4095
      BWIDTH = PIXEL + 1
!                          
      DO WHILE (IN .LT. TOTAL)
         IF (TABLE .GE. 4095) THEN
!           Start new data stream; if n=PIXEL, then
!           2**n-1  (n+1)-bit codes
!           2*2**n  (n+2)-bit codes
!           4*2**n  (n+3)-bit codes
!              .         .      .
!             1024     11-bit codes
!             2048     12-bit codes (incl. one clear code)
!
!           push a clear code first
            CALL GRGI07(UNIT, CLEAR)
            DO M=0,255
               DO K=0,4095
                  CODE(K,M) = 0
               END DO
            END DO  
!           Initialize last used table entry and code width in bits
            TABLE  = EOI
            BWIDTH = PIXEL + 1                    
         END IF
!
! `Read' next character; check if combination prefix/extension occurred earlier
         IN     = IN + 1 
         EXT    = PIXMAP(IN)
         IF (EXT .LT. 0) EXT = EXT + 256
         OLDPRE = PRE
         PRE    = CODE(PRE, EXT)
!
         IF (PRE .EQ. 0) THEN
!           If no earlier occurrence add combination to table
            CALL GRGI07(UNIT, OLDPRE)
            TABLE = TABLE + 1
            CODE(OLDPRE,EXT) = TABLE
            PRE = EXT
!           May need to increase width of code entry by one bit
            IF (ISHFT(TABLE,-BWIDTH) .NE. 0) BWIDTH = BWIDTH + 1
         END IF
      END DO
!
! Last character
!
      CALL GRGI07(UNIT, PRE)
      CALL GRGI07(UNIT, EOI)
      IF (BSHIFT .GT. 0) CALL GRGI08(UNIT, BREST)
      IF (BOUT .GT. 0) THEN
         BLKOUT(0) = BOUT
         BOUT = BOUT + 1
      END IF
      BLKOUT(BOUT) = 0
	IF (BOUT .EQ. 254) THEN
	   WRITE (UNIT, REC=BREC) (BLKOUT(I),I=0,254)
	   BOUT = -1  
	   BREC = BREC + 1
	END IF
!
! Write GIF Trailer.
!
      BLKOUT(BOUT+1) = ICHAR(';')
      DO I=BOUT+2,254
!        Zero fill the final direct-access record
         BLKOUT(I) = 0
      END DO
      WRITE (UNIT,REC=BREC) (BLKOUT(I),I=0,254)
      RETURN
      END
!
!*GRGI07 -- Compress GIF output codes into 8-bit bytes
!
      SUBROUTINE GRGI07(UNIT, INCODE)
      INTEGER UNIT, INCODE
      INTEGER*4 BWIDTH, BSHIFT, BREST, BOUT, BREC
      INTEGER(1) :: BLKOUT(0:266)
      COMMON  /GRGICO/ BWIDTH, BSHIFT, BREST, BOUT, BREC, BLKOUT
!
      BREST  = IOR(BREST, ISHFT(INCODE, BSHIFT))
      BSHIFT = BSHIFT + BWIDTH
!
      DO WHILE (BSHIFT .GE. 8)
!        Send low byte to buffer, and delete it
         CALL GRGI08(UNIT, BREST)
         BREST  = ISHFT(BREST, -8)
         BSHIFT = BSHIFT - 8
      END DO
!
      RETURN
      END
!
!*GRGI08 -- Accumulate bytes and write GIF output sub-block
!
      SUBROUTINE GRGI08(UNIT, INCODE)
      INTEGER UNIT, INCODE, I
      INTEGER*4 BWIDTH, BSHIFT, BREST, BOUT, BREC
      INTEGER(1) :: BLKOUT(0:266)
      COMMON  /GRGICO/ BWIDTH, BSHIFT, BREST, BOUT, BREC, BLKOUT
!
      BOUT = BOUT + 1
      BLKOUT(BOUT) = IAND(INCODE, 255)
      IF (BOUT .GE. 254) THEN
         WRITE (UNIT,REC=BREC) (BLKOUT(I),I=0,254)
         BOUT = 0
         BLKOUT(0) = 254
         BREC = BREC + 1
      END IF
      RETURN
      END
!
!*GRGI09 -- Encode integer into 2 bytes
!
      SUBROUTINE GRGI09(IN, IOUT)
      INTEGER IN
      INTEGER(1) :: IOUT(2)
!
      IOUT(1) = IAND(IN, 255)
      IOUT(2) = ISHFT(IN, -8)
      RETURN
      END
!
!*GRGI10 -- Replace # in filename by picture number
!
      SUBROUTINE GRGI10 (NAME1, NP, NAME2)
      CHARACTER*(*) NAME1
      CHARACTER*(*) NAME2
      CHARACTER*80  TMP
      INTEGER GRTRIM
      INTEGER NP, IDX, L, LN
!
      LN  = GRTRIM(NAME1)
      IDX = INDEX(NAME1,'#')
      IF (IDX .GT. 0) THEN
!        -- if the supplied name contains a #-character, replace
!           it with the page number
         CALL GRFAO(NAME1, L, TMP, NP, 0, 0, 0)
      ELSE IF (NP .EQ. 1) THEN
!        -- if this is the first page, use the supplied name
         NAME2 = NAME1
         RETURN
      ELSE IF (LN+2 .LE. LEN(NAME1)) THEN
!        -- append an underscore and the page number to the supplied
!           name
         NAME1(LN+1:LN+2) = '_#'
         CALL GRFAO(NAME1, L, TMP, NP, 0, 0, 0)
      ELSE
!        -- last resort: invent a new name
         CALL GRFAO('pgplot#.gif', L, TMP, NP, 0, 0, 0)
      END IF
      CALL GRWARN ('Writing new GIF image as: '//TMP(:L))
      NAME2 = TMP(:L)
      RETURN
      END
