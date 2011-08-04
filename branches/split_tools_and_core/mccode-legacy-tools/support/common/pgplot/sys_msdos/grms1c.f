      include 'flib.fi'
      INCLUDE 'FGRAPH.FI'
      SUBROUTINE GRMS1C( IX, IY, CHR, VID)
      INCLUDE 'FGRAPH.FD'
      RECORD /xycoord/ XY
      RECORD /VIDEOCONFIG/ VID
      INTEGER*2 IX, IY
      CHARACTER*(*) CHR
C*  cursor key input
      INTEGER*4 IMSIZE,INC,CNT(2),IERR
      INTEGER*2 X0, Y0, X1, Y1, DUMMY, ACTION,
     c IHR,IMIN,ISEC,ITICK
      INTEGER*1 SCAN, ICHR, BUFFER[ALLOCATABLE] (:)
      DATA ACTION/ $GPSET /
C OVERKILL ON IMAGESIZE IN CASE THERE ARE BYTE ALLIGNMENT ISSUES
      IMSIZE = IMAGESIZE( 0,0,25,25 )
      ALLOCATE( BUFFER( IMSIZE ), STAT = IERR )
      IF( IERR .NE. 0 ) THEN
           DUMMY = SETVIDEOMODE( $DEFAULTMODE )
           STOP 'Error: insufficient memory'
      ENDIF
C COUNTER AND INCREMENT TO ADD CURSOR ACCELERATION
      CNT(1) = 0
      INC = 1
      ICHR = 0
      DO WHILE(ICHR .EQ. 0)
         IX = MAX0( IX, 0)
         IY = MAX0( IY, 0)
         IX = MIN0( IX, (VID.NUMXPIXELS - 1))
         IY = MIN0( IY, (VID.NUMYPIXELS - 1))
         X0 = MAX0( (IX - 10), 0 )
         Y0 = MAX0( (IY - 10), 0 )
         X1 = MIN0( (IX + 10), (VID.NUMXPIXELS - 1))
         Y1 = MIN0( (IY + 10), (VID.NUMYPIXELS - 1))
C SAVE IMAGE BELOW WHERE CURSOR WILL BE
         CALL GETIMAGE( X0, Y0, X1, Y1, BUFFER )
C NOW DRAW CURSOR
         CALL MOVETO( X0, IY, XY)
         DUMMY = LINETO( X1, IY)
         CALL MOVETO( IX, Y0, XY)
         DUMMY = LINETO( IX, Y1)
         CALL GETCH(ICHR,SCAN)
C RESTORE IMAGE
         CALL PUTIMAGE( X0, Y0, BUFFER, ACTION )
C CALCULATE TIME PAST AND ACCELERATE IF NECESSARY
         CALL GETTIM(IHR,IMIN,ISEC,ITICK)
         CNT(2) = ITICK + 100*ISEC + 6000*IMIN
         IF ((CNT(2)-CNT(1)) .LT. 25) THEN
          INC = MIN0((INC + 1),30)
         ELSE
          INC = 1
         ENDIF
         CNT(1) = CNT(2)
         IF(SCAN .EQ. #48) THEN
         IY = IY - INC
         ELSE IF (SCAN .EQ. #50) THEN
         IY = IY + INC
         ELSE IF(SCAN .EQ. #4D) THEN
         IX = IX + INC
         ELSE IF(SCAN .EQ. #4B) THEN
         IX = IX - INC
         ENDIF
      ENDDO
      DEALLOCATE( BUFFER )
      CHR =CHAR(ICHR)
      RETURN
      END
C------
      SUBROUTINE GETCH(CHR,SCAN)
      include 'flib.fd'
      integer*1 chr,scan 
ctd 12/93  read keyboard, cursors
      character*1 result
      chr=#00
      scan=#00
      result=getcharqq()
      chr=ichar(result)
      if(chr.eq.#00)then
       result=getcharqq()
       scan=ichar(result)
      endif
      return
      end
