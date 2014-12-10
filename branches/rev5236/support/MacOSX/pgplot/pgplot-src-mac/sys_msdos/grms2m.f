      INCLUDE 'FLIB.FI'   !include if compiled separately
      INCLUDE 'FGRAPH.FI'
      INCLUDE 'MOUSE.FI'
      SUBROUTINE GRMS2M( IX, IY, CHR)
      INCLUDE 'FLIB.FD'
      INCLUDE 'FGRAPH.FD'
      INCLUDE 'MOUSE.FD'
C* pos and return mouse cursor, return key pressed
C C. T. Dum March 23,1995
c link with mouse.obj (FL32 distr.)
      RECORD /EVENT/ pEvent
      INTEGER*4 ichr
      INTEGER*2 IX, IY
      CHARACTER*(*) CHR
      ICHR = 0
c move mouse cursor, exit if key other than Function or Cursor is
c pressed
      DO WHILE(ICHR .EQ. 0)
         call setptrpos(int4(ix),int4(iy))
         call setptrvis(1)
         chr=getcharQQ()
         ichr=ichar(chr)
         if(ichr.eq.0) then
c function or cursor key was pressed
          chr=getcharQQ()
         endif
         call getptrpos(pEvent)
         ix=pEvent.x
         iy=pEvent.y
      ENDDO
      call setptrvis(2)
      RETURN
      END
