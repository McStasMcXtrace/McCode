* Date: Tue, 24 Nov 92 10:20:47 -0700
* From: seeger@gem.LANL.GOV

      SUBROUTINE PGPLOT10
C
C     Subroutines to implement high-level functions from the author's 
C       PLOT-10 library, and to interpret calls to PLOT-10 primatives 
C       as corresponding entries in the PGPLOT library.  When logarithmic
C       axes have been defined by calling GRAPH, the PLOT-10 move and draw
C       calls will automatically take logarithms before plotting.  Additional
C       high-level calls (CONTOUR, CURVE, ERRBAR) which duplicate existing 
C       PGPLOT functions (but which support logarithmic scaling) are included 
C       at the end of this file.
C
C     P. A. Seeger, Los Alamos National Laboratory, Oct. 6, 1992
C
C     Entries:
C     ANCHO     DASHA     DASHR     DRAWA     DRAWR     DRWABS    DRWREL    
C     DSHABS    DSHREL    DWINDO    ERASE     FINITT    GRAPH     HDCOPY    
C     HLABEL    INITT     MOVABS    MOVEA     MOVER     MOVREL    OSTRING   
C     PNTABS    PNTREL    POINTA    POINTR    RESET     SCURSR    SEEDW     
C     SEELOC    SEETW     SETCOLOR  SWINDO    SYMBOL    TERM      TWINDO    
C     VCURSR    VLABEL    VWINDO    
C
C     Auxiliary Externals:
C     CHECK_TEXT   LINAXIS   LOGAXIS   
C
C     PGPLOT Externals:
C     PGBEGIN   PGBOX     PGCURSE   PGDRAW    PGEND     PGETXT    PGMOVE    
C     PGMTEXT   PGNUMB    PGPOINT   PGQINF    PGQPOS    PGQVP     PGSCH     
C     PGSCI     PGSLS     PGSLW     PGVPORT   PGWINDOW  
C
      IMPLICIT NONE
C
C     Variables which may occur in calling sequences of entry points
      CHARACTER TITLE*(*),SUBTITLE*(*),XLABEL*(*),YLABEL*(*),CH*1
      REAL XMN,XMX,X,YMN,YMX,Y
      INTEGER LOGX,LOGY,L,IX,IY,LX,LY
C
C     Local temporary variables:
      CHARACTER XOPTION*7,YOPTION*8,XFORMAT*6,YFORMAT*6,STRING*12,
     1 NEWSTRING*255
      REAL XX,XMAJOR,XMINOR,YY,YMAJOR,YMINOR
      INTEGER I,J,IXMAJOR,IXTICK,IYMAJOR,IYTICK,NC,LL,IX2,IY2
      LOGICAL LPOINT
C
C     Local variables to be saved between entries:
      CHARACTER TERMTYPE*40,HDCPYTYPE*40
      REAL ASPECT,X1,X2,XMIN,XMAX,XPERPIX,X0,Y1,Y2,YMIN,YMAX,YPERPIX,Y0
      INTEGER LINE,ISYMB,ITERM,IHDCPY
      LOGICAL XLOG,YLOG,PLOT10
      SAVE PLOT10,TERMTYPE,HDCPYTYPE,ITERM,IHDCPY,ASPECT,LINE,ISYMB,
     1     X1,X2,XMIN,XMAX,XLOG,XPERPIX,X0,
     2     Y1,Y2,YMIN,YMAX,YLOG,YPERPIX,Y0
      DATA PLOT10, TERMTYPE,HDCPYTYPE,ITERM,IHDCPY,ASPECT,LINE,ISYMB
     1    /.FALSE.,'?',     '?',      1,    1,     0.75,  -1,  -1/
      DATA X1,X2,   XPERPIX,X0,Y1,Y2,  YPERPIX,Y0
     1    /0.,1023.,1.,     0.,0.,767.,1.,     0./
C
      ENTRY INITT(X)
C     Initialize graphics terminal device
      plot10 = .true.
      call pgbegin(0, termtype(1:iterm), 1, 1)
      if (termtype.eq.'?') then
         call pgqinf('dev/type',termtype,iterm)
         iterm = min0(iterm,index(termtype,'/')+3)
      end if
      return
C
      ENTRY HDCOPY
C     Initialize hardcopy output device
      call pgbegin(0, hdcpytype(1:ihdcpy), 1, 1)
      if (termtype.eq.'?') then
         call pgqinf('dev/type',hdcpytype,ihdcpy)
         ihdcpy = min0(ihdcpy,index(hdcpytype,'/')+3)
      end if
      return
C
      ENTRY ERASE
      ENTRY RESET
C     Erase the screen
      call pgpage
      ENTRY TERM(IX,IY)
      return
C
      ENTRY FINITT(X,Y)
C     Exit graphics
      call pgend
      return
C
C     Define graph area in world co-ordinates, to fill the available
C       viewport area.  Place two lines of titles at top, and axis labels
C       on bottom and left.  Draw box with tick marks and labels; logarithmic
C       if the corresponding LOGX or LOGY value is non-zero.  XMN, XMX, YMN,
C       and YMX will be changed to rounded values.
C
      ENTRY GRAPH(TITLE, SUBTITLE, XLABEL, YLABEL,
     1            XMN, XMX, LOGX,   YMN, YMX, LOGY)
      call pgetxt
      call pgqvp(3,x1,x2,y1,y2)
      aspect = (y2-y1+1.)/(x2-x1+1.)
      call pgvport(0.20*aspect, 1.0-0.05*aspect, 0.10, 0.85 )
C
      call pgsch(2.0)
      call pgslw(3)
      call check_text(title,newstring,j,plot10)
      call pgmtext('T', 2.0, 0.5, 0.5, newstring(1:j))
C
      call pgsch(1.5)
      call pgslw(2)
      call check_text(subtitle,newstring,j,plot10)
      call pgmtext('T', 1.2, 0.5, 0.5, newstring(1:j))
C
      call check_text(xlabel,newstring,j,plot10)
      call pgmtext('B', 2.16, 0.5, 0.5, newstring(1:j))
C
      call check_text(ylabel,newstring,j,plot10)
      call pgmtext('L', 4.0, 0.5, 0.5, newstring(1:j))
      call pgsch(1.0)
      call pgslw(1)
C
      xlog = logx.gt.0
      ylog = logy.gt.0
      if (xlog) then
         call logaxis(xmn, xmx, ixmajor, ixtick, xminor, xformat)
         xmin = alog10(xmn)
         xmax = alog10(xmx)
         xmajor = 1.
         ixtick = 9
         xoption = 'BCLNTS'
         if (.not.ylog) xoption = 'ABCLNTS'
      else
         call linaxis(xmn, xmx, ixmajor, ixtick, xformat)
         xmin = xmn
         xmax = xmx
         xmajor = (xmax-xmin)/float(ixmajor)
         xoption = 'BCNTS'
         if (.not.ylog) xoption = 'ABCNTS'
      end if
C
      if (ylog) then
         call logaxis(ymn, ymx, iymajor, iytick, yminor, yformat)
         ymin = alog10(ymn)
         ymax = alog10(ymx)
         ymajor = 1.
         iytick = 9
         yoption = 'BCLNTSV'
         if (.not.xlog) yoption = 'ABCLNTSV'
      else
         call linaxis(ymn, ymx, iymajor, iytick, yformat)
         ymin = ymn
         ymax = ymx
         ymajor = (ymax-ymin)/float(iymajor)
         yoption = 'BCNTSV'
         if (.not.xlog) yoption = 'ABCNTSV'
      end if
C
      call pgwindow(xmin, xmax, ymin, ymax)
      call pgbox(xoption, xmajor, ixtick, yoption, ymajor, iytick)
      xperpix = (xmax-xmin)/(x2-x1)/(1.-0.25*aspect)
      x0 = x1 + 0.20*aspect*(x2-x1)
      yperpix = (ymax-ymin)/(y2-y1)/0.75
      y0 = y1 + 0.10*(y2-y1)
C
      if (xlog) then
C        Make sure ends of X-axis are labeled
         if (amod(abs(xmin)+0.01,1.) .gt. 0.02) then
            i = nint(xmin-0.5)
            j = nint(xmn/10.**i)
            call pgnumb(j, i, 0, string, nc)
            call pgmtext('B', 1.25, 0., 0.5, string(1:nc))
         end if
         if (amod(abs(xmax)+0.01,1.) .gt. 0.02) then
            i = nint(xmax-0.5)
            j = nint(xmx/10.**i)
            call pgnumb(j, i, 0, string, nc)
            call pgmtext('B', 1.25, 1., 0.5, string(1:nc))
         end if
      end if
C
      if (ylog) then
C        Make sure ends of Y-axis are labeled
         if (amod(abs(ymin)+0.01,1.) .gt. 0.02) then
            i = nint(ymin-0.5)
            j = nint(ymn/10.**i)
            call pgnumb(j, i, 0, string, nc)
            call pgmtext('LV', aspect, 0., 1., string(1:nc))
         end if
         if (amod(abs(ymax)+0.01,1.) .gt. 0.02) then
            i = nint(ymax-0.5)
            j = nint(ymx/10.**i)
            call pgnumb(j, i, 0, string, nc)
            call pgmtext('LV', aspect, 1., 1., string(1:nc))
         end if
      end if
      return
C
C     Following entries either move to a new point without drawing, or
C       move to a new point and draw a single dot or a symbol, accounting 
C       for possibility of log scales.
C
      ENTRY MOVEA(X,Y)
      lpoint = .false.
      xx = x
      yy = y
      go to 100
C
      ENTRY MOVABS(IX,IY)
      lpoint = .false.
      xx = xmin + xperpix*(float(ix)-x0)
      yy = ymin + yperpix*(float(iy)-y0)
      go to 110
C
      ENTRY POINTA(X,Y)
      lpoint = .true.
      ll = -1
      xx = x
      yy = y
      go to 100
C
      ENTRY SYMBOL(X,Y,L)
      lpoint = .true.
      ll = l
      xx = x
      yy = y
      go to 100
C
      ENTRY ANCHO(L)
      lpoint = .true.
      ll = l
      call pgqpos(xx,yy)
      go to 110
C
      ENTRY PNTABS(IX,IY)
      lpoint = .true.
      ll = -1
      xx = xmin + xperpix*(float(ix)-x0)
      yy = ymin + yperpix*(float(iy)-y0)
      go to 110
C
      ENTRY MOVREL(IX,IY)
      lpoint = .false.
      go to 80
C
      ENTRY PNTREL(IX,IY)
      lpoint = .true.
      ll = -1
80    call pgqpos(xx,yy)
      xx = xx + xperpix*float(ix)
      yy = yy + yperpix*float(iy)
      go to 110
C
      ENTRY MOVER(X,Y)
      lpoint = .false.
      go to 90
C
      ENTRY POINTR(X,Y)
      lpoint = .true.
      ll = -1
90    continue
      call pgqpos(xx,yy)
      if (xlog) then
         xx = (10.**xx) + x
      else
         xx = xx + x
      end if
      if (ylog) then
         yy = (10.**yy) + y
      else
         yy = yy + y
      end if
100   continue
      if (xlog) then
         if (xx.le.0.) then
            xx = -38.
         else
            xx = alog10(xx)
         end if
      end if
      if (ylog) then
         if (yy.le.0.) then
            yy = -38.
         else
            yy = alog10(yy)
         end if
      end if
110   if (lpoint) then
         call pgpoint(1, xx, yy, ll)
      else
         call pgmove(xx,yy)
      end if
      return
C
C     Following entries draw a solid or dashed line from the current
C       location to a new point, accounting for possibility of log scales.
C
      ENTRY DRAWA(X,Y)
      ll = 0
      xx = x
      yy = y
      go to 200
C
      ENTRY DRWABS(IX,IY)
      ll = 0
      xx = xmin + xperpix*(float(ix)-x0)
      yy = ymin + yperpix*(float(iy)-y0)
      go to 210
C
      ENTRY DASHA(X,Y,L)
      ll = l
      xx = x
      yy = y
      go to 200
C
      ENTRY DSHABS(IX,IY,L)
      ll = l
      xx = xmin + xperpix*(float(ix)-x0)
      yy = ymin + yperpix*(float(iy)-y0)
      go to 210
C
      ENTRY DRWREL(IX,IY)
      ll = 0
      go to 180
C
      ENTRY DSHREL(IX,IY,L)
      ll = l
180   call pgqpos(xx,yy)
      xx = xx + xperpix*float(ix)
      yy = yy + yperpix*float(iy)
      go to 210
C
      ENTRY DRAWR(X,Y)
      ll = 0
      go to 190
C
      ENTRY DASHR(X,Y,L)
      ll = l
190   continue
      call pgqpos(xx,yy)
      if (xlog) then
         xx = (10.**xx) + x
      else
         xx = xx + x
      end if
      if (ylog) then
         yy = (10.**yy) + y
      else
         yy = yy + y
      end if
200   continue
      if (xlog) then
         if (xx.le.0.) then
            xx = -38.
         else
            xx = alog10(xx)
         end if
      end if
      if (ylog) then
         if (yy.le.0.) then
            yy = -38.
         else
            yy = alog10(yy)
         end if
      end if
210   if (ll.lt.0) then
         call pgmove(xx,yy)
      else
         if (ll.ne.line) then
            line = mod(ll,5)
            call pgsls(line+1)
         end if
         call pgdraw(xx,yy)
      end if
      return
C
C     Set boundaries of viewport or of world co-ordinate system
C
      ENTRY VWINDO(XMN, X, YMN, Y)
      xmax = xmn+x
      ymax = ymn+y
      go to 250
C
      ENTRY DWINDO(XMN, XMX, YMN, YMX)
      xmax = xmx
      ymax = ymx
250   xmin = xmn
      ymin = ymn
      call pgwindow(xmin, xmax, ymin, ymax)
      xperpix = (xmax-xmin)/(x2-x1+1.)
      x0 = x1
      yperpix = (ymax-ymin)/(y2-y1+1.)
      y0 = y1
      return
C
      ENTRY SWINDOW(IX, LX, IY, LY)
      ix2 = x+lx
      iy2 = y+ly
      go to 260
C
      ENTRY TWINDO(IX, LX, IY, LY)
      ix2 = lx
      iy2 = ly
260   call pgqvp(3, x1, x2, y1, y2)
      call pgvport(float(ix)/(x2-x1), float(ix2)/(x2-x1),
     1             float(iy)/(y2-y1), float(iy2)/(y2-y1))
      call pgqvp(3, x1, x2, y1, y2)
      xperpix = (xmax-xmin)/(x2-x1+1.)
      x0 = x1
      yperpix = (ymax-ymin)/(y2-y1+1.)
      y0 = y1
      return
C
C     Write horizontal or vertical text strings 
C
      ENTRY HLABEL(TITLE, IX, IY, LX, LY)
      string = 'B'
      xx = xperpix*(float(ix)-x0)/(xmax-xmin)
      ly = nint((y2-y1)/40.)
      lx = (3*ly)/4
      yy = -(float(iy)-y0)/((y2-y1)/40.)
      go to 300
C
      ENTRY OSTRING(TITLE,L)
      call pgqpos(xx,yy)
      if (l.eq.0) then
         string = 'B'
         xx = (xx-xmin)/(xmax-xmin)
         yy = (yy-ymin)/yperpix/(y2-y1)*40.
      else
         string = 'L'
         xx = (yy-ymin)/(ymax-ymin)
         yy = (xx-xmin)/yperpix/(y2-y1)*40.
      end if
      go to 300
C
      ENTRY VLABEL(TITLE, IX, IY, LX, LY)
      string = 'L'
      xx = yperpix*(float(iy)-y0)/(ymax-ymin)
      ly = nint((y2-y1)/40.)
      lx = (3*ly)/4
      yy = -(float(iy)-y0)/((y2-y1)/40.)
300   continue
      call check_text(title,newstring,j,plot10)
      call pgmtext(string, yy, xx, 0.5, newstring(1:j))
      return
C
C     Find ("see") various window parameters
C
      ENTRY SEETW(IX, LX, IY, LY)
      ix = x1
      lx = x2
      iy = y1
      ly = y2
      return
C
      ENTRY SEEDW(XMN,XMX,YMN,YMX)
      if (xlog) then
         xmn = 10.**xmin
         xmx = 10.**xmax
      else
         xmn = xmin
         xmx = xmax
      end if
      if (ylog) then
         ymn = 10.**ymin
         ymx = 10.**ymax
      else
         ymn = ymin
         ymx = ymax
      end if
      return
C
      ENTRY SEELOC(IX,IY)
      call pgqpos(xx,yy)
      ix = x0+(xx-xmin)/xperpix
      iy = y0+(yy-ymin)/yperpix
      return
C
      ENTRY SETCOLOR(L)
      call pgsci(l)
      return
C
      ENTRY SCURSR(L, IX, IY)
      xx = xmin+xperpix*(float(ix)-x0)
      yy = ymin+yperpix*(float(iy)-y0)
      call pgcurse(xx, yy, ch)
      l = ichar(ch)
      if (l.gt.0) then
         ix = x0+(xx-xmin)/xperpix
         iy = y0+(yy-ymin)/yperpix
      end if
      return
C
      ENTRY VCURSR(L, X, Y)
      if (xlog) then
         if (x.le.0.) then
            xx = xmin
         else
            xx = alog10(x)
         end if
      else
         xx = x
      end if
      if (ylog) then
         if (y.le.0.) then
            yy = ymin
         else
            yy = alog10(y)
         end if
      else
         yy = y
      end if
      call pgcurse(xx,yy,ch)
      l = ichar(ch)
      if (l.ne.0) then
         if (xlog) then
            x = 10.**xx
         else
            x = xx
         end if
         if (ylog) then
            y = 10.**yy
         else
            y = yy
         end if
      end if
      return
C
      END   
C
      SUBROUTINE CHECK_TEXT(IN,OUT,KOUT,PLOT10_FLAG)
C
C     Decode special characters in PLOT10 string to PGPLOT
C
      IMPLICIT NONE
      CHARACTER IN*(*),OUT*(*)
      INTEGER KOUT
      LOGICAL PLOT10_FLAG
C
      CHARACTER SPECIAL(5)*1,CH2*2,SYMBOL(8)*6
      INTEGER I,J,K,IIN,JIN,IOUT,MODE,KMAX
      DATA SPECIAL/'<', '>', '?', '#', '&'/
      DATA SYMBOL/'\(845)', '\(847)', '\(840)', '\(846)', '\(841)',
     1            '\(842)', '\(843)', '\(852)'/
C
C     Omit trailing blanks and nulls
      do 2 jin=len(in),3,-1
         if (in(jin:jin).ne.' ' .and. in(jin:jin).ne.char(0)) go to 3
2     continue
C     Omit terminal '$'
3     if (in(jin:jin).eq.'$') jin = jin-1
C
      kmax = len(out)
      j = 0
      k = 0
      mode = 2
10    continue
         j = j+1
         if (plot10_flag) then
C           Look for PLOT10 special characters
            do 80 i=1,5
               if (in(j:j).eq.special(i)) then
                  if ((i.eq.1.or.i.eq.2).and.(i.ne.mode)) then
                     k = k+3
                     out(k-2:k) = '\f1'
                  else if (i.eq.5) then
C                    Convert next 2 characters to lower case for testing
                     ch2(1:1) = char(ior(ichar(in(j+1:j+1)),32))
                     ch2(2:2) = char(ior(ichar(in(j+2:j+2)),32))
                     if (ch2.eq.'ex') then
C                       End of superscript
                        j = j+2
                        k = k+2
                        out(k-1:k) = '\d'
                     else if (ch2(1:1).eq.'e') then
C                       Beginning of superscript
                        j = j+1
                        k = k+2
                        out(k-1:k) = '\u'
                     else if (ch2.eq.'lx') then
C                       End of subscript
                        j = j+2
                        k = k+2
                        out(k-1:k) = '\u'
                     else if (ch2(1:1).eq.'l') then
C                       Beginning of subscript
                        j = j+1
                        k = k+2
                        out(k-1:k) = '\d'
                     end if
                  end if
                  mode = i
                  go to 100
               end if
80          continue
C           Not a special case, just keep the character
            if (mode.eq.3 .or. mode.eq.4) then
C              Character is Greek, must be preceded with flag
               k = k+2
               out(k-1:k) = '\g'
            end if
            k = k+1
            if (mode.eq.1 .or. mode.eq.4) then
C              Character must be lower case
               out(k:k) = char(ior(ichar(in(j:j)),32))
            else
C              No case modification
               out(k:k) = in(j:j)
            end if
         else
C
C           Look for PGPLOT symbol numbers
            if (in(j:j).eq.'\') then
               i = ichar(in(j+1:j+1)) - ichar('0')
               if (i.ge.1 .and. i.le.8) then
                  j = j+1
                  k = k+6
                  out(k-5:k) = symbol(i)
                  go to 100
               end if
            end if
C           Not symbol number, copy unmodified character to output
            k = k+1
            out(k:k) = in(j:j)
         end if
C
100   if (j.lt.jin .and. k.lt.kmax) go to 10
      kout = min0(k,kmax)
      return
      END
C
      SUBROUTINE LINAXIS(XONE,XTWO,MAJOR,MINOR,FORMAT)
C
C  SCALE ENDS OF LINEAR AXIS TO ROUNDED NUMBERS
C
C    An axis is defined to include XONE and XTWO, with each end rounded to   
C  be an integer number of units of the form (1, 2, or 5) x 10**n.  The
C  resulting number MAJOR of major divisions will be between 4 and 10, with
C  MINOR minor divisions in each step.  A character string (FORMAT = 'Fww.dd')
C  is generated to use as the format for labeling the axis.
C
C  P. A. Seeger, Los Alamos National Laboratory, May 24, 1986
C    Modified to prevent log of X=0., Aug. 31, 1986
C    Use E instead of F format if width > 10, Nov. 21, 1987
C    Guard against axis ends > 10**38, Feb. 5, 1990
C
C    No Externals
C
      IMPLICIT NONE
      REAL*4 XONE,XTWO,XMIN,XMAX,AXLEN,UNIT
      INTEGER MAJOR,MINOR,IPOWR,NN,IW,ID
      CHARACTER*6 FORMAT
C
      XMIN = AMAX1(-0.7E38,AMIN1(XONE,XTWO))
      XMAX = AMIN1( 0.7E38,AMAX1(XONE,XTWO))
      IF (XMAX.EQ.XMIN) THEN
         IF (XMAX.LT.0.) THEN
            XMAX = 0.
         ELSE IF (XMIN.GT.0.) THEN
            XMIN = 0.
         ELSE 
            XMAX = 0.001
         END IF
      END IF
      AXLEN = XMAX-XMIN
      IPOWR = NINT(ALOG10(AXLEN)-1.05)
      UNIT = 10.**IPOWR
      NN = AXLEN/UNIT
      IF (NN.GE.15) THEN
         UNIT=UNIT*5.
         MINOR=1
         IF (NN.LE.30) MINOR=5
      ELSE IF (NN.GE.7) THEN
         UNIT=UNIT*2.
         MINOR=2
         IF (NN.LE.10) MINOR=4
      ELSE
         MINOR=2
         IF (NN.LE.4) MINOR=5
      END IF
C
      IF (XMIN.GE.0.) THEN
         XMIN = UNIT*AINT((XMIN+0.01*AXLEN)/UNIT)
      ELSE
         XMIN = UNIT*AINT((XMIN+0.01*AXLEN)/UNIT-1.)
      END IF
      IF (XMAX.GE.0.) THEN
         XMAX = UNIT*AINT((XMAX-0.01*AXLEN)/UNIT+1.)
      ELSE
         XMAX = UNIT*AINT((XMAX-0.01*AXLEN)/UNIT)
      END IF
C
      MAJOR = NINT((XMAX-XMIN)/UNIT)
      IF (XTWO.GT.XONE) THEN
         XONE = XMIN
         XTWO = XMAX
      ELSE
         XONE = XMAX
         XTWO = XMIN
      END IF
C
      ID = MAX0(0,-IPOWR)
      IF (XMIN.LT.0.) XMIN=-10.*XMIN
      IF (XMAX.LT.0.) XMAX=-10.*XMAX
      IPOWR = ALOG10(AMAX1(XMIN,XMAX,1.))+0.001
      IW = IPOWR+ID+2
      IF (IW.LT.10) THEN
         WRITE (FORMAT,100) IW,ID
100      FORMAT ('F',I2,'.',I2)
      ELSE
         FORMAT = 'E 9. 2'
      END IF
C
      RETURN
      END
C
      SUBROUTINE LOGAXIS(XONE,XTWO,MAJOR,MINOR,UNIT,FORMAT)
C
C  SCALE ENDS OF LOGARITHMIC AXIS TO ROUNDED NUMBERS
C    
C    An axis is defined to include XONE and XTWO, with each end rounded to   
C  be (1, 2, 3, or 5) x some power of 10.  The resulting axis spans MAJOR
C  decades, including partial decades at one or both ends.  The lowest decade
C  on the axis has MINOR units of size UNIT; if it is a full decade, MINOR = 9
C  and UNIT = min(X).  A character string (FORMAT = 'Fww.dd') is generated to
C  use as the format for labeling the axis.
C
C  P. A. Seeger, Los Alamos National Laboratory, May 20, 1986
C      Modified to avoid log(X) when X.LE.0., June 17, 1986
C      Corrected FORMAT when XMAX<1., June 19, 1987
C      Use E instead of F format when width > 10, Nov. 21, 1987
C      Guard against axis ends > 10**38, Feb. 5, 1990
C
C    No Externals
C
      IMPLICIT NONE
      REAL*4 XONE,XTWO,UNIT,XMIN,XMAX,XLOG,LOG2,LOG3,LOG5,DELTA
      INTEGER MAJOR,MINOR,IMIN,IMAX,IW,ID
      CHARACTER*6 FORMAT
      PARAMETER (LOG2=0.3010300,LOG3=0.4771213,LOG5=0.6989700)
C
      XMIN = AMIN1(XONE,XTWO)
      XMAX = AMAX1(XONE,XTWO)
      IF (XMAX.LE.0.) XMAX = 1.
      IF (XMIN.LE.0.) THEN
         XMIN = XMAX/1000.
      ELSE IF (XMAX.GE.1.E38) THEN
         XMAX = XMIN*1.E6
      ELSE
         XMIN = AMAX1(XMIN,XMAX/1.E24)
      END IF
      IF (XMIN.EQ.XMAX) THEN
         XMAX = XMAX*2.
         XMIN = XMIN/2.
      END IF
      DELTA = 0.01*ALOG10(XMAX/XMIN)
C
C     Look at small end of axis first
      XLOG = ALOG10(XMIN)+DELTA
      IMIN = NINT(XLOG-0.5)
      XMIN = 10.**IMIN
      UNIT = XMIN
      XLOG = XLOG-FLOAT(IMIN)
      IF (XLOG.GT.LOG5) THEN
         XMIN = 5.*XMIN
         MINOR = 5
      ELSE IF (XLOG.GT.LOG3) THEN
         XMIN = 3.*XMIN
         MINOR = 7
      ELSE IF (XLOG.GT.LOG2) THEN
         XMIN = 2.*XMIN
         MINOR = 8
      ELSE
         MINOR = 9
      END IF
      ID = MAX0(0,-IMIN)
C
C     Now do "same" thing at larger end
      XLOG = ALOG10(XMAX)-DELTA
      IMAX = NINT(XLOG-0.5)
C     Compute total number of decades included
      MAJOR = IMAX-IMIN+1
      XMAX = 10.**IMAX
      XLOG = XLOG-FLOAT(IMAX)
      IF (XLOG.LT.LOG2) THEN
         XMAX = 2.*XMAX
      ELSE IF (XLOG.LT.LOG3) THEN
         XMAX = 3.*XMAX
      ELSE IF (XLOG.LT.LOG5) THEN
         XMAX = 5.*XMAX
      ELSE
         XMAX = 10.*XMAX
         IMAX = IMAX+1
      END IF
      IF (MAJOR.EQ.1) MINOR=NINT((XMAX-XMIN)/UNIT)
C
      IW = MAX0(0,IMAX)+ID+2
      IF (IW.LT.10) THEN
         WRITE (FORMAT,100) IW,ID
100      FORMAT ('F',I2,'.',I2)
      ELSE
         FORMAT = 'E 9. 2'
      END IF
      IF (XONE.LT.XTWO) THEN
         XONE = XMIN
         XTWO = XMAX
      ELSE
         XONE = XMAX
         XTWO = XMIN
      END IF
C
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE CONTOUR(Z,NRZ,X,NX,Y,NY,CV,NCV,LINE,ZMAX,BITMAP)
C
C  DRAW CONTOURS THROUGH EQUAL VALUES IN AN ARRAY
C
C  From Collected Algorithms from ACM #531 "Contour Plotting [J6]"
C   by: William V. Snyder, 1978
C  Copied from GSAS; transferred to IBM-PC, Dec. 8, 1987 (P.A.Seeger)
C  Added X, Y, and LINE to calling sequence, Dec. 9, 1987 (PAS) 
C  Restructured, closer to "standard", Dec. 11, 1987 (PAS)
C  Changed I and J to II and JJ in innermost loop, Dec. 13, 1987 (PAS)
C  Revised scan pattern from spiral to linear, Aug. 13, 1988 (PAS)
C
C  Externals
C    DASHA     FILL0     LGETMARK  MOVEA
C
C  Arguments in calling sequence:
C    Z         R(NRZ,*) Input    Array of values to be contoured; nodes must
C                                lie on a topologically rectangular grid.
C    NRZ       I        Input    Number of rows declared for array Z
C    X         R(*)     Input    Values of X at grid points of array Z
C    NX        I        Input    Limit for 1st subscript of Z and X
C    Y         R(*)     Input    Values of Y at grid points of array Z
C    NY        I        Input    Limit for 2nd subscript of Z and Y
C    CV        R(*)     Input    Values of contour levels
C    NCV       I        Input    No. of contour levels
C    LINE      I(*)     Input    Line style for each contour level
C    ZMAX      R        Input    Maximum Z to be considered; grid lines at
C                                a node with value above ZMAX are excluded
C    BITMAP    I(*)     Scratch  Work area of size (2*NX*NY*NCV+7)/8 bytes
C
      IMPLICIT NONE
      INTEGER NRZ,NX,NY,NCV,LINE(*)
      REAL*4 Z(NRZ,*),X(*),Y(*),CV(*),ZMAX
      INTEGER*4 BITMAP(*) 
C
      LOGICAL LGETMARK
C
C  Local variables in CONTOUR:
C    CVAL      R        Contour-line value being traced
C    DELZ      R        Change in Z when moving 1 cell right or up
C    IADD      I        Bit address in BITMAP, starting at zero
C    IBORDER   I        Edge of current cell which is on a border
C    ICV       I        Index of contour line being traced
C    IEDGE     I        Edge of new cell where contour line enters
C    IFLAG     I        1=continue, 2=start at boundary, 3=start in interior,
C                       4=end at boundary, 5=close contour, 6=none found yet
C    I,J       I        Subscripts for search
C    IJ        I(2)     Equivalent to I,J
C    IJMAX     I(2)     Local copies of NX and NY
C    I1(2),I2(2),I3(6)  Used for subscript computations
C    II,JJ     I        Cell with continuation of contour line being traced
C    INDEX     I        L-1 + 2*(I-1 + NX*(J-1))
C    K         I        Index of cell edges: 1=bottom, 2=left, 3=top, 4=right
C    KS        I        Next cell boundary to cross
C    L,LL      I        Orientation flags: 1 is horizontal line, 2 vertical
C    LBORDER   L        Flag to show that only border lines are to be done
C    NI        I        Number of edges of cell which contour crosses
C    XINT      R(4)     Intersections of contour with edges of cell,
C                          in order bottom, left, top, right
C    XX,YY     R        Plotting coordinates
C    Z1,Z2     R        Smaller and larger values at segment ends
C    Z3,Z4     R        Values at ends of segment for continuation of contour
C
      REAL*4 XINT(4),Z1,Z2,Z3,Z4,DELZ,CVAL,XX,YY
      INTEGER I1(2),I2(2),I3(6),I,J,IJ(2),IJMAX(2),K,L,LL,II,JJ,ICV,
     , IBORDER,IEDGE,IFLAG,NI,KS
      INTEGER*4 INDEX,MAXINDX,NBITS,IADD
      LOGICAL LBORDER
      EQUIVALENCE (IJ(1),I),(IJ(2),J)
      DATA I1/1,0/,  I2/1,-1/,  I3/1,0,0,1,1,0/
C
      IJMAX(1) = NX
      IJMAX(2) = NY
C  Clear bit map
      NBITS = 2*NX*NY*NCV
      CALL FILL0(BITMAP,NBITS)
      IFLAG = 6
C
C  Search every cell in rectangular array for a line segment such that:
C    1. the end points are not excluded because Z > ZMAX
C    2. current contour <= Z at one end and > Z at other end
C    3. no mark has been recorded for this contour on this segment
C
C  Set start at lower left corner of array; do borders first, then interior
C
      LBORDER = .TRUE.
      MAXINDX = 2*NX*NY-2
100   CONTINUE
      DO 500 INDEX=0,MAXINDX
         L = MOD(INDEX,2)+1
         I = MOD(INDEX/2,NX)+1
         J = (INDEX/2)/NX+1
         IF (Z(I,J).LE.ZMAX) THEN
C           Node itself is within non-excluded range
            II = I+I1(L)
            JJ = J+I1(3-L)
            IF (II.LE.NX .AND. JJ.LE.NY .AND. Z(II,JJ).LE.ZMAX) THEN
C              Both ends of grid line within range; test if "border" segment
               IBORDER = 0
               IF (IJ(3-L).EQ.1 .OR.
     1             Z(I-I1(3-L),J-I1(L)).GT.ZMAX .OR.
     2             Z(I+I2(L),J+I2(3-L)).GT.ZMAX)   IBORDER = 1
               IF (IJ(3-L).GE.IJMAX(3-L) .OR.
     1             Z(I+I1(3-L),J+I1(L)).GT.ZMAX .OR.
     2             Z(I+1,J+1).GT.ZMAX)     IBORDER = IBORDER+2 
C              1st time, do ONLY borders (including edges of exclusions)
               IF (IBORDER.NE.3 .AND. (LBORDER.EQV.(IBORDER.NE.0))) THEN
C                 Examine this line segment this pass
                  Z1 = AMIN1(Z(I,J),Z(II,JJ))
                  Z2 = AMAX1(Z(I,J),Z(II,JJ))
                  DELZ = Z(II,JJ)-Z(I,J)
                  DO 400 ICV=1,NCV
C                    Test for all possible contours crossing this grid line;
C                     first check if already done, and set bit to show done
                     IADD = ICV-1 + NCV*INDEX
                     IF (.NOT.LGETMARK(BITMAP,IADD) .AND. 
     1                   CV(ICV).GT.Z1 .AND. CV(ICV).LE.Z2) THEN
C                       Found one we haven't done yet!!!  Interpolate.
                        CVAL = CV(ICV)
C                       Decide which edge we are entering cell from
                        IEDGE = L
                        IF (IBORDER.EQ.2) IEDGE = IEDGE+2
                        XINT(IEDGE) = (CVAL-Z(I,J))/DELZ
C                       Move "pen" to starting point of contour
                        XX = X(I)+XINT(IEDGE)*(X(II)-X(I))
                        YY = Y(J)+XINT(IEDGE)*(Y(JJ)-Y(J))
                        IFLAG = 3
                        IF (LBORDER) IFLAG = 2
                        CALL MOVEA(XX,YY)
C
C  Follow this contour until it hits boundary or closes on itself
                        II = I
                        JJ = J
                        IFLAG = 1
C                       "DO WHILE (IFLAG.LT.4)"
200                     CONTINUE
C                          If haven't moved to next cell yet, do so
                           IF (IEDGE.EQ.3) JJ = JJ-1
                           IF (IEDGE.EQ.4) II = II-1
                           NI = 1
                           DO 300 K = 1,4
C                             Test interpolation on other 3 edges
                              IF (K.NE.IEDGE) THEN
                                 Z3 = Z(II+I3(K),JJ+I3(K+1))
                                 Z4 = Z(II+I3(K+1),JJ+I3(K+2))
                                 IF (CVAL.GT.AMIN1(Z3,Z4) .AND. 
     .                               CVAL.LE.AMAX1(Z3,Z4)) THEN
C                                   The contour also crosses this edge
                                    IF (K.EQ.1 .OR. K.EQ.4) THEN
                                       XINT(K) = (CVAL-Z4)/(Z3-Z4)
                                    ELSE
                                       XINT(K) = (CVAL-Z3)/(Z4-Z3)
                                    END IF
C                                   Count how many crossings
                                    NI = NI+1
                                    KS = K
                                 END IF
                              END IF
300                        CONTINUE
C
                           IF (NI.NE.2) THEN
C  The contour crosses all four edges of the cell being examined. Choose the 
C  lines top-to-left and bottom-to-right if the interpolation point on the top 
C  edge is less than the interpolation point on the bottom edge. Otherwise, 
C  choose the other pair. This method produces the same result if the axes are 
C  reversed. The contour may close at any edge, but must not cross itself 
C  inside any cell.
                              IF (XINT(3).GE.XINT(1)) THEN
                                 KS = 3-IEDGE
                                 IF (KS.LE.0) KS = KS+4
                              ELSE
                                 KS = 5-IEDGE
                              END IF
                           END IF
C
C  Determine if the contour will close or run into a boundary at edge KS of the 
C  current cell.
                           IF (KS.LE.2) THEN
                              LL = KS
                              IEDGE = KS+2
                           ELSE
C                             Must move to adjacent cell before test for closure
                              II = II+I3(KS)
                              JJ = JJ+I3(KS+2)
                              LL = KS-2
                              IEDGE = KS-2
                           END IF
                           IADD = ICV-1+NCV*(LL-1+2*(II-1+NX*(JJ-1)))
                           IF (LGETMARK(BITMAP,IADD)) THEN
C                             We've already been here; contour has closed 
                              IFLAG = 5
                           ELSE IF (LL.EQ.2.AND.(II.EQ.1.OR.II.GE.NX)
     1                     .OR. LL.EQ.1.AND.(JJ.EQ.1.OR.JJ.GE.NY)) THEN
C                             Segment is actual boundary of plot
                              IFLAG = 4
                           ELSE IF (Z(II-I1(3-LL),JJ-I1(LL)).GT.ZMAX 
     1                     .OR. Z(II+I2(LL),JJ+I2(3-LL)).GT.ZMAX
     2                     .OR. Z(II+I1(3-LL),JJ+I1(LL)).GT.ZMAX
     3                     .OR. Z(II+1,JJ+1).GT.ZMAX) THEN
C                             Segment is boundary of an excluded cell
                              IFLAG = 4
                           END IF
C
C                          Draw piece of contour
                           XINT(IEDGE) = XINT(KS)
                           IF (LL.EQ.1) THEN
                              XX = X(II)+XINT(IEDGE)*(X(II+1)-X(II))
                              YY = Y(JJ)
                           ELSE
                              XX = X(II)
                              YY = Y(JJ)+XINT(IEDGE)*(Y(JJ+1)-Y(JJ))
                           END IF
                           CALL DASHA(XX,YY,LINE(ICV))
C
                        IF (IFLAG.LT.4) GO TO 200
C                       Reset II and JJ before looking for next contour
                        II = I+I1(L)
                        JJ = J+I1(3-L)
                     END IF
400               CONTINUE
               END IF
            END IF
         END IF
500   CONTINUE
C
      IF (.NOT.LBORDER) RETURN
      LBORDER = .FALSE.
      GO TO 100
      END
C  
C  CONTOUR SUBROUTINES FOR BIT MANIPULATION
C
      SUBROUTINE FILL0(BITMAP,N)
C  Fills entire BITMAP with zeros
      IMPLICIT NONE
      INTEGER N,I,LOOP
      INTEGER*4 BITMAP(*)
C
      LOOP = (N-1)/32+1
      DO 10 I=1,LOOP
         BITMAP(I) = 0
10    CONTINUE
      RETURN
      END
C
      LOGICAL FUNCTION LGETMARK(BITMAP,N)
C  Tests bit in BITMAP, and then sets it to one
      IMPLICIT NONE
      INTEGER N,NWORD,NBIT
      INTEGER*4 BITMAP(*)
      LOGICAL BTEST
C                                        
      NWORD = N/32+1
      NBIT = MOD(N,32)
      LGETMARK = BTEST(BITMAP(NWORD),NBIT)
      IF (.NOT.LGETMARK) BITMAP(NWORD) = IBSET(BITMAP(NWORD),NBIT)
      RETURN
      END
C
C******************************************************************************
C
      SUBROUTINE CURVE(X,Y,N,LDASH,ISYM)
C
C  PLOT N POINTS AT (X(I),Y(I)), USING SYMBOL CORRESPONDING TO ISYM, AND
C    CONNECTING WITH LINE DESCRIBED BY LDASH.
C
C  P. A. Seeger, Los Alamos National Laboratory, May 24, 1986
C
      INTEGER N,LDASH,ISYM,I
      REAL*4 X(*),Y(*)
C
C  Externals
C     DASHA     MOVEA     SYMBOL
C
      CALL MOVEA(X(1),Y(1))
      DO 100 I=1,N
         CALL DASHA(X(I),Y(I),LDASH)
         CALL SYMBOL(X(I),Y(I),ISYM)
100   CONTINUE
C
      RETURN
      END
C
C******************************************************************************
C
      SUBROUTINE ERRBARS(X,Y,DY,N,INC,LOGY)
C
C  PLOT ERROR BARS ON EVERY INCth POINT OF AN ARRAY OF N POINTS 
C    (X(I),Y(I)+-DY(I)).  LOGY=1 IF ORDINATE IS LOGARITHMIC.
C
C  P. A. Seeger, Los Alamos National Laboratory, Nov. 24, 1987
C
      IMPLICIT NONE
      INTEGER N,INC,LOGY,I
      REAL*4 X(*),Y(*),DY(*),D,R,Y1,Y2
C
C  Externals
C     DRAWA     MOVEA     
C
      DO 100 I=1,N,INC
         D = ABS(DY(I))
         IF (LOGY.EQ.1) THEN
            IF (Y(I).LE.0.) THEN
               Y1 = 2.E-38
               Y2 = 2.E-38
            ELSE IF (D.LT.0.05*Y(I)) THEN
               Y1 = Y(I)-D
               Y2 = Y(I)+D
            ELSE IF (D.LT.88.*Y(I)) THEN
               R = EXP(D/Y(I))
               Y1 = Y(I)/R
               Y2 = Y(I)*R
            ELSE
               Y1 = 2.E-38
               Y2 = 1.E+38
            END IF
         ELSE
            Y1 = Y(I)-D
            Y2 = Y(I)+D
         END IF
         CALL MOVEA(X(I),Y1)
         CALL DRAWA(X(I),Y2)
100   CONTINUE
C
      RETURN
      END
