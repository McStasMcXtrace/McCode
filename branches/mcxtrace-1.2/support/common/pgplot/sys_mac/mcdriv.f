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
      SUBROUTINE MCDRIV (OPCODE, RBUF, NBUF, CHR, LCHR)
      Implicit None
      INTEGER OPCODE, NBUF, LCHR
      REAL RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for Macintosh computers.
C
C-----------------------------------------------------------------------
C

!!SETC USINGINCLUDES = .FALSE.
      include    'globals.f'

c   Bounding rectangle for the window
      record /rect/ bounds, rectangle
      record /WindowPtr/ myWindow
      RECORD /Point/ MouseLoc            !Where it was clicked
      RECORD /KeyMap/ KeysTyped        !What was typed.
      RECORD /PolyHandle/ Polyhnd          !handle for a polygon region
      Record /PicHandle/ windowPic     !handle for picture
      string*255    title,text
      logical*2    visible,goAway, Update, Ignore
      integer*4 minus1,Lw,ndum,npolypts,countpoly,windhgt,I,J,ColArr(0:7)
      Integer*2 xpt,ypt
      parameter (minus1 = -1, visible = .true.,    goAway = .false., 
     +           Update = .True.)
      Character*120 MSG,ch*1,Picture*3
      Character*(*) MCTYPE
      Parameter (MCTYPE = 'MAC   (Macintosh Window)')
      Data ColArr/WhiteColor,BlackColor,RedColor,GreenColor,BlueColor,
     +     CyanColor,MagentaColor,YellowColor/

C  Variables to handle event record.

      RECORD /EventRecord/ theEvent
      LOGICAL*2 DONE
      LOGICAL*1 AN_EVENT
      INTEGER*2 EVENT_MASK

      Save bounds, Mywindow, polyhnd, lw, npolypts, countpoly,
     +      Windhgt,xpt, ypt, Ignore, QDG, ColArr, windowPic, Picture

      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,900,200,
     2     210,220,230,240,900,260,900,900,900), OPCODE
      GOTO 900
C
C--- OPCODE = 1, Return device name.-------------------------------------
C
   10 CHR = MCTYPE
      LCHR = LEN(MCTYPE)
C  Get the global values now since this is needed later on and opcode=1
C  is always called before the globals are needed.
      QDG = JQDGLOBALS()
      RETURN
C
C--- OPCODE = 2, Return physical min and max for plot device, and range
C               of color indices.---------------------------------------
C
c    Set up the bounding rectangle for the window that is the largest
c    that will fit on the screen.
C   Note that the bottom left hand corner is 0,0 in pgplot, while the
C   top left hand corner is 0,0 on a Mac.  Leave about 40 pixels for the title
C   bar of the window.  See OPCODE = 9 for how we translate the y coordinate.

   20 RBUF(1) = 0.0
      RBUF(2) = float(QDG^.screenbits.bounds.right)
      RBUF(3) = 0.0
      RBUF(4) = float(QDG^.screenbits.bounds.bottom - 40)
      RBUF(5) = 0.0
      RBUF(6) = 7.0
      NBUF = 6
      RETURN
C
C--- OPCODE = 3, Return device resolution. ------------------------------
C
   30 RBUF(1) = 72.0
      RBUF(2) = 72.0
      RBUF(3) = 1.0
      NBUF = 3
      RETURN
C
C--- OPCODE = 4, Return misc device info. -------------------------------
C    (This device is Interactive, Cursor Control, No Hardware dashed lines,
C     Arbitary Polygons Fills, Pen Thickness support but ends are not
C     rounded, Rectangle Fills, Pixel Primitives Support, No Extra Prompt
C     before closing window, No color query support, No hardware symbol
C     support yet.
C
   40 CONTINUE
      CHR = 'ICNATRPNNN'
      RETURN
C
C--- OPCODE = 5, Return default name. ------------------------------
C
   50 CHR = 'PGPLOT Mac Window'
      LCHR = LEN(MCTYPE)
      RETURN
C
C--- OPCODE = 6, Return default physical size of plot. ------------------
C
   60 RBUF(1) = 0.0
      RBUF(2) = float(QDG^.screenbits.bounds.right)
      RBUF(3) = 0.0
      RBUF(4) = float(QDG^.screenbits.bounds.bottom - 40)
      NBUF = 4
      RETURN
C
C--- OPCODE = 7, Return misc defaults. ----------------------------------
C
   70 RBUF(1) = 1
      NBUF = 1
      RETURN
C
C--- OPCODE = 8, Select plot. -------------------------------------------
C
   80 CONTINUE
      RETURN
C
C--- OPCODE = 9, Open workstation. --------------------------------------
C
   90 NBUF = 2
C   If the size of the window has not been set than set it to the 
C  default window size.
      If ((bounds.right .eq. 0) .or. (bounds.bottom .eq. 0)) Then
         bounds = QDG^.screenbits.bounds
         bounds.top = QDG^.screenbits.bounds.top + 40
         bounds.bottom = QDG^.screenbits.bounds.bottom
      End If
	  windhgt = bounds.bottom - bounds.top

C   Ignore RBUF(3) for now.  I'm not sure what it is used for.  
c    Call NewWindow with nil to create the window on the heap
      title = chr(:lchr)
      myWindow.P = NewWindow(nil,bounds,%ref(title),
     1        visible,int2(noGrowDocProc),minus1,
     2        goAway,nil)
      If (myWindow.P .EQ. 0) then
         RBUF(1) = 0.0
         RBUF(2) = 0.0
	  Else
	     Call Setport(myWindow.p)
         RBUF(1) = float(myWindow.P)
         RBUF(2) = 1.0
      End If
      RETURN
C
C--- OPCODE=10, Close workstation. --------------------------------------
C
  100 CONTINUE
      Call DisposeWindow(myWindow.P)
      RETURN
C
C--- OPCODE=11, Begin picture. ------------------------------------------
C
  110 CONTINUE
C     Erase previous screen
      Call EraseRect(myWindow.P^.portRect)
	  
C     Resize Window
      bounds.right = int2(nint(Rbuf(1)))
      bounds.bottom = int2(nint(rbuf(2))+40)
      windhgt = bounds.bottom - bounds.top
      Call SizeWindow(myWindow.P,bounds.right,bounds.bottom,Update)
      If (MyWindow.p .eq. NiL) Then
         WRITE (MSG,'(''Could not resize window: '',I10)') OPCODE
         CALL GRWARN(MSG)
         Ignore = .TRUE.
      End If

C   Set the origin of the window to the lower, left hand corner to correspond
C   with PGPLOT.  Note that the y-coordinate increases downward on a mac
C   while it increases upward in PGPLOT.  So after setting the origin to
C   the bottom left hand corner, y-coordinate on mac equals the negative of the
C   y-coordinate from PGPLOT.
      call ClipRect(myWindow.P^.portRect)
      Call SetOrigin(int2(0),int2(0))

      Call grgenv('MACPICTURE',Picture,ndum)
	  If (picture(1:2) .eq. 'ON') Then
C        Open picture to record quick draw calls.
C         Write(0,*) 'Opening Picture'
         rectangle.top = 0
   	     rectangle.bottom = windhgt
   	     rectangle.left = 0
	     rectangle.right = bounds.right
         windowPic =  OpenPicture(rectangle)
         call ClipRect(rectangle)
	     Call ShowPen()
	  End If
C  Initialize constants.
      Ignore = .False.
      npolypts = 0
      Countpoly = 0
      RETURN
C
C--- OPCODE=12, Draw line. ----------------------------------------------
C
  120 CONTINUE
      If (Ignore) Return
      Call MoveTo(int2(nint(Rbuf(1))),int2(windhgt-nint(Rbuf(2))))
      Call LineTo(int2(nint(Rbuf(3))),int2(windhgt-nint(Rbuf(4))))
      Return
C
C--- OPCODE=13, Draw dot. -----------------------------------------------
C
  130 CONTINUE
      If (Ignore) Return
      xpt = Nint(rbuf(1))
      ypt = windhgt-Nint(rbuf(2))
      ndum = Nint(float(LW)/2.)
      If (ndum .gt. 0) Then
         rectangle.left = int2(xpt-ndum)
         rectangle.right = int2(xpt + ndum)
         rectangle.top = int2(ypt-ndum)
         rectangle.bottom = int2(ypt+ndum)
         Call PaintOval(Rectangle)
      Else
         Call MoveTo(int2(xpt),int2(ypt))
         Call LineTo(int2(xpt),int2(ypt))
      End If
      Return
C
C--- OPCODE=14, End picture. --------------------------------------------
C
  140 CONTINUE
      If (Ignore) Return
	  If (Picture(1:2) .eq. 'ON') Then
         Call ClosePicture()
C	     Write(0,*)'Picture size = ',gethandlesize(windowpic)
	     Call HidePen()
	     Call Setport(mywindow.p)
	     Call SetWindowPic(myWindow.P,windowpic)
      End If
      call GetWTitle(myWindow.P,%ref(text))  ! save window title
      title = 'Type return to continue. Command . to quit program'
      Call SetWTitle(myWindow.p,title)       ! put up new instructions.
      EVENT_MASK = $FFFF      !ALL EVENTS
      DONE = .false.
      DO WHILE (DONE = .false.)
         AN_EVENT = GetNextEvent(EVENT_MASK,%REF(theEvent))
         IF (AN_EVENT) CALL EVENTHANDLER(theEvent,DONE,ch,MouseLoc,opcode)
      END DO
      Call SetWTitle(myWindow.p,text)        ! Restore original title
      If (Rbuf(1) .ne. 0.0)  Call EraseRect(myWindow.P^.portRect)
       Call KillPicture(Windowpic)
      RETURN
C
C--- OPCODE=15, Select color index. -------------------------------------
C
  150 CONTINUE
      Call ForeColor(ColArr(Nint(rbuf(1))))
      Return
C
C--- OPCODE=16, Flush Buffer. --------------------------------------------
C   Ignore
  160 Continue
      Return

C
C--- OPCODE=17, Read cursor. --------------------------------------------
C
  170 Continue
      If (Ignore) Return
      call GetWTitle(myWindow.P,%ref(text))  ! save window title
      title = 'Type any character or use mouse. Command . to quit program'
      Call SetWTitle(myWindow.p,title)       ! put up new instructions.
      EVENT_MASK = $FFFF      !ALL EVENTS
      DONE = .false.
      DO WHILE (DONE = .false.)
         AN_EVENT = GetNextEvent(EVENT_MASK,%REF(theEvent))
         IF (AN_EVENT) CALL EVENTHANDLER(theEvent,DONE,ch,MouseLoc,opcode)
      END DO
      chr(1:1) = ch
      Rbuf(1) = float(MouseLoc.h)
      Rbuf(2) = float(windhgt-MouseLoc.v)
      NBuf = 2
      Call SetWTitle(myWindow.p,text)        ! Restore original title
      Return
C
C--- OPCODE=18, Erase alpha screen. -------------------------------------
C    (Null operation: there is no alpha screen.)
C
  180 CONTINUE
      RETURN
C
C--- OPCODE=20, Polygon fill. -------------------------------------------
C
  200 CONTINUE
      If (Ignore) Then
         Return
      else If (Npolypts .ne. 0) Then
         Countpoly = Countpoly + 1
         If (CountPoly .eq. 1) Then
            xpt = int2(Nint(Rbuf(1)))
            ypt = Int2(Nint(Windhgt-Rbuf(2)))
            Call MoveTo(xpt,ypt)
         Else If (CountPoly .lt. Npolypts) Then
            Call LineTo(int2(Nint(Rbuf(1))),Int2(Nint(Windhgt-Rbuf(2))))
         Else
            Call LineTo(int2(Nint(Rbuf(1))),Int2(Nint(Windhgt-Rbuf(2))))
            Call LineTo(xpt,ypt)
            Call ClosePoly()
            Call PaintPoly(Polyhnd)
            Call KillPoly(PolyHnd)
            Npolypts = 0
            CountPoly = 0
         End If
      Else
         Npolypts = Nint(Rbuf(1))
         Polyhnd = OpenPoly()
      End If
      Return
C
C--- OPCODE=21, Set color representation. -------------------------------
C
  210 CONTINUE
      RETURN
C
C--- OPCODE=22, Set line width. -----------------------------------------
C
  220 CONTINUE
       LW = NINT(max(RBUF(1)/2.,1.))
       Call PenSize(Int2(Lw),int2(Lw))
      Return
C
C--- OPCODE=23, Escape. -------------------------------------------------
C
C  The text in char is drawn directly on the screen at the current location
C  and font.
  230 CONTINUE
      If (ignore) Return
      text = chr(:LCHR)
      Call DrawString(text)
      RETURN
C
C--- OPCODE=24, Rectangle fill. -------------------------------------------
C
  240 CONTINUE
      If (ignore) Return
      rectangle.left = int2(nint(rbuf(1)))
      Rectangle.right = int2(nint(rbuf(3)))
      Rectangle.top = int2(Windhgt-nint(rbuf(4)))
      Rectangle.bottom = int2(Windhgt-nint(rbuf(2)))
      Call PaintRect(Rectangle)
      Return
C
C--- OPCODE=26, Image.---------------------------------------------------
C
  260 CONTINUE
      If (ignore) Return
      xpt = Nint(Rbuf(1))
      ypt = Windhgt - Nint(Rbuf(2))
      Call MoveTo(Int2(xpt),Int2(ypt))
      Call ForeColor(Colarr(Nint(Rbuf(3))))
      Do I = 4, Nbuf
         If (Rbuf(I) .ne. Rbuf(I-1)) Then
            Call LineTo(int2(Nint(Rbuf(I-1))),int2(ypt))
            Call ForeColor(Colarr(Nint(Rbuf(I))))
            Call MoveTo(Int2(Nint(Rbuf(I))),int2(ypt))
         End If
      End Do
      Call LineTo(Int2(Nint(Rbuf(I))),int2(ypt))
      Return
C-----------------------------------------------------------------------
C Error: unimplemented function.
C
  900 WRITE (MSG,
     1  '(''Unimplemented function in MC device driver: '',I10)') OPCODE
      CALL GRWARN(MSG)
      NBUF = -1
      RETURN
      End

      SUBROUTINE EVENTHANDLER(theEvent,DONE,ch,MouseLoc,opcode)
c      This routines figures out what kind of event has occurred and 
c      calls the appropriate routine to take action in response to the event.
c      It returns DONE as true when it is finished.
      implicit none

!!SETC USINGINCLUDES = .FALSE.
      include    'globals.f'

      RECORD /EventRecord/ theEvent
      RECORD /WindowPtr/ P, OLDPORT, CLWIN
      RECORD /Rect/ LIMITRECT
      RECORD /Point/ MouseLoc            !Where it was clicked
      LOGICAL*2 DONE
      LOGICAL*1 CLOSEIT
      INTEGER*2 WindowPart
      CHARACTER*1 CH
      INTEGER*4 OUTPUTWINDOW,opcode
      EXTERNAL OUTPUTWINDOW      !Routine to identify the default window
      
      QDG = JQDGLOBALS()
      DONE = .false.
      Select Case (theEvent.what)
         Case (mouseDown)      
            WindowPart = FindWindow(theEvent.where,%REF(P))
            Select Case (WindowPart)
               Case (inMenuBar)     !MENUBAR
C                  CALL DO_MENU(MenuSelect(theEvent.where),DONE)
               Case (inSysWindow)   !in Sys window
                  Call SystemClick(theEvent,P)
                              
               Case (inContent)   !CONTENT Region
                  IF (P.P .NE. FrontWindow) Then
                     CALL SelectWindow(P.P)
                  Else 
                     If (Opcode .eq. 17) Then
                        Call GetMouse(%ref(MouseLoc))
                        If (JIAND(theEvent. modifiers,OptionKey) .eq. 2048) Then
                           If (JIAND(theEvent. modifiers,ShiftKey) .eq. 512) Then
                              ch = 'X'  ! option-shift click means send X
                           Else 
                              ch = 'D'  ! option click means send D
                           End If
                        Else 
                           ch = 'A'  ! click means send A
                        End If
                        Done = .True.
                     End If
                  End If
                        
               Case (inDrag)   !DRAG Region
                  IF (P.P .NE. FrontWindow) Then
                     CALL SelectWindow(P.P)
                  else
                     LIMITRECT = QDG^.screenBits.bounds
                     Call InsetRect(LimitRect,int2(4),int2(4))
                     CALL DragWindow(P,theEvent.where,LIMITRECT)
                  End if
                              
               Case (inGrow)   !SIZE Region
                        
               Case (inGoAway)  !close box
C                 IF (P.P .NE. OUTPUTWINDOW()) THEN
C                    CLOSEIT = TrackGoAway(P,theEvent.where)
C                    IF (CLOSEIT) CALL CLOSE_A_WINDOW(P)
C                 End if
                              
               Case Default ! No other window parts to deal with.
                              
            End Select      ! End of mouseDown Event.
                  
         Case (mouseUp)      !MOUSE UP This program does nothing in response to this event.
            
         Case ( keyDown)    !key PRESS
            CH = char(JIAND(theEvent.message,charCodeMask))
            If ((JIAND(theEvent. modifiers,cmdKey) .eq. 256) .and. (ch .eq. '.')) then
               Stop 'Program stopped by command-.'
            End If 
            if (Opcode .eq. 17) Then
               Call GetMouse(%ref(MouseLoc))
               Done = .True.
            Else If (ch .eq. char(13))  Then
               Done = .True.
            End If 
            
         Case (keyUp)    !This program does nothing in response to this event.
            
         Case (autoKey)   !This program does nothing in response to this event.
            
         Case (updateEvt)             
			CALL REDRAW_WIN(theEvent.message)	! Redraw window connect to start 5 and 6	

         Case (diskEvt)   !This program does nothing in response to this event.
            
         Case (activateEvt) 
C           CALL SetPort(theEvent.message)
C           CALL DrawGrowIcon(theEvent.message)
            
         Case Default
      End Select
      
      RETURN

      ENTRY REDRAW_WIN(CLWIN)
      CALL GetPort(%REF(OLDPORT))	!Remember current port
      CALL SetPort(CLWIN)
      CALL BeginUpdate(CLWIN)
      IF (CLWIN.P .EQ. OUTPUTWINDOW()) THEN
         CALL F_DRAWOUTPWINDOW
         CALL DrawControls(CLWIN)
      END IF
      CALL DrawGrowIcon(CLWIN)
      CALL EndUpdate(CLWIN)
      CALL SetPort(OLDPORT)
10    RETURN
      END
 
