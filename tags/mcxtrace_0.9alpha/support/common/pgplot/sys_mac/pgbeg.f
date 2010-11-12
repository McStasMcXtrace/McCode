C*PGBEG -- begin PGPLOT, open output device
C%int cpgbeg(int unit, char *file, int nxsub, int nysub);
C+
      INTEGER FUNCTION PGBEG (UNIT, FILE, NXSUB, NYSUB)
      INTEGER       UNIT
      CHARACTER*(*) FILE
      INTEGER       NXSUB, NYSUB
C
C Begin PGPLOT, open the plot file.  A call to PGBEG is
C required before any other calls to PGPLOT subroutines.  If a plot
C file is already open for PGPLOT output, it is closed before the new
C file is opened.
C
C Returns:
C  PGBEG         : a status return value. A value of 1 indicates
C                    successful completion, any other value indicates
C                    an error. In the event of error a message is
C                    written on the standard error unit.  
C                    To test the return value, call
C                    PGBEG as a function, eg IER=PGBEG(...); note
C                    that PGBEG must be declared INTEGER in the
C                    calling program.
C Arguments:
C  UNIT  (input)   : this argument is ignored by PGBEG (use zero).
C  FILE  (input)   : the "device specification" for the plot device.
C                    Device specifications are installation dependent,
C                    but usually have the form "device/type" or
C                    "file/type". If this argument is a
C                    question mark ('?'), PGBEG will prompt the user
C                    to supply a string. If the argument is a blank
C                    string (' '), PGBEG will use the value of
C                    environment variable PGPLOT_DEV.
C  NXSUB  (input)  : the number of subdivisions of the view surface in
C                    X (>0 or <0).
C  NYSUB  (input)  : the number of subdivisions of the view surface in
C                    Y (>0).
C                    PGPLOT puts NXSUB x NYSUB graphs on each plot
C                    page or screen; when the view surface is sub-
C                    divided in this way, PGPAGE moves to the next
C                    panel, not the  next physical page. If
C                    NXSUB > 0, PGPLOT uses the panels in row
C                    order; if <0, PGPLOT uses them in column order.
C--
C  1-Jan-1984 [TJP]
C  8-Aug-1985 [TJP] - add '?' prompting.
C 31-Dec-1985 [TJP] - fix '?' prompting in batch jobs.
C 11-Sep-1986 [TJP] - add PGLDEV call.
C  9-Feb-1988 [TJP] - replace VMS-specific code with GRGCOM.
C 13-Dec-1990 [TJP] - make error reading input non-fatal.
C 22-Jun-1992 [TJP] - background and foreground colors.
C  3-Sep-1992 [WD/TJP] - add PGPLOT_DEV environment variable and
C                     row/column ordering of panels.
C 13-Oct-1992 [TJP] - add arrow-head attributes.
C 21-Jan-1993 [TJP] - add default for '?' [TJP].
C 17-Mar-1994 [TJP] - initialize color index range [TJP].
C 15-Sep-1994 [TJP] - initialize transfer function [TJP].
C  6-Jun-1995 [TJP] - explicitly initialize PGOPEN [TJP].
C-----------------------------------------------------------------------
      INCLUDE       'pgplot.inc'
      INTEGER       DEFTYP,GRDTYP,GROPEN,L,LR,IC1
      INTEGER       GRGCOM, IER, LDEFDE
      REAL          DUMMY,DUMMY2,XCSZ
      CHARACTER*128 DEFDEV
      CHARACTER*20  DEFSTR
      CHARACTER*256 REQ
      LOGICAL JUNK
C
C  Move the initialization of pgopen to a block data subprogram in
C  pgblck file.  John S. Salmento 7/5/95
C

C
C Close the plot-file if it is already open.
C
      IF (PGOPEN.NE.0) CALL PGEND
C
C Get the default device/type (environment variable PGPLOT_DEV).
C
      CALL GRGENV('DEV', DEFDEV, LDEFDE)
      IF (LDEFDE.EQ.0) THEN
          DEFDEV = '/NULL'
          LDEFDE = 5
      END IF
C
C Open the plot file; default type is given by environment variable
C PGPLOT_TYPE.
C
      CALL GRGENV('TYPE', DEFSTR, L)
      IF (L.EQ.0) THEN
          DEFTYP = 0
      ELSE
          CALL GRTOUP(DEFSTR, DEFSTR)
          DEFTYP = GRDTYP(DEFSTR(1:L))
      END IF
      IF (FILE.EQ.' ') THEN
          PGBEG = GROPEN(DEFTYP,UNIT,DEFDEV(1:LDEFDE),IDENT)
      ELSE IF (FILE(1:1).EQ.'?') THEN
   10     IF (LDEFDE.EQ.0) THEN
              IER = GRGCOM(REQ,
     :           'Graphics device/type (? to see list): ',LR)
          ELSE
              IER = GRGCOM(REQ,
     :           'Graphics device/type (? to see list, default '//
     :           DEFDEV(1:LDEFDE)//'): ',LR)
          END IF
          IF (IER.NE.1) THEN
              CALL GRWARN('Error reading device specification')
              PGBEG = IER
              RETURN
          END IF
          IF (LR.LT.1 .OR. REQ.EQ.' ') THEN
              REQ = DEFDEV(1:LDEFDE)
          ELSE IF (REQ(1:1).EQ.'?') THEN
              CALL PGLDEV
              GOTO 10
          END IF
          PGBEG = GROPEN(DEFTYP,UNIT,REQ,IDENT)
          IF (PGBEG.NE.1) GOTO 10
      ELSE
          PGBEG = GROPEN(DEFTYP,UNIT,FILE,IDENT)
      END IF
C
C Failed to open plot file?
C
      IF (PGBEG.NE.1) RETURN
C
C Success: determine device characteristics.
C
      PGOPEN = 1
      ADVSET = 0
      PGPFIX = .FALSE.
      CALL GRSIZE(IDENT,XSZ,YSZ,DUMMY,DUMMY2,XPERIN,YPERIN)
      CALL GRCHSZ(IDENT,XCSZ,DUMMY,XSP,YSP)
      PGROWS = .TRUE.
      IF (NXSUB.LT.0) PGROWS = .FALSE.
      NX = MAX(ABS(NXSUB),1)
      NY = MAX(ABS(NYSUB),1)
      XSZ = XSZ/NX
      YSZ = YSZ/NY
      NXC = NX
      NYC = NY
      CALL GRQTYP(DEFSTR,JUNK)
C
C Set the prompt state to ON, so that terminal devices pause between
C pages; this can be changed with PGASK.
C
      CALL PGASK(.TRUE.)
C
C If environment variable PGPLOT_BUFFER is defined (any value),
C start buffering output.
C
      PGBLEV = 0
      CALL GRGENV('BUFFER', DEFSTR, L)
      IF (L.GT.0) CALL PGBBUF
C
C Set background and foreground colors if requested.
C
      CALL GRGENV('BACKGROUND', DEFSTR, L)
      IF (L.GT.0) CALL PGSCRN(0, DEFSTR(1:L), IER)
      CALL GRGENV('FOREGROUND', DEFSTR, L)
      IF (L.GT.0) CALL PGSCRN(1, DEFSTR(1:L), IER)
C
C Set default attributes.
C
      CALL PGSCI(1)
      CALL PGSLS(1)
      CALL PGSLW(1)
      CALL PGSCH(1.0)
      CALL PGSCF(1)
      CALL PGSFS(1)
      CALL PGSAH(1, 45.0, 0.3)
      CALL PGSTBG(-1)
      CALL PGSHS(45.0, 1.0, 0.0)
C
C Set the default range of color indices available for images (16 to
C device maximum, if device maximum >= 16; otherwise not possible).
C Select linear transfer function.
C
      CALL GRQCOL(IC1, PGMXCI)
      PGMNCI = 16
      IF (PGMXCI.LT.16) PGMXCI = 0
      PGITF = 0
C
C Set the default window (unit square).
C
      XBLC = 0.0
      XTRC = 1.0
      YBLC = 0.0
      YTRC = 1.0
C
C Set the default viewport.
C
      CALL PGVSTD
      END
	  
