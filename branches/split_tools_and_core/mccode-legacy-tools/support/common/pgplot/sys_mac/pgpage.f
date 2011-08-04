C*PGPAGE -- advance to new page
C%void cpgpage(void);
C+
      SUBROUTINE PGPAGE
C
C Advance plotter to a new page or panel, clearing the screen if
C necessary. If the "prompt state" is ON (see PGASK), confirmation is
C requested from the user before clearing the screen. If the view
C surface has been subdivided into panels with PGBEG or PGSUBP, then
C PGPAGE advances to the next panel, and if the current panel is the
C last on the page, PGPAGE clears the screen or starts a new sheet of
C paper.  PGPAGE does not change the PGPLOT window or the viewport
C (in normalized device coordinates); but note that if the size of the
C view-surface is changed externally (e.g., by a workstation window
C manager) the size of the viewport is chnaged in proportion.
C
C Arguments: none
C--
C  7-Feb-1983
C 23-Sep-1984 - correct bug: call GRTERM at end (if flush mode set).
C 31-Jan-1985 - make closer to Fortran-77.
C 19-Nov-1987 - explicitly clear the screen if device is interactive;
C               this restores the behavior obtained with older versions
C               of GRPCKG.
C  9-Feb-1988 - move prompting into routine GRPROM.
C 11-Apr-1989 - change name to PGPAGE.
C 10-Sep-1990 - add identification labelling.
C 11-Feb-1992 - check if device size has changed.
C  3-Sep-1992 - allow column ordering of panels.
C 17-Nov-1994 - move identification to drivers.
C 23-Nov-1994 - fix bug: character size not getting reset.
C 23-Jan-1995 - rescale viewport if size of view surface  has changed.
C-----------------------------------------------------------------------
      INCLUDE      'pgplot.inc'
      CHARACTER*16 STR
      LOGICAL      INTER, PGNOTO
      REAL DUM1, DUM2, XS, YS, XVP1, XVP2, YVP1, YVP2
C
      IF (PGNOTO('PGPAGE')) RETURN
C
      IF (PGROWS) THEN
        NXC = NXC + 1
        IF (NXC.GT.NX) THEN
          NXC = 1
          NYC = NYC + 1
          IF (NYC.GT.NY) NYC = 1
        END IF
      ELSE
        NYC = NYC + 1
        IF (NYC.GT.NY) THEN
          NYC = 1
          NXC = NXC + 1
          IF (NXC.GT.NX) NXC = 1
        END IF
      END IF
      IF (NXC.EQ.1 .AND. NYC.EQ.1) THEN
          IF (ADVSET.EQ.1 .AND. PROMPT) THEN
              CALL GRTERM
              CALL GRQCAP(STR)
              IF (STR(8:8).EQ.'V') CALL GRPROM
C              CALL GRPROM
          END IF
          CALL GRPAGE
          IF (.NOT.PGPFIX) THEN
C             -- Get current viewport in NDC.
              CALL PGQVP(0, XVP1, XVP2, YVP1, YVP2)
C             -- Reset view surface size if it has changed
              CALL GRSIZE(IDENT, XS,YS, DUM1,DUM2, XPERIN,YPERIN)
              XSZ = XS/NX
              YSZ = YS/NY
C             -- and character size
              CALL PGSCH(PGCHSZ)
C             -- and viewport
              CALL PGSVP(XVP1, XVP2, YVP1, YVP2)
          END IF
C
C If the device is interactive, call GRBPIC to clear the page.
C (If the device is not interactive, GRBPIC will be called
C automatically before the first output; omitting the call here
C ensures that a blank page is not output.)
C
          CALL GRQTYP(STR,INTER)
          IF (INTER) CALL GRBPIC
      END IF
      XOFF = XVP + (NXC-1)*XSZ
      YOFF = YVP + (NY-NYC)*YSZ
C
C Window the plot in the new viewport.
C
      CALL PGVW
      ADVSET = 1
      CALL GRTERM
      END
